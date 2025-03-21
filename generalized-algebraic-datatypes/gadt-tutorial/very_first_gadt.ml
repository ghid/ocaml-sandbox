[@@@warning "-27-32-34-37"]

(* Part 1 *)
(* My Very First GADT *)

type empty = Empty
type non_empty = NonEmpty

type _ int_list =
  | Nil : empty int_list
  | Cons : int * _ int_list -> non_empty int_list

let nil : empty int_list = Nil
let one_two : non_empty int_list = Cons (1, Cons (2, Nil))

(* Using the GADT: A Specialised Function *)

let hd : non_empty int_list -> int = fun (Cons (x, _)) -> x

let is_empty : type e. e int_list -> bool = function
  | Nil -> true
  | Cons _ -> false
;;

(* Using the GADT: A Generic Function *)

let rec iter : type e. (int -> unit) -> e int_list -> unit =
  fun f xs ->
  match xs with
  | Nil -> ()
  | Cons (x, xs) ->
    f x;
    iter f xs
;;

(* Using the GADT: Existential Constructor *)

(* FIXME: broken code: type error *)
(* let kvs = [ "a", Nil; "b", Cons (0, Nil); "c", Cons (1, Cons (4, Nil)) ] *)

type any_int_list = Any : _ int_list -> any_int_list

let kvs = [ "a", Any Nil; "b", Any (Cons (0, Nil)); "c", Any (Cons (1, Cons (4, Nil))) ]

let iter_any f xs =
  match xs with
  | Any Nil -> ()
  | Any (Cons _ as xs) ->
    iter f xs (* In this branch only, [xs] has type [non_empty int_list] *)
;;

(* Part 2 *)
(* Multiple Parameters *)

type ('a, _) elist' =
  | Nil : ('a, empty) elist'
  | Cons : 'a * ('a, _) elist' -> ('a, non_empty) elist'

type yes = Yes
type no = No

type _ elist'' =
  | Nil : < elt : 'a ; empty : yes > elist''
  | Cons : 'a * < elt : 'a ; empty : _ > elist'' -> < elt : 'a ; empty : no > elist''

(* Built-in Types as Property Types *)

type _ v =
  | Int64 : int64 -> int64 v
  | Bool : bool -> bool v

type _ expr =
  | Value : 'a v -> 'a expr
  | Equal : 'a expr * 'a expr -> bool expr
  | IfThenElse : bool expr * 'a expr * 'a expr -> 'a expr

(* "Functions" Over Types *)

type odd = Odd
type even = Even

type (_, 'a) l =
  | Nil : (even, 'a) l
  | ConsE : 'a * (odd, 'a) l -> (even, 'a) l
  | ConsO : 'a * (even, 'a) l -> (odd, 'a) l

let rec iter : type e. ('a -> unit) -> (e, 'a) l -> unit =
  fun f l ->
  match l with
  | Nil -> ()
  | ConsE (x, xs) ->
    f x;
    iter f xs
  | ConsO (x, xs) ->
    f x;
    iter f xs
;;

type ('previous, 'current) parity =
  | O : (even, odd) parity
  | E : (odd, even) parity

type (_, 'a) l' =
  | Nil : (even, 'a) l'
  | Cons : 'a * ('p, 'q) parity * ('p, 'a) l' -> ('q, 'a) l'

let rec iter' : type e. ('a -> unit) -> (e, 'a) l' -> unit =
  fun f l ->
  match l with
  | Nil -> ()
  | Cons (x, _, xs) ->
    f x;
    iter' f xs
;;

(* List Syntax *)

type _ elist =
  | [] : < elt : 'a ; empty : yes > elist
  | ( :: ) : 'a * < elt : 'a ; empty : _ > elist -> < elt : 'a ; empty : no > elist

let xs = [ 3; 4; 5 ]

let rec length : type e. < elt : 'a ; empty : e > elist -> int = function
  | [] -> 0
  | _ :: xs -> 1 + length xs
;;

type t =
  (* A type for s-expressions *)
  | [] : t
  | ( :: ) : t * t -> t
  | Atom : string -> t

let dune_file =
  [ Atom "library"
  ; [ Atom "libraries"; Atom "cmdliner"; Atom "bos"; Atom "astring" ]
  ; [ Atom "name"; Atom "queenslib" ]
  ]
;;

(* Accumulator of Types: Tuples *)

type _ hlist =
  | [] : unit hlist
  | ( :: ) : 'a * 'b hlist -> ('a * 'b) hlist

let xs : (int * (string * unit)) hlist = [ 3; "this" ]
let hd : ('a * _) hlist -> 'a = fun (x :: _) -> x

type _ hrlist =
  | [] : unit hrlist
  | ( :: ) : 'a ref * 'b hrlist -> ('a * 'b) hrlist

let rec set_all : type t. t hrlist -> t hlist -> unit =
  fun rs vs ->
  match rs, vs with
  | [], [] -> ()
  | r :: rs, v :: vs ->
    r := v;
    set_all rs vs
;;

(* Accumulator of Types: Arrows *)

(* validator returns [Some error_msg] if invalid, [None] if valid *)
type 'a validator = 'a -> string option

(* A list of validators,
   [raw] is the type of the function without validation,
   [validated] is the type of the function with validation *)
type ('raw, 'validated) validators =
  | [] : ('r, ('r, string) result) validators
  | ( :: ) : 'a validator * ('r, 'v) validators -> ('a -> 'r, 'a -> 'v) validators

(* This is to consume all arguments when a validation has failed *)
let rec traverse_and_fail : type b a. string -> (a, b) validators -> b =
  fun msg vs ->
  match vs with
  | [] -> Error msg
  | _ :: vs -> fun _ -> traverse_and_fail msg vs
;;

(* The main wrapper: [validate validators f] is a function similiar to [f]
   but it checks the validity of its arguments *)
let rec validate : type raw validated. (raw, validated) validators -> raw -> validated =
  fun vs f ->
  match vs with
  | [] -> Ok f
  | v :: vs ->
    fun x ->
      (match v x with
       | None -> validate vs (f x)
       | Some msg -> traverse_and_fail msg vs)
;;

(* For example, [repeat] is a function for printing a string multiple times,
   but it fails on negative times and on empty strings *)
let repeat =
  validate
    [ (fun x -> if x < 0 then Some "negative" else None)
    ; (fun s -> if s = "" then Some "empty" else None)
    ]
    (fun x s ->
       for _ = 1 to x do
         print_endline s
       done)
;;

(* Nested GADTs *)

type s_static = SStatic : int -> s_static
type s_dynamic = SDynamic : s_dynamic

type _ s_knowable =
  | KnowableStatic : s_static s_knowable
  | KnowableDynamic : s_dynamic s_knowable

type s_unknowable = SUnknowable : s_unknowable

type _ sized =
  | Static : s_static s_knowable sized
  | Dynamic : s_dynamic s_knowable sized
  | Unknowable : s_unknowable sized

(* Define some aliases for convenience *)
type static = s_static s_knowable sized
type dynamic = s_dynamic s_knowable sized
type 'a knowable = 'a s_knowable sized
type unknowable = s_unknowable sized

(* Define codec type *)
type ('a, 's) codec =
  | Uint8 : (int, static) codec
  | String : (string, unknowable) codec
  | SizeHeader : (int, _ knowable) codec * ('a, unknowable) codec -> ('a, dynamic) codec
  | List : ('a, _ knowable) codec -> ('a list, unknowable) codec
(* etc. *)

module type Data_encoding = sig
  val read : ('a, _ knowable) codec -> char Seq.t -> 'a
  val read_unknowable : int -> ('a, unknowable) codec -> char Seq.t (* NOTE: 'a? *)
end

(* A Type Variable cannot be Deduced *)

(* FIXME: In the GADT constructor
     Wrap : 'a -> 'a P.t t
   the type variable 'a cannot be deduced from the type parameters.

    module Mk (P : sig
        type 'a t
      end) =
    struct
      type _ t = Wrap : 'a -> 'a P.t t
    end
*)

module Mk (P : sig
    type !'a t
  end) =
struct
  type _ t = Wrap : 'a -> 'a P.t t
end

module Mk' (P : sig
    type 'a t
  end) =
struct
  type (_, _) t = Wrap : 'a -> ('a P.t, 'a) t
end
