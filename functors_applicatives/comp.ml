module type S = sig
  type t

  val f : t -> t
end

(* Creating a Functor *)
module ListFunctor (M : S) = struct
  let map lst = List.map M.f lst
end

module type APPLICATIVE = sig
  type 'a t

  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

(* Creating an Applicative *)
module OptionApplicative : APPLICATIVE with type 'a t = 'a option = struct
  type 'a t = 'a option

  let pure x = Some x

  let apply f x =
    match f, x with
    | Some f', Some x' -> Some (f' x')
    | _ -> None
  ;;
end

(* Composing Functions with Functors *)

module StringModule : S with type t = string = struct
  type t = string

  let f s = String.uppercase_ascii s
end

module StringListFunctor = ListFunctor (StringModule)

let _string_result = StringListFunctor.map [ "hello"; "world" ]

(* Composing with Applicatives *)

let add x y = x + y

let add_optionals x y =
  let open OptionApplicative in
  apply (apply (pure add) x) y
;;

let _opional_result = add_optionals (Some 3) (Some 4)
