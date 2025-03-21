(* See: https://ocaml.org/manual/5.3/gadts-tutorial.html *)

type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

(* 1 Recursive functions *)

let rec eval : type a. a term -> a = function
  | Int n -> n (* a = int *)
  | Add -> fun x y -> x + y (* a = int -> int -> int *)
  (* eval called at types (b->a) and b for fresh b *)
  | App (f, x) -> (eval f) (eval x)
;;

let two = eval (App (App (Add, Int 1), Int 1))

(* 2 Type inference *)

let rec sum : type a. a term -> _ =
  fun x ->
  let y =
    match x with
    | Int n -> n
    | Add -> 0
    | App (f, x) -> sum f + sum x
  in
  y + 1
;;

let get_int : int term -> int = function
  | Int n -> n
  | App (_, _) -> 0
;;

(* 3 Refrutation cases *)

type _ t =
  | Int : int t
  | Bool : bool t

let deep : (char t * int) option -> char = function
  | None -> 'c'
  | _ -> .
;;
