type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | String : string -> string expr
  | Add : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
  | Equal : int expr * int expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Concat : string expr * string expr -> string expr

let rec eval : type a. a expr -> a = function
  | Int n -> n
  | Bool b -> b
  | String s -> s
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Equal (e1, e2) -> eval e1 = eval e2
  | If (cond, e1, e2) -> if eval cond then eval e1 else eval e2
  | Concat (s1, s2) -> eval s1 ^ eval s2
;;

let rec to_string : type a. a expr -> string = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | String s -> "'" ^ s ^ "'"
  | Add (e1, e2) -> "(" ^ to_string e1 ^ " + " ^ to_string e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ to_string e1 ^ " - " ^ to_string e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ to_string e1 ^ " * " ^ to_string e2 ^ ")"
  | Equal (e1, e2) -> "(" ^ to_string e1 ^ " = " ^ to_string e2 ^ ")"
  | If (cond, e1, e2) ->
    "(if " ^ to_string cond ^ " then " ^ to_string e1 ^ " else " ^ to_string e2 ^ ")"
  | Concat (s1, s2) -> "(" ^ to_string s1 ^ " ^ " ^ to_string s2 ^ ")"
;;

let rec count_ops : type a. a expr -> int = function
  | Int _ | Bool _ | String _ -> 0
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Equal (e1, e2) ->
    1 + count_ops e1 + count_ops e2
  | If (cond, e1, e2) -> 1 + count_ops cond + count_ops e1 + count_ops e2
  | Concat (s1, s2) -> 1 + count_ops s1 + count_ops s2
;;

(* Korrekt *)
let () = assert (eval (Add (Int 5, Mul (Int 2, Int 3))) = 11)
let () = assert (eval (Equal (Int 10, Add (Int 5, Int 5))) = true)
let () = assert (eval (If (Equal (Int 1, Int 1), Int 42, Int 0)) = 42)
let () = assert (eval (If (Bool true, Int 5, Int 10)) = 5)
let () = assert (eval (If (Bool false, Bool true, Bool false)) = false)

let () =
  assert (
    eval (Concat (String "Hello", Concat (String ", ", String "World!")))
    = "Hello, World!")
;;

let () =
  let expr = If (Equal (Int 5, Int 5), Concat (String "Yes", String "!"), String "No") in
  assert (to_string expr = "(if (5 = 5) then ('Yes' ^ '!') else 'No')")
;;

let () = assert (count_ops (Add (Int 1, Mul (Int 2, Int 3))) = 2)

(* FEHLER - Der Typ-Checker verweigert dies! *)
(* let bad = Add (Bool true, Int 5) *)
