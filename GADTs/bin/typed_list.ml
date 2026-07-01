type _ typed_list =
  | Nil : 'a typed_list
  | IntCons : int * int typed_list -> int typed_list
  | StringCons : string * string typed_list -> string typed_list

let rec list_length : type a. a typed_list -> int = function
  | Nil -> 0
  | IntCons (_, rest) -> 1 + list_length rest
  | StringCons (_, rest) -> 1 + list_length rest
;;

let rec sum_list : type a. a typed_list -> int = function
  | Nil -> 0
  | IntCons (n, rest) -> n + sum_list rest
  | StringCons (_, _) -> assert false
;;

let () =
  let lst = IntCons (1, IntCons (2, IntCons (3, Nil))) in
  assert (list_length lst = 3);
  assert (sum_list lst = 6)
;;
