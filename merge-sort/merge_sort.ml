[@@@warning "-32"]

type comparision =
  | LT
  | EQ
  | GT

let compare x y = if x < y then LT else if x > y then GT else EQ

let rec merge l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | x :: xs, y :: ys ->
    (match compare x y with
     | LT -> x :: merge xs l2
     | GT -> y :: merge l1 ys
     | EQ -> x :: y :: merge xs ys)
;;

let rec split l =
  match l with
  | [] | [ _ ] -> l, []
  | x :: y :: ls ->
    let xs, ys = split ls in
    x :: xs, y :: ys
;;

let rec merge_sort l =
  match l with
  | [] | [ _ ] -> l
  | _ ->
    let xs, ys = split l in
    merge (merge_sort xs) (merge_sort ys)
;;
