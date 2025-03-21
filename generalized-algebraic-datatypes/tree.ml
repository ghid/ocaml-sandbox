(* Tree *)

type 'a tree =
  | Empty : 'a tree
  | Tree : 'a tree * 'a * 'a tree -> 'a tree

let incr : int tree -> int tree = function
  | Empty -> Empty
  | Tree (l, v, r) -> Tree (l, v + 1, r)
;;

let mx : int -> int -> int = function
  | a ->
    (function
      | b -> if a > b then a else b)
;;

let rec depth : 'a. 'a tree -> int = function
  | Empty -> 0
  | Tree (l, _, r) -> 1 + mx (depth l) (depth r)
;;

let top : 'a. 'a tree -> 'a option = function
  | Empty -> None
  | Tree (_, v, _) -> Some v
;;

let rec swivel : 'a. 'a tree -> 'a tree = function
  | Empty -> Empty
  | Tree (l, v, r) -> Tree (swivel r, v, swivel l)
;;

let rec zip : 'a 'b. 'a tree -> 'b tree -> ('a * 'b) tree =
  fun t1 t2 ->
  match t1, t2 with
  | Empty, Empty -> Empty
  | Tree (l1, v1, r1), Tree (l2, v2, r2) -> Tree (zip l1 l2, (v1, v2), zip r1 r2)
  | _ -> failwith "Trees are not the same shape"
;;

(* Gtree *)

type z = Z : z
type 'n s = S : 'n -> 'n s

type ('a, _) gtree =
  | EmptyG : ('a, z) gtree
  | TreeG : ('a, 'n) gtree * 'a * ('a, 'n) gtree -> ('a, 'n s) gtree

(* Gtree functions *)

let incrG : type n. (int, n s) gtree -> (int, n s) gtree = function
  | TreeG (l, v, r) -> TreeG (l, v + 1, r)
;;

let rec depthG : type a n. (a, n) gtree -> n = function
  | EmptyG -> Z
  | TreeG (l, _, _) -> S (depthG l)
;;

let topG : type a n. (a, n s) gtree -> a = function
  | TreeG (_, v, _) -> v
;;

let rec swivelG : type a n. (a, n) gtree -> (a, n) gtree = function
  | EmptyG -> EmptyG
  | TreeG (l, v, r) -> TreeG (swivelG r, v, swivelG l)
;;

let rec zipG : type a b n. (a, n) gtree -> (b, n) gtree -> (a * b, n) gtree =
  fun t1 t2 ->
  match t1, t2 with
  | EmptyG, EmptyG -> EmptyG
  | TreeG (l1, v1, r1), TreeG (l2, v2, r2) -> TreeG (zipG l1 l2, (v1, v2), zipG r1 r2)
;;
