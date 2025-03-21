type z = Z : z
type 'n s = S : 'n -> 'n s

type nat =
  | NatZ
  | NatS of nat

type ('a, _) vec =
  | Nil : ('a, z) vec
  | Cons : 'a * ('a, 'n) vec -> ('a, 'n s) vec

let head : type a n. (a, n s) vec -> a = function
  | Cons (hd, _) -> hd
;;

let tail : type a n. (a, n s) vec -> (a, n) vec = function
  | Cons (_, tl) -> tl
;;

let rec map : type a b n. (a -> b) -> (a, n) vec -> (b, n) vec = function
  | f ->
    (function
      | Nil -> Nil
      | Cons (hd, tl) -> Cons (f hd, map f tl))
;;

let rec crush : type a b n. b -> (a -> b -> b) -> (a, n) vec -> b =
  fun init ->
  fun f ->
  (function
    | Nil -> init
    | Cons (hd, tl) -> crush (f hd init) f tl)
;;
