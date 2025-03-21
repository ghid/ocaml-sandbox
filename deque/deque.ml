(* Double Ended Queue - Abbr. deque [dɛk] *)

type 'a deque =
  | Empty
  | Deque of 'a option * ('a * 'a) deque * 'a option

let create = Empty
let is_empty deque = deque = Empty

let rec push_front : type a. a -> a deque -> a deque =
  fun x deque ->
  match deque with
  | Empty -> Deque (Some x, Empty, None)
  | Deque (left, child, right) ->
    (match left with
     | None -> Deque (Some x, child, right)
     | Some l -> Deque (None, push_front (x, l) child, right))
;;

let rec pop_front : type a. a deque -> a option * a deque = function
  | Empty -> None, Empty
  | Deque (Some x, Empty, None) -> Some x, Empty
  | Deque (Some x, child, right) -> Some x, Deque (None, child, right)
  | Deque (None, Empty, right) -> right, Empty
  | Deque (None, child, right) ->
    let front, new_deque = pop_front child in
    let first = Option.map fst front in
    let second = Option.map snd front in
    first, Deque (second, new_deque, right)
;;

let front deque = fst (pop_front deque)
let drop_front deque = snd (pop_front deque)

let rec push_back : type a. a -> a deque -> a deque =
  fun x deque ->
  match deque with
  | Empty -> Deque (Some x, Empty, None)
  | Deque (left, child, right) ->
    (match right with
     | None -> Deque (left, child, Some x)
     | Some l -> Deque (left, push_back (l, x) child, None))
;;

let rec pop_back : type a. a deque -> a option * a deque = function
  | Empty -> None, Empty
  | Deque (Some x, Empty, None) -> Some x, Empty
  | Deque (left, child, Some x) -> Some x, Deque (left, child, None)
  | Deque (left, Empty, None) -> left, Empty
  | Deque (left, child, None) ->
    let back, new_deque = pop_back child in
    let first = Option.map fst back in
    let second = Option.map snd back in
    second, Deque (left, new_deque, first)
;;

let back deque = fst (pop_back deque)
let drop_back deque = snd (pop_back deque)
