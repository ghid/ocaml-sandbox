(* Double Ended Queue - Abbr. deque [dɛk] *)

type 'a deque =
  | Empty
  | Deque of 'a option * ('a * 'a) deque * 'a option

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
