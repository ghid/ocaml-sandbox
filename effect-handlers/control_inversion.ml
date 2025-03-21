[@@@warning "-32"]

open Effect
open Effect.Deep

let invert (type a) ~(iter : (a -> unit) -> unit) : a Seq.t =
  let module M = struct
    type _ Effect.t += Yield : a -> unit t
  end
  in
  let yield v = perform (M.Yield v) in
  fun () ->
    match iter yield with
    | () -> Seq.Nil
  | effect M.Yield v, k -> Seq.Cons (v, continue k)

let lst_iter = Fun.flip List.iter [ 1; 2; 3 ]
let s = invert ~iter:(Fun.flip String.iter "OCaml")
let next = Seq.to_dispenser s;;

