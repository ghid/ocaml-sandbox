module Helpers = struct
  let id x = x
  let const x _ = x
  let flip f x y = f y x
  let compose f g x = f (g x)
  let ( <.> ) f g = fun x -> f (g x)
  let ( >.> ) g f = fun x -> f (g x)
  let cons x xs = x :: xs
end
