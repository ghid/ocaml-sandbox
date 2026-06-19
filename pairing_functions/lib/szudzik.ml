let szudzik_pair (x, y) =
  assert (x >= 0 && y >= 0);
  let x = float_of_int x in
  let y = float_of_int y in
  if x >= y then (x *. x) +. x +. y else (y *. y) +. x
;;

let szudzik_unpair z =
  let z = float_of_int z in
  let a = Float.(floor (sqrt z)) in
  let b = z -. (a *. a) in
  let a = int_of_float a in
  let b = int_of_float b in
  if b >= a then a, b - a else b, a
;;
