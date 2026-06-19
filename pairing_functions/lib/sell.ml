let sell_pair (x, y) =
  let a = if x >= 0 then x * 2 else (x * -2) - 1 in
  let b = if y >= 0 then y * 2 else (y * -2) - 1 in
  if a >= b then (a * a) + a + b else (b * b) + a
;;

let sell_unpair z =
  let a = Float.(floor (sqrt (float_of_int z))) |> int_of_float in
  let b = z - (a * a) in
  let r, r' = if b >= a then a, b - a else b, a in
  let a = if r mod 2 = 0 then r / 2 else (r + 1) / -2 in
  let b = if r' mod 2 = 0 then r' / 2 else (r' + 1) / -2 in
  a, b
;;
