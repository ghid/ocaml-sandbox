let cantor_pair (x, y) =
  assert (x >= 0 && y >= 0);
  (* (x + y) * ((x + y) + 1) / 2 *)
  let sum = x + y in
  sum * (sum + 1) / 2
;;

let cantor_unpair z =
  let z = float_of_int z in
  let a = Float.(floor ((sqrt ((8. *. z) +. 1.) -. 1.) /. 2.)) in
  let b = ((a *. a) +. a) /. 2. in
  let y = z -. b in
  let x = a -. y in
  int_of_float x, int_of_float y
;;
