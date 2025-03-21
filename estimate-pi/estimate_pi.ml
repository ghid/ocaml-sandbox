[@@@warning "-32"]

let fact n =
  let rec fact_aux n a =
    match n with
    | 0 -> 1
    | 1 -> a
    | n -> fact_aux (n - 1) (n * a)
  in
  fact_aux n 1
;;

(*
   See also: https://en.wikipedia.org/wiki/Pi

    @startuml
    :<math>1/pi=(2sqrt2)/9801 sum_(k=0)^oo((4k)!(1103+26390k))/((k!)^4 396^(4k))</math>;
    @enduml

  Use: sed -ne '/@startuml/,/@enduml/p' estimate_pi.ml | plantuml -p | fim -i
*)

let x = 2. *. sqrt 2. /. 9801.
let numerator k = fact (4 * k) * (1103 + (26390 * k))
let denominator k = (float_of_int (fact k) ** 4.) *. (396. ** (4. *. float_of_int k))

let estimate_pi n =
  let rec estimate_pi_aux k sum =
    if k >= 0
    then estimate_pi_aux (k - 1) sum +. (float_of_int (numerator k) /. denominator k)
    else sum
  in
  1. /. (x *. estimate_pi_aux (n + 1) 0.0)
;;

let estimate_pi' n =
  let sum = ref 0.0 in
  for k = 0 to n do
    sum := !sum +. (float_of_int (numerator k) /. denominator k)
  done;
  1. /. (x *. !sum)
;;

let () = Printf.printf "\n%.20f\n" (estimate_pi 10)
