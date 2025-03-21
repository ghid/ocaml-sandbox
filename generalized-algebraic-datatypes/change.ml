let rec change till amt =
  if amt = 0
  then [ [] ]
  else (
    match till with
    | [] -> []
    | c :: till ->
      if amt < c
      then change till amt
      else (
        let rec allc = function
          | [] -> []
          | cs :: css -> (c :: cs) :: allc css
        in
        allc (change (c :: till) (amt - c)) @ change till amt))
;;

exception Change

let rec change' till amt =
  if amt = 0
  then []
  else (
    match till with
    | [] -> raise Change
    | c :: till ->
      if amt < 0
      then raise Change
      else (
        try c :: change' (c :: till) (amt - c) with
        | Change -> change' till amt))
;;
