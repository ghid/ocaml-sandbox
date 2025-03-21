type z = Z : z
type 'n s = S : 'n -> 'n s

let zero : z = Z
let one : z s = S Z

type ('a, _) vec =
  | VNil : ('a, z) vec
  | VCons : 'a * ('a, 'n) vec -> ('a, 'n s) vec

let v' : ('a, z) vec = VNil
let v'' : (int, z s) vec = VCons (3, VNil)

module Vector : sig
  val vhd : ('a, 'n s) vec -> 'a
  val vtl : ('a, 'n s) vec -> ('a, 'n) vec
end = struct
  let vhd : type a n. (a, n s) vec -> a = function
    | VCons (hd, _) -> hd
  ;;

  let vtl : type a n. (a, n s) vec -> (a, n) vec = function
    | VCons (_, tl) -> tl
  ;;
end
