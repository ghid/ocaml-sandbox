module type APPLICATIVE = sig
  type 'a t

  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

(* Creating an Applicative *)
module OptionApplicative : APPLICATIVE with type 'a t = 'a option = struct
  type 'a t = 'a option

  let pure x = Some x

  let apply f x =
    match f, x with
    | Some f', Some x' -> Some (f' x')
    | _ -> None
  ;;
end

(* Using the Applicative *)
let add x y = x + y

let _result =
  OptionApplicative.apply
    (OptionApplicative.apply (OptionApplicative.pure add) (OptionApplicative.pure 3))
    (OptionApplicative.pure 4)
;;
