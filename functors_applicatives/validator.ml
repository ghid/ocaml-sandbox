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

(* Defining the Validation Function *)
module type VALIDATION = sig
  type t

  val validate : t -> bool
end

module AgeValidation : VALIDATION with type t = int = struct
  type t = int

  let validate age = age >= 0 && age <= 120
end

module NameValidation : VALIDATION with type t = string = struct
  type t = string

  let validate name = String.length name > 0
end

module ValidationFunctor (V : VALIDATION) = struct
  let validate_input input = if V.validate input then Some input else None
end

(* Using the Validation Functor *)

module ValidateAge = ValidationFunctor (AgeValidation)
module ValidateName = ValidationFunctor (NameValidation)

let _valid_age = ValidateAge.validate_input 25
let _valid_name = ValidateName.validate_input "Alice"

(* Combinig Validations with Applicatives *)

let validate_user age name =
  let open OptionApplicative in
  apply
    (apply (pure (fun a n -> a, n)) (ValidateAge.validate_input age))
    (ValidateName.validate_input name)
;;

let _user_result = validate_user 25 "Alice"
