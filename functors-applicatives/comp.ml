open Applic

module type S = sig
  type t

  val f : t -> t
end

(* Creating a Functor *)
module ListFunctor (M : S) = struct
  let map lst = List.map M.f lst
end

(* Composing Functions with Functors *)

module StringModule : S with type t = string = struct
  type t = string

  let f s = String.uppercase_ascii s
end

module StringListFunctor = ListFunctor (StringModule)

let _string_result = StringListFunctor.map [ "hello"; "world" ]

(* Composing with Applicatives *)

let add x y = x + y

let add_optionals x y =
  let open OptionApplicative in
  apply (apply (pure add) x) y
;;

let _opional_result = add_optionals (Some 3) (Some 4)
