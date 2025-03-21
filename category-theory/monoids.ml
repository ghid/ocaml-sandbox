(* See: https://www.mseri.me/typeclass-ocaml/ *)

(* Monoids *)
(* ======= *)

module type MONOID = sig
  type t

  (** Neutral element; without this we have a semigroup. *)
  val mempty : t

  (** Associative operation. *)
  val mappend : t -> t -> t
end

(* Implement monoid modules for integers for Sum and Prod. *)
module Sum : MONOID with type t = int = struct
  type t = int

  let mempty = 0
  let mappend = ( + )
end

module Prod : MONOID with type t = int = struct
  type t = int

  let mempty = 1
  let mappend = ( * )
end

(*
   These are true monoids if we make sure that

    closed: however you choose x, y, z with type t below...

    - Neutral element
      mappend mempty x = x
      mappend x memphy = x

    - Associative
      mappend x (mappend y z) = mappend (mappend x y) z
*)

(* Add a simple test module to a monoid by using an (OCaml) functor. *)
module TestMonoid (M : MONOID) = struct
  open M

  let test_neutral_element x =
    assert (mappend mempty x = x);
    assert (mappend x mempty = x)
  ;;

  let test_assoc x y z = assert (mappend x (mappend y z) = mappend (mappend x y) z)
end

(* Run the tests of [TestMonoid] for [Sum] and [Prod] monoids. *)
module TestSum = TestMonoid (Sum)
module TestProd = TestMonoid (Prod)

let () =
  TestSum.test_neutral_element 5;
  TestSum.test_assoc 1 2 3;
  TestProd.test_neutral_element 6;
  TestProd.test_assoc 4 5 6;
  print_endline "Tests successful"
;;

let example1 () =
  let xs = [ 1; 2; 3; 4; 5; 6 ] in
  let sum = List.fold_left Sum.mappend Sum.mempty in
  let prod = List.fold_left Prod.mappend Prod.mempty in
  Printf.printf "sum: %d prod: %d\n" (sum xs) (prod xs)
;;

module MonoidUtils (M : MONOID) = struct
  (** Generic Monoid helpers.
      These should really be part of the Monoid module itself. *)
  open M

  (** A convenient shorthand for mappend *)
  let ( <+> ) x y = mappend x y

  (** Any monoid can be concatenated.
      This is more general and works for any foldable... *)
  let concat xs = List.fold_left ( <+> ) mempty xs
end

(* Another common example of monoid, that does not involve numbers *)
module StringM : MONOID with type t = string = struct
  type t = string

  let mempty = ""
  let mappend s1 s2 = s1 ^ s2
end

(* Let's try another one...
   Broken example:

module ListM : MONOID with type t = 'a list = struct
  type t = 'a list

  let mempty = []
  let mappend = ( @ )
end
*)

(* ... this can be workaround be using an intermediate dummy module *)
module type GENERIC_TYPE_WORKAROUND = sig
  type t
end

module ListM (T : GENERIC_TYPE_WORKAROUND) : MONOID with type t = T.t list = struct
  type t = T.t list

  let mempty = []
  let mappend = ( @ )
end

let example2 () =
  let xs = [ 1; 2; 3; 4; 5; 6 ] in
  let module MSum = MonoidUtils (Sum) in
  let module MProd = MonoidUtils (Prod) in
  Printf.printf "sum: %d prod %d\n" (MSum.concat xs) (MProd.concat xs)
;;

(* Note how to label the type to make it polymorphic *)
let example3 () =
  let xs = [ [ 1; 2; 3 ]; [ 1; 2; 3 ]; [ 1; 2; 3 ] ] in
  let concat (type a) xs =
    let module ListM =
      ListM (struct
        type t = a
      end)
    in
    let module ListU = MonoidUtils (ListM) in
    ListU.concat xs
  in
  concat xs
;;

(* Two other interesting monoids are [Any] and [All]: *)
module All : MONOID with type t = bool = struct
  type t = bool

  let mempty = false
  let mappend = ( && )
end

module Any : MONOID with type t = bool = struct
  type t = bool

  let mempty = false
  let mappend = ( || )
end

let example4 () =
  let xs = [ true; false; false; true ] in
  let xs' = [ true; true ] in
  let xs'' = [ false; false ] in
  let module AllU = MonoidUtils (All) in
  let module AnyU = MonoidUtils (Any) in
  Printf.printf "all: %b %b %b\n" (AllU.concat xs) (AllU.concat xs') (AllU.concat xs'');
  Printf.printf "any: %b %b %b\n" (AnyU.concat xs) (AnyU.concat xs') (AnyU.concat xs'')
;;
