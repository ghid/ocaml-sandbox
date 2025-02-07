(* Part 1: Semigroup intro *)

module type SEMIGROUP = sig
  type t

  val append : t -> t -> t
end

module IntAdd = struct
  type t = int

  let append = ( + )
end

module IntMul = struct
  type t = int

  let append = ( * )
end

module BoolAnd = struct
  type t = bool

  let append = ( && )
end

module BoolOr = struct
  type t = bool

  let append = ( || )
end

module String = struct
  type t = string

  let append = ( ^ )
end

(*
   ( - ) is not assoicative:

  module InsSub = struct
    type t = string

    let append = ( - )
  end
*)

(* Part 2: Composing Semigroups *)

module IntMin' = struct
  type t = int

  let append x y = if x <= y then x else y
end

module IntMax = struct
  type t = int

  let append x y = if x >= y then x else y
end

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module Min (C : COMPARABLE) = struct
  type t = C.t

  let append x y = if C.compare x y <= 0 then x else y
end

module Max (C : COMPARABLE) = struct
  type t = C.t

  let append x y = if C.compare x y >= 0 then x else y
end

module IntMin = Min (Int)
module FloatMin = Min (Float)

module IntFirst = struct
  type t = int

  let append x _ = x
end

module IntLast = struct
  type t = int

  let append _ y = y
end

module First (T : sig
    type t
  end) =
struct
  type t = T.t

  let append x _ = x
end

module Last (T : sig
    type t
  end) =
struct
  type t = T.t

  let append _ y = y
end

module type SEMIGROUP' = sig
  type t

  val ( <+> ) : t -> t -> t
end

module IntList' = struct
  type t = int list

  let append = ( @ )
end

module List (T : sig
    type t
  end) =
struct
  type t = T.t

  let append = ( @ )
end

module IntList = List (Int)

module Pair (S1 : SEMIGROUP) (S2 : SEMIGROUP) = struct
  type t = S1.t * S2.t

  let append (a1, b1) (a2, b2) = S1.append a1 a2, S2.append b1 b2
end

module PairStringInt = Pair (String) (IntAdd);;

PairStringInt.append ("foo", 3) ("bar", 5) (* - : string * int = ("foobar", 8) *)

module MinMax = Pair (Min (Int)) (Max (Int));;

MinMax.append (3, 3) (MinMax.append (7, 7) (5, 5))
(* - : int * int = (3, 7) *)

(* Part 3: Associativty *)
