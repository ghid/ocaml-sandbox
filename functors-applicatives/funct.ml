module type S = sig
  type t

  val f : t -> t
end

(* Creating a Functor *)
module ListFunctor (M : S) = struct
  let map lst = List.map M.f lst
end

(* Using the Functor *)
module IntModule : S with type t = int = struct
  type t = int

  let f x = x + 1
end

module IntListFunctor = ListFunctor (IntModule)

let _ = IntListFunctor.map [ 1; 2; 3; 4 ]
