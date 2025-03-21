open Helpers

(* Functors *)
(* ======== *)

module type FUNCTOR = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

(* In this case we do not need a workaround to implement the FUNCTOR for lists. *)
module ListF : FUNCTOR with type 'a t = 'a list = struct
  type 'a t = 'a list

  let fmap f = List.map f
end

(*
   As for monoids, functors need to saisfy some laws.

   - We map the identity to itself:
     fmap id = id

   - Mapping a composition of functions is equivalent to composing the mapped functions:
     fmap (f <.> g) = fmap f <.> fmap g
*)

module TestFunctor (F : FUNCTOR) = struct
  open F
  open Helpers

  let test_id x = assert (fmap id x = x)
  let test_compose f g x = assert (fmap (f <.> g) x = fmap f (fmap g x))
end

(* Many commonly used types are in fact functor instances. For example ... *)
module OptionF : FUNCTOR with type 'a t = 'a option = struct
  type 'a t = 'a option

  let fmap f = function
    | Some x -> Some (f x)
    | None -> None
  ;;
end

module ResultF : FUNCTOR with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let fmap f = function
    | Ok x -> Ok (f x)
    | Error _ as err -> err
  ;;
end

(* Functors often come with generic helpers as well *)
module FunctorUtils (F : FUNCTOR) = struct
  open Helpers

  (** Generic functor helpers.
      These should be part of the Functor module itself... *)
  open F

  (** A convenient shorthand for fmap *)
  let ( <$> ) f x = fmap f x

  (** Replace all locations in the input with the same value *)
  let ( <$ ) r x = fmap (const r) x

  (** Flipped version of <$ *)
  let ( $> ) r x = flip ( <$ ) r x

  (** [void] discards or ignores the result of evaluation *)
  let void f x = fmap (fun x -> ignore (f x)) x
end

(* Applicative Functors *)
(* ==================== *)

module type APPLICATIVE = sig
  type 'a t

  (* Include the signature of FUNCTOR,
     rewriting the types to make them match *)
  include FUNCTOR with type 'a t := 'a t

  (** Lift a value *)
  val pure : 'a -> 'a t

  (** Sequential application *)
  val ap : ('a -> 'b) t -> 'a t -> 'b t

  (* Note that we still have to define the functor,
     you can define [fmap] from the above functions as
     [let fmap f x = pure f <*> x]
  *)
end

module ApplicativeUtils (A : APPLICATIVE) = struct
  (** Generic function helpers.
      This should really be part of the APPLICATIVE module itself *)

  open Helpers
  open A
  module FunU = FunctorUtils (A)
  include FunU

  (** A convenient shorthand for ap - called apply *)
  let ( <*> ) f = ap f

  (* Below, we denote 'actions' the elements of the applicative typeclass *)

  (** Lift a function to actions. This function may be used as a value
      for [fmap] in a functor instance. *)
  let liftA f x = f <$> x

  (** Lift a binary function fo actions. *)
  let liftA2 f x y = f <$> x <*> y

  (** Lift a ternary function to actions. *)
  let liftA3 f x y z = f <$> x <*> y <*> z

  (** Sequence actions, discarding the value of the second argument. *)
  let ( <* ) r x = const <$> r <*> x

  (** Sequence actions, discarding the value of the first argument. *)
  let ( *> ) r x = (fun _ y -> y) <$> r <*> x
  (* == flip ( <* ) *)

  (* These should be part of foldable or traversable, and in turn
     they end up with applicatives *)

  (** Evaluate each action in the structure from left to right and
      collect the results. *)
  let rec sequenceA = function
    | [] -> pure []
    | x :: xs -> List.cons <$> x <*> sequenceA xs
  ;;

  (** Evaluate each action in the structure from left to right and
      ignore the results. *)
  let sequenceA_ xs = List.fold_right ( *> ) xs (pure ())

  (** Map each element of a structure to an action, evaluate these actions
      from left to right and collect the results. *)
  let traverseA f = List.map f >.> sequenceA

  (** Map each element of a structure to an action, evaluate these actions
      from left to right and ignore the results. *)
  let traverseA_ f xs = List.fold_right (( *> ) <.> f) xs (pure ())

  (** [forA] is [traverseA] with its arguments flipped. *)
  let forA xs = (flip traverseA) xs
end

(*
   A complete definition must satisfy the following laws:

  - Identity law
    pure id <*> v = v

  - Homomorphism
    pure f <*> pure x = pure (f x)

  - Interchange
    u <*> pure y = pure ($ y) <*> u

  - Composition
    pure (<.>) <*> u <*> v <*> w = u <*> (v <*> w)
*)

module TestApplicative (A : APPLICATIVE) = struct
  open Helpers
  open A
  module ApplU = ApplicativeUtils (A)
  open ApplU

  let test_id x = assert (pure id <*> x = x)
  let test_homomorphism f x = assert (pure f <*> pure x = pure (f x))
  let test_interchange u y = assert (u <*> pure y = (pure (fun f -> f y) <*> u))
  let test_composition u v w = assert (pure compose <*> u <*> v <*> w = (u <*> (v <*> w)))
end

(* Note that as a consequence of these laws, the functor instance [f] will satisfy
   [fmap f x = pure f <*> x]
*)

module ListA : APPLICATIVE with type 'a t = 'a list = struct
  include ListF

  (** Put a value in a list. *)
  let pure x = [ x ]

  (** Take a list of functions and a list of values
      and apply each function to each element of the
      list - in practice, this is a caresian product. *)
  let ap fs xs = fmap (fun f -> fmap (fun x -> f x) xs) fs |> List.concat
end

module OptionA : APPLICATIVE with type 'a t = 'a option = struct
  include OptionF

  (** Put a value in an optional. *)
  let pure x = Some x

  (** Take an option function and an option value
      and apply the function to the value if they both exist. *)
  let ap f x =
    match f, x with
    | Some f, Some x -> Some (f x)
    | _ -> None
  ;;
end

module ResultA : APPLICATIVE with type 'a t = ('a, string) result = struct
  include ResultF

  (** Put a value in a result. *)
  let pure x = Ok x

  (** Take a result function and a result value
      and apply the function to the value if they both exist. *)
  let ap f x =
    match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error e, Ok _ -> Error e
    | Ok _, Error e -> Error e
    | Error ef, Error ex -> Error (String.concat " " [ ef; ex ])
  ;;
end

(* Note here that we can replace the string with any monoidal type
   and the last line of ap would just be changed into 
   [| Error ef, Error ex -> Error (ef <> ex)].
*)

(* The identity applicative is simply ... *)
type 'a id = 'a

module IdApp : APPLICATIVE with type 'a t = 'a id = struct
  type 'a t = 'a id

  let pure x = x
  let fmap f = f
  let ap = fmap
end

(* What can we do with applicatives? *)

type vm_type =
  | PV
  | HVM

type storage_location =
  | Local
  | NFS
  | SCSI

type vgpu_type =
  | None
  | AMD
  | Nvidia

type vm =
  { storage_size : int
  ; storage_location : storage_location
  ; vgpu_type : vgpu_type
  ; vm_type : vm_type
  }

let vm_of storage_size storage_location vgpu_type vm_type =
  { storage_size; storage_location; vgpu_type; vm_type }
;;

(* We can generate all possible configuration in one go by lifting the constructor
   and listing the parameter values. *)
module ListApp = ApplicativeUtils (ListA)

let vm_templates =
  let open ListApp in
  let open FunU in
  vm_of
  <$> [ 1_000_000; 10_000_000; 10_000_000; 1234567890123456 ]
  <*> [ Local; NFS; SCSI ]
  <*> [ None; AMD; Nvidia ]
  <*> [ PV; HVM ]
;;
