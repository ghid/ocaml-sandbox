[@@@warning "-32"]

open Effect
open Effect.Deep

(* Basics *)

type _ Effect.t += Xchg : int -> int t

let comp1 () = perform (Xchg 0) + perform (Xchg 1)

(* Concurrency *)

type 'a status =
  | Complete of int
  | Suspended of
      { msg : int
      ; cont : (int, 'a status) continuation
      }

let step (f : unit -> 'a) () : 'a status =
  match f () with
  | v -> Complete v
  | effect (Xchg msg), cont -> Suspended {msg; cont}
;;

let rec run_both a b =
  match a (), b () with
  | Complete va, Complete vb -> va, vb
  | Suspended { msg = m1; cont = k1 }, Suspended { msg = m2; cont = k2 } ->
    run_both (fun () -> continue k1 m2) (fun () -> continue k2 m1)
  | _ -> failwith "Improper synchronization"
;;

let comp2 () = perform (Xchg 21) * perform (Xchg 21)
let _ = run_both (step comp1) (step comp2)

(* User-level threads *)

type _ Effect.t += Fork : (unit -> unit) -> unit t | Yield : unit t

let fork f = perform (Fork f)
let yield () = perform Yield
let xchg v = perform (Xchg v)

(* A concurrent round-robin scheduler *)

let run (main : unit -> unit) : unit =
  let exchanger : (int * (int, unit) continuation) option ref =
    ref None (* waiting exchanger *)
  in
  let run_q = Queue.create () in
  (* scheduler queue *)
  let enqueue k v =
    let task () = continue k v in
    Queue.push task run_q
  in
  let dequeue () =
    if Queue.is_empty run_q
    then () (* done *)
    else (
      let task = Queue.pop run_q in
      task ())
  in
  let rec spawn (f : unit -> unit) : unit =
    match f () with
    | () -> dequeue ()
    | exception e ->
      print_endline (Printexc.to_string e);
      dequeue ()
    | effect Yield, k -> enqueue k (); dequeue ()
    | effect (Fork f), k -> enqueue k (); spawn f
    | effect (Xchg n), k -> (
      match !exchanger with
      | Some (n', k') -> exchanger := None; enqueue k' n; continue k n'
      | None -> exchanger := Some (n, k); dequeue ())
  in
  spawn main
;;

open Printf

let _ = run (fun _ ->
  fork (fun _ ->
    printf "[t1] Sending 0\n";
    let v = xchg 0 in
    printf "[t1] received %d\n" v);
  fork (fun _ ->
    printf "[t2] Sending 1\n";
    let v = xchg 1 in
    printf "[t2] received %d\n" v))
;;
