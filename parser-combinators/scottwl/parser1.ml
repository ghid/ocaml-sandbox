(* tags: parser-combinator *)

(* Type that wraps a parsing function *)
type 'a parser = Parser of (string -> ('a * string, string) result)

(* Parse a single character *)
let pchar char_to_match =
  let inner_fn input =
    if input = ""
    then Error "No more input"
    else (
      let first = input.[0] in
      if first = char_to_match
      then (
        let remaining = String.drop_first 1 input in
        Ok (char_to_match, remaining))
      else (
        let msg = Printf.sprintf "Expecting '%c'. Got '%c'" char_to_match first in
        Error msg))
  in
  Parser inner_fn
;;

(* Run a parser with some input *)
let run parser input =
  (* Unwrap parser to get inner function *)
  let (Parser inner_fn) = parser in
  (* Call inner function with input *)
  inner_fn input
;;

let () =
  let pchar_A = pchar 'A' in
  let inputABC = "ABC" in
  assert (run pchar_A inputABC = Ok ('A', "BC"));
  let inputZBC = "ZBC" in
  assert (run pchar_A inputZBC = Error "Expecting 'A'. Got 'Z'")
;;
