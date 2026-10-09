(* tags: parser-combinator *)

(* Type that wraps a parsing function *)
type 'a parser = Parser of (string -> ('a * string, string) result)

(* Run a parser with some input *)
let run parser input =
  (* Unwrap parser to get inner function *)
  let (Parser inner_fn) = parser in
  (* Call inner function with input *)
  inner_fn input
;;

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

let and_then parser1 parser2 =
  let inner_fn input =
    let result1 = run parser1 input in
    match result1 with
    | Error msg -> Error msg
    | Ok (value1, remaining1) ->
      let result2 = run parser2 remaining1 in
      (match result2 with
       | Error msg -> Error msg
       | Ok (value2, remaining2) ->
         let combined_value = value1, value2 in
         Ok (combined_value, remaining2))
  in
  Parser inner_fn
;;

let or_else parser1 parser2 =
  let inner_fn input =
    let result1 = run parser1 input in
    match result1 with
    | Ok _ -> result1
    | Error _ ->
      let result2 = run parser2 input in
      result2
  in
  Parser inner_fn
;;

let map_p f parser =
  let inner_fn input =
    let result = run parser input in
    match result with
    | Ok (value, remaining) ->
      let new_value = f value in
      Ok (new_value, remaining)
    | Error msg -> Error msg
  in
  Parser inner_fn
;;

let rec parse_0_or_more parser input =
  let first_result = run parser input in
  match first_result with
  | Error _ -> [], input
  | Ok (first_value, input_after_first_parse) ->
    let subsequent_values, remaining_input =
      parse_0_or_more parser input_after_first_parse
    in
    let values = first_value :: subsequent_values in
    values, remaining_input
;;

let many parser =
  let rec inner_fn input = Ok (parse_0_or_more parser input) in
  Parser inner_fn
;;

let many1 parser =
  let rec inner_fn input =
    let first_result = run parser input in
    match first_result with
    | Error err -> Error err
    | Ok (first_value, input_after_first_parse) ->
      let subsequent_values, remaining_input =
        parse_0_or_more parser input_after_first_parse
      in
      let values = first_value :: subsequent_values in
      Ok (values, remaining_input)
  in
  Parser inner_fn
;;

let ( >&> ) = and_then
let ( <|> ) = or_else
let ( <$> ) x f = map_p f x

(* Helper functions *)
let reduce f = function
  | [] -> failwith "empty list"
  | h :: t -> List.fold_left f h t
;;

let choice list_of_parsers = reduce ( <|> ) list_of_parsers
let any_of list_of_chars = list_of_chars |> List.map pchar |> choice

let sequence list_of_parsers =
  let concat_results p1 p2 = p1 >&> p2 <$> fun (a, b) -> a @ b in
  list_of_parsers
  |> List.map (fun parser -> parser <$> List.singleton)
  |> reduce concat_results
;;

let pstring str =
  str
  |> String.to_seq
  |> Seq.map pchar
  |> List.of_seq
  |> sequence
  <$> List.to_seq
  <$> String.of_seq
;;

let digit = any_of (List.init 10 (fun i -> Char.(chr (code '0' + i))))
let digits = many1 digit

let pint =
  let result_to_int digit_list =
    List.to_seq digit_list |> String.of_seq |> int_of_string
  in
  digits <$> result_to_int
;;

let () =
  let pchar_A = pchar 'A' in
  let inputABC = "ABC" in
  assert (run pchar_A inputABC = Ok ('A', "BC"));
  let inputZBC = "ZBC" in
  assert (run pchar_A inputZBC = Error "Expecting 'A'. Got 'Z'")
;;

let () =
  let parse_A = pchar 'A' in
  let parse_B = pchar 'B' in
  let parse_C = pchar 'C' in
  let parse_A_and_then_B = parse_A >&> parse_B in
  assert (run parse_A_and_then_B "ABC" = Ok (('A', 'B'), "C"));
  assert (run parse_A_and_then_B "ZBC" = Error "Expecting 'A'. Got 'Z'");
  assert (run parse_A_and_then_B "AZC" = Error "Expecting 'B'. Got 'Z'");
  let parse_A_or_else_B = parse_A <|> parse_B in
  assert (run parse_A_or_else_B "AZZ" = Ok ('A', "ZZ"));
  assert (run parse_A_or_else_B "BZZ" = Ok ('B', "ZZ"));
  assert (run parse_A_or_else_B "CZZ" = Error "Expecting 'B'. Got 'C'");
  let parse_B_or_else_C = parse_B <|> parse_C in
  let parse_A_and_then_B_or_C = parse_A >&> parse_B_or_else_C in
  assert (run parse_A_and_then_B_or_C "ABZ" = Ok (('A', 'B'), "Z"));
  assert (run parse_A_and_then_B_or_C "ACZ" = Ok (('A', 'C'), "Z"));
  assert (run parse_A_and_then_B_or_C "QBZ" = Error "Expecting 'A'. Got 'Q'");
  assert (run parse_A_and_then_B_or_C "AQZ" = Error "Expecting 'C'. Got 'Q'");
  let parse_A_and_then_B_as_string =
    parse_A >&> parse_B <$> fun (a, b) -> String.(of_char a ^ of_char b)
  in
  assert (run parse_A_and_then_B_as_string "ABC" = Ok ("AB", "C"));
  let parse_12 =
    pchar '1'
    >&> pchar '2'
    <$> (fun (a, b) -> String.(of_char a ^ of_char b))
    <$> int_of_string
  in
  assert (run parse_12 "12C" = Ok (12, "C"))
;;

let _ =
  let _a_z = List.init 26 (fun i -> Char.(chr (code 'a' + i))) in
  let _0_9 = List.init 10 (fun i -> Char.(chr (code '0' + i))) in
  let parse_lowercase = any_of _a_z in
  let parse_digits = any_of _0_9 in
  assert (run parse_lowercase "aBC" = Ok ('a', "BC"));
  assert (run parse_lowercase "ABC" = Error "Expecting 'z'. Got 'A'");
  assert (run parse_digits "1AB" = Ok ('1', "AB"));
  assert (run parse_digits "|AB" = Error "Expecting '9'. Got '|'");
  let parsers = [ pchar 'A'; pchar 'B'; pchar 'C' ] in
  let combined = sequence parsers in
  assert (run combined "ABCD" = Ok ([ 'A'; 'B'; 'C' ], "D"));
  let parseABC = pstring "ABC" in
  assert (run parseABC "ABCDE" = Ok ("ABC", "DE"));
  assert (run parseABC "AB|DE" = Error "Expecting 'C'. Got '|'")
;;

let _ =
  assert (run pint "1ABC" = Ok (1, "ABC"));
  assert (run pint "12ABC" = Ok (12, "ABC"));
  assert (run pint "123ABC" = Ok (123, "ABC"));
  assert (
    run
      (many (choice [ pstring "hello"; pstring "world" ] <$> String.uppercase_ascii))
      "helloworldhellohelloworldhello"
    = Ok ([ "HELLO"; "WORLD"; "HELLO"; "HELLO"; "WORLD"; "HELLO" ], ""))
;;
