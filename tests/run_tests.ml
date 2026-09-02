open Util
open DFA

(* DFA filename, input string, expected result *)
type test_case = string * string * bool

let test_cases : test_case array =
  [| ("../examples/ABC.dfa", "ABC", true)
   ; ("../examples/ABC.dfa", "123", false)
   ; ("../examples/ABC.dfa", "A2C", false)
  |]

let test_eval dfa input_str expect_result : bool =
  let (result : bool) = DFA.eval dfa (Util.explode_to_strings input_str) in
  result = expect_result

let main () =
  for i = 0 to (Array.length test_cases) - 1 do
    Printf.printf "Test %d: " i;
    let (filename, input_str, expect_result) = test_cases.(i) in
    let lines = In_channel.with_open_text filename In_channel.input_lines in
    let (dfa_opt : DFA.dfa option) = DFA.parse_lines lines in
    match dfa_opt with
      | None -> Printf.printf "error: failed to parse DFA\n";
      | Some dfa ->
         if test_eval dfa input_str expect_result
         then Printf.printf "PASSED\n"
         else Printf.printf "FAILED\n"
  done

let () = main ()
