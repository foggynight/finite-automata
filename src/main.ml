open DFA
open Util

let main () =
  let (dfa_input : string) = "ABC" in
  let (lines : string list) = In_channel.input_lines stdin in
  let (parsed : DFA.dfa option) = DFA.parse_lines lines in
  match parsed with
    | None -> Printf.printf "Error: Failed to parse DFA\n"
    | Some dfa ->
       let result = DFA.eval dfa (Util.explode_to_strings dfa_input) in
       Printf.printf "Result: %b\n" result

let () = main ()
