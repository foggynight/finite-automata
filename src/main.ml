open DFA
open Util

module Main = struct

let eval dfa input_str =
  Util.newline ();
  print_string ("Evaluate: \"" ^ input_str ^ "\"\n");
  let result = DFA.eval dfa (Util.explode_to_strings input_str) in
  Printf.printf "Result: %b\n" result

let main () =
  let argc = Array.length Sys.argv in

  let (lines : string list) = In_channel.input_lines stdin in
  let (dfa_opt : DFA.dfa option) = DFA.parse_lines lines in

  match dfa_opt with
    | None -> Printf.printf "Error: Failed to parse DFA\n"
    | Some dfa ->
       print_string (DFA.show dfa);
       for i = 1 to (argc - 1) do
         eval dfa Sys.argv.(i)
       done

let () = main ()

end (* module Main *)
