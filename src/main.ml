(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module Main = struct

open DFA
open Util

let eval dfa input_str =
  Util.newline ();
  print_string ("Evaluate: \"" ^ input_str ^ "\"\n");
  let result = DFA.eval dfa (Util.explode_to_strings input_str) in
  Printf.printf "Result: %b\n" result

let main () =
  let argc = Array.length Sys.argv in

  let (lines : string list) = In_channel.input_lines stdin in
  let (dfa_res : (DFA.dfa, string) result) = DFA.parse_lines lines in

  match dfa_res with
  | Error msg -> Printf.printf "Error: failed to parse DFA: %s\n" msg
  | Ok dfa ->
     print_string (DFA.show dfa);
     for i = 1 to (argc - 1) do
       eval dfa Sys.argv.(i)
     done

let () = main ()

end (* module Main *)
