(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module Main = struct

open DFA
open NFA
open Util

(* type fa_type = DFA | NFA *)

let error msg = (* ?(code = 1) = *)
  Printf.eprintf "Error: %s\n" msg;
  exit 1 (* code *)

let main () =
  let flag_verbose = ref false
  and flag_fa_type = ref "DFA"
  and input_strs = ref [] in

  let speclist =
    [ ("-v", Arg.Set flag_verbose, "Enable verbose output messages.")
    ; ("-x", Arg.Set_string flag_fa_type, "{DFA,NFA} Type of automaton to simulate.")
    ]
  and usage_msg = "Usage: finite-automata [-hv] [-x {DFA,NFA}] INPUT_STRS" in

  Arg.parse speclist (fun arg -> input_strs := arg :: !input_strs) usage_msg;
  input_strs := List.rev !input_strs;

  let go fa_lines parse_fun eval_fun input_strs =
    match parse_fun fa_lines with
    | Error msg -> error ("failed to parse automaton: " ^ msg)
    | Ok fa ->
       if !flag_verbose then print_string (DFA.show fa);
       List.iter
         (fun input_str ->
           let result = eval_fun fa (Util.explode_to_strings input_str) in
           if !flag_verbose then
             begin
               Util.newline ();
               print_string ("Evaluate: \"" ^ input_str ^ "\"\n");
               Printf.printf "Result: %b\n" result
             end)
         input_strs
  in
  let fa_lines = In_channel.input_lines stdin in
  match !flag_fa_type with
  | "DFA" -> go fa_lines DFA.parse_lines DFA.eval !input_strs
  | "NFA" -> go fa_lines DFA.parse_lines NFA.eval !input_strs
  | str   -> error ("invalid FA type: %s" ^ Util.show_string str)

let () = main ()

end (* module Main *)
