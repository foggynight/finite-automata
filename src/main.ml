(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module Main = struct

open Util
open NFA
open DFA

let fail ?(code = 1) msg =
  Printf.eprintf "Error: %s\n" msg;
  exit code

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
    | Error msg -> fail ~code:2 ("parse failed: " ^ msg)
    | Ok fa ->
       if !flag_verbose then
         begin
           print_string (DFA.show fa);
           if input_strs <> [] then print_char '\n';
         end;
       List.iter
         (fun input_str ->
           let result = eval_fun fa (Util.explode_to_strings input_str) in
           if !flag_verbose then
             Printf.printf "%s => %b\n" (Util.show_string input_str) result;)
         input_strs
  in

  let fa_lines = In_channel.input_lines stdin in
  match !flag_fa_type with
  | "DFA" -> go fa_lines DFA.parse DFA.eval !input_strs
  | "NFA" -> go fa_lines NFA.parse NFA.eval !input_strs
  | str   -> fail ~code:1 ("invalid FA type: %s" ^ Util.show_string str)

let () = main ()

end (* module Main *)
