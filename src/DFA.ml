(* DFA.ml - Deterministic Finite Automata in OCaml. *)
(* Copyright (C) 2026 Robert Coffey *)

open Option.Syntax

open Util

module DFA = struct

type dfa_trans = {
    curr_state : string;
    albet_sym : string;
    next_state : string;
  }

type dfa = {
    albet : string array;
    states : string array;
    init_state : string;
    accept_states : string array;
    transs : dfa_trans array;
  }

(* TODO: Use alphabet and state indices during computation. This function
 * currently just checks if given state is within list of DFA states. *)
let find_state dfa state : string option =
  if Array.mem state dfa.states then Some state else None

let find_trans dfa state albet_sym : dfa_trans option =
  Array.find_opt
    (fun x -> x.curr_state = state && x.albet_sym = albet_sym)
    dfa.transs

let step (dfa : dfa) (state : string) (albet_sym : string) : string option =
  let* state = find_state dfa state in
  let* trans = find_trans dfa state albet_sym in
  Some trans.next_state

let eval (dfa : dfa) (input : string list) : bool =
  let rec go curr_state curr_input =
    match curr_input with
    | [] ->
       Printf.printf "State = %s; Symbol = \n" curr_state;
       Array.mem curr_state dfa.accept_states
    | head :: tail ->
       Printf.printf "State = %s; Symbol = %s\n" curr_state head;
       match step dfa curr_state head with
       | None -> false
       | Some next_state -> go next_state tail
  in go dfa.init_state input

(* DFA Description File Parser ************************************************)

let line_is_comment line = String.starts_with ~prefix:"--" line

let extract_single_line (lines : string list) : (string * string list) option =
  Util.uncons lines

let extract_counted_lines (lines : string list)
      : (int * string list * string list) option =
  let* (head, tail) = Util.uncons lines in
  let n = int_of_string head in
  match tail with
    | [] -> None
    | rest_lines ->
       if List.length rest_lines < n
       then None
       else Some (n, List.take n rest_lines, List.drop n rest_lines)

let parse_counted_lines_array (lines : string list)
    : (string array * string list) option =
  let* (count, targ_lines, rest_lines) = extract_counted_lines lines in
  Some (Array.of_list targ_lines, rest_lines)

let rec parse_trans (str : string) : dfa_trans option =
  match Util.string_split_whitespace str with
  | [curr_state; albet_sym; next_state] ->
     Some { curr_state; albet_sym ; next_state }
  | _ -> None

(* Expects a list of strings representing the lines of a DFA description file,
 * with the terminating newline characters removed. *)
let parse_lines lines : dfa option =
  let lines =
    List.filter
      (fun x -> not (Util.string_space_only x || line_is_comment x))
      lines
  in

  (* let* (albet_size, dfa_albet_strs, lines) = extract_counted_lines lines in *)
  let* (albet, lines) = parse_counted_lines_array lines in
  Printf.printf "Alphabet (%d):\n" (Array.length albet);
  Util.print_list_string (Array.to_list albet);
  print_char '\n';

  let* (states, lines) = parse_counted_lines_array lines in
  Printf.printf "States (%d):\n" (Array.length states);
  Util.print_list_string (Array.to_list states);
  print_char '\n';

  let* (init_state, lines) = extract_single_line lines in
  Printf.printf "Initial State: \"%s\"" init_state;

  let* (accept_states, lines) = parse_counted_lines_array lines in
  Printf.printf "Accepting States (%d):\n" (Array.length accept_states);
  Util.print_list_string (Array.to_list accept_states);
  print_char '\n';

  let* (transs, lines) = parse_counted_lines_array lines in
  let dfa_transs =
    List.filter_map parse_trans (Array.to_list transs)
    |> Array.of_list
  in
  if Array.length dfa_transs <> Array.length transs
  then (Printf.printf "Failed to parse transition: (TODO)\n";
        None)
  else (Printf.printf "Transitions (%d):\n" (Array.length transs);
        Util.print_list_string (Array.to_list transs);
        print_char '\n';

        if lines <> []
        then None
        else Some {
                 albet; states; init_state; accept_states; transs = dfa_transs;
               }
        )

end
