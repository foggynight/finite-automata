(* DFA.ml - Deterministic Finite Automata in OCaml. *)
(* Copyright (C) 2026 Robert Coffey *)

open Option.Syntax

open Util

module DFA = struct

type dfa_trans = {
    c : char;
    next_state : int;
  }

type dfa_state = {
    id : int;
    transs : dfa_trans list;
  }

type dfa = {
    states : dfa_state list;
    accepts : int list;
  }

let find_state (lst : dfa_state list) (state : int) : dfa_state option =
  List.find_opt (fun x -> x.id = state) lst

let find_trans (lst : dfa_trans list) (c : char) : dfa_trans option =
  List.find_opt (fun x -> x.c = c) lst

let step (dfa : dfa) (state : int) (c : char) : int =
  match find_state dfa.states state with
  | None -> 0
  | Some st ->
     match find_trans st.transs c with
     | None -> 0
     | Some trans -> trans.next_state

let rec eval (dfa : dfa) (state : int) (cs : char list) : bool =
  Printf.printf "State = %d; Input = %s\n" state (Util.implode cs);
  match cs with
  | [] -> List.mem state dfa.accepts
  | head :: tail ->
     let next_state = step dfa state head in
     eval dfa next_state tail

(* DFA Description File Parser ************************************************)

let line_is_comment (line : string) : bool =
  let res = String.starts_with ~prefix:"--" line in
  if res then Printf.printf "Comment: %s\n" line;
  res

let parse_single_line (lines : string list) : (string * string list) option =
  Util.uncons lines

let parse_counted_lines (lines : string list)
    : (int * string list * string list) option =
  let* (head, tail) = Util.uncons lines in
  let n = int_of_string head in
  match tail with
    | [] -> None
    | rest_lines ->
       if List.length rest_lines < n
       then None
       else Some (n, List.take n rest_lines, List.drop n rest_lines)

(* Expects a list of strings representing the lines of a DFA description file,
 * with the terminating newline characters removed. *)
let parse_lines (lines : string list) : dfa option =
  let lines = List.filter
                (fun x -> not (Util.string_space_only x || line_is_comment x))
                lines
  in

  let* (albet_size, dfa_albet_strs, lines) = parse_counted_lines lines in
  Printf.printf "Alphabet Size: %d\n" albet_size;
  Util.print_list_string dfa_albet_strs;
  print_char '\n';

  let* (state_count, dfa_state_strs, lines) = parse_counted_lines lines in
  Printf.printf "State Count: %d\n" state_count;
  Util.print_list_string dfa_state_strs;
  print_char '\n';

  let* (init_state_str, lines) = parse_single_line lines in
  Printf.printf "Initial State: %s\n" init_state_str;

  let* (accept_count, accept_state_strs, lines) = parse_counted_lines lines in
  Printf.printf "Accepting State Count: %d\n" accept_count;
  Util.print_list_string accept_state_strs;
  print_char '\n';

  let* (trans_count, trans_strs, lines) = parse_counted_lines lines in
  Printf.printf "Transition Count: %d\n" trans_count;
  Util.print_list_string trans_strs;
  print_char '\n';

  if lines = []
  then Some { states = []; accepts = [] }
  else None

end
