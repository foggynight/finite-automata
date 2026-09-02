(* DFA.ml - Deterministic Finite Automata in OCaml. *)
(* Copyright (C) 2026 Robert Coffey *)

module DFA = struct

open Util

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

let find_state dfa state : string option =
  if Array.mem state dfa.states then Some state else None

let find_trans dfa state albet_sym : dfa_trans option =
  Array.find_opt
    (fun x -> x.curr_state = state && x.albet_sym = albet_sym)
    dfa.transs

let step (dfa : dfa) (state : string) (albet_sym : string)
    : (string, string) result =
  let (let*) = Result.bind in
  let* state =
    match find_state dfa state with
    | None -> Error "failed to find current state"
    | Some state -> Ok state
  in
  let* trans =
    match find_trans dfa state albet_sym with
    | None -> Error "failed to find transition for..."
    | Some trans -> Ok trans
  in
  Ok trans.next_state

let eval (dfa : dfa) (input : string list) : bool =
  let rec go curr_state curr_input =
    match curr_input with
    | [] -> Array.mem curr_state dfa.accept_states
    | head :: tail ->
       match step dfa curr_state head with
       | Error msg -> false
       | Ok next_state -> go next_state tail
  in go dfa.init_state input

let show_trans ({ curr_state : string;
                  albet_sym : string;
                  next_state : string;
                } : dfa_trans) : string =
  List.fold_left
    (fun acc x -> acc ^ " " ^ x)
    curr_state
    [albet_sym; next_state]

let show ({ albet : string array;
            states : string array;
            init_state : string;
            accept_states : string array;
            transs : dfa_trans array;
          } : dfa) : string =
  let str_albet =
    (Printf.sprintf "Alphabet (%d):\n" (Array.length albet))
    ^ (Util.show_list_string (Array.to_list albet))
    ^ "\n" in

  let str_states =
    (Printf.sprintf "States (%d):\n" (Array.length states))
    ^ (Util.show_list_string (Array.to_list states))
    ^ "\n" in

  let str_init_state = Printf.sprintf "Initial State: \"%s\"\n" init_state in

  let str_accept_states =
    (Printf.sprintf "Accepting States (%d):\n" (Array.length accept_states))
    ^ (Util.show_list_string (Array.to_list accept_states))
    ^ "\n" in

  let trans_strs = Array.map show_trans transs in
  let str_trans_strs =
    (Printf.sprintf "Transitions (%d):\n" (Array.length transs))
    ^ (Util.show_list_string (Array.to_list trans_strs))
    ^ "\n" in

  str_albet ^ str_states ^ str_init_state ^ str_accept_states ^ str_trans_strs

(* DFA Description File Parser ************************************************)

let line_is_comment line = String.starts_with ~prefix:"--" line

let extract_single_line (lines : string list)
    : (string * string list, string) result =
  match Util.uncons lines with
    | None -> Error "failed to extract single line"
    | Some x -> Ok x

let extract_counted_lines (lines : string list)
      : (int * string list * string list, string) result =
  let (let*) = Result.bind in
  let* (head, tail) =
    Option.to_result
      ~none:"failed to extract counted lines: missing count line"
      (Util.uncons lines)
  in
  match tail with
    | [] -> Error "failed to extract counted lines: missing body lines"
    | rest_lines ->
       let* n =
         Option.to_result
           ~none:"failed to parse count line"
           (int_of_string_opt head)
       in
       if List.length rest_lines < n
       then Error "failed to extract counted lines: missing body lines"
       else Ok (n, List.take n rest_lines, List.drop n rest_lines)

let parse_counted_lines_array (lines : string list)
    : (string array * string list, string) result =
  let (let*) = Result.bind in
  let* (count, targ_lines, rest_lines) = extract_counted_lines lines in
  Ok (Array.of_list targ_lines, rest_lines)

let rec parse_trans (str : string) : dfa_trans option =
  match Util.string_split_whitespace str with
  | [curr_state; albet_sym; next_state] ->
     Some { curr_state; albet_sym ; next_state }
  | _ -> None

(* Expects a list of strings representing the lines of a DFA description file,
 * with the terminating newline characters removed. *)
let parse_lines lines : (dfa, string) result =
  let (let*) = Result.bind in
  let lines =
    List.filter
      (fun x -> not (Util.string_space_only x || line_is_comment x))
      lines
  in
  let* (albet, lines) = parse_counted_lines_array lines in
  let* (states, lines) = parse_counted_lines_array lines in
  let* (init_state, lines) = extract_single_line lines in
  let* (accept_states, lines) = parse_counted_lines_array lines in
  let* (trans_strs, lines) = parse_counted_lines_array lines in
  let transs =
    List.filter_map parse_trans (Array.to_list trans_strs)
    |> Array.of_list
  in
  if Array.length transs <> Array.length trans_strs then
    Error "failed to parse transition (TODO: output invalid transitions)"
  else if lines <> [] then Error "input remaining"
  else Ok { albet; states; init_state; accept_states; transs; }

end (* module DFA *)
