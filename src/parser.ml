(* Parser for finite automaton description files. *)
(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

(* NOTE: `lines` refers to a list of strings, each representing a single line of
 * text, with the newline character(s) removed. *)

(* TODO: Verify states in transitions are valid. *)

module Parser = struct

open Util
open NFA
open DFA

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

let rec parse_trans (str : string) : DFA.trans option =
  match Util.string_split_whitespace str with
  | [curr_state; albet_sym; next_state] ->
     Some { curr_state; albet_sym ; next_state }
  | _ -> None

let invalid_state_msg state_type state =
  Printf.sprintf "invalid %s state: %s" state_type state

let parse_DFA (lines : string list) : (DFA.t, string) result =
  let (let*) = Result.bind in
  let lines =
    List.filter
      (fun x -> not (Util.string_space_only x || line_is_comment x))
      lines
  in
  let* (albet, lines) = parse_counted_lines_array lines in
  let* (states, lines) = parse_counted_lines_array lines in
  let* (init_state, lines) = extract_single_line lines in
  let* () =
    if not (Array.mem init_state states)
    then Error (invalid_state_msg "initial" init_state)
    else Ok ()
  in
  let* (accept_states, lines) = parse_counted_lines_array lines in
  let* () =
    match Array.find_opt (fun x -> not (Array.mem x states)) accept_states with
    | Some invalid_state -> Error (invalid_state_msg "accept" invalid_state)
    | None -> Ok ()
  in
  let* (trans_strs, lines) = parse_counted_lines_array lines in
  let transs =
    List.filter_map parse_trans (Array.to_list trans_strs)
    |> Array.of_list
  in
  if Array.length transs <> Array.length trans_strs then
    Error "failed to parse transition (TODO: output invalid transitions)"
  else if lines <> [] then Error "input remaining"
  else Ok ({ albet; states; init_state; accept_states; transs; } : DFA.t)

let parse_NFA (lines : string list) : (NFA.t, string) result =
  let (let*) = Result.bind in
  let lines =
    List.filter
      (fun x -> not (Util.string_space_only x || line_is_comment x))
      lines
  in
  let* (albet, lines) = parse_counted_lines_array lines in
  let* (states, lines) = parse_counted_lines_array lines in
  let* (init_state, lines) = extract_single_line lines in
  let* () =
    if not (Array.mem init_state states)
    then Error (invalid_state_msg "initial" init_state)
    else Ok ()
  in
  let* (accept_states, lines) = parse_counted_lines_array lines in
  let* () =
    match Array.find_opt (fun x -> not (Array.mem x states)) accept_states with
    | Some invalid_state -> Error (invalid_state_msg "accept" invalid_state)
    | None -> Ok ()
  in
  let* (trans_strs, lines) = parse_counted_lines_array lines in
  let transs =
    List.filter_map parse_trans (Array.to_list trans_strs)
    |> Array.of_list
  in
  if Array.length transs <> Array.length trans_strs then
    Error "failed to parse transition (TODO: output invalid transitions)"
  else if lines <> [] then Error "input remaining"
  else Ok ({ albet; states; init_state; accept_states; transs; } : NFA.t)

end (* module Parser *)
