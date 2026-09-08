(* Parser for finite automaton description files. *)
(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

(* NOTE: `lines` refers to a list of strings, each representing a single line of
 * text, with the terminating newline character removed. *)

(* TODO: Move this into NFA module, and add parse_DFA to DFA module. *)

(* TODO: Better error messages, show line number, improve root message, less
 * nesting. *)

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

let rec parse_trans (str : string) : NFA.trans option =
  match Util.string_split_whitespace str with
  | [curr_state; symbol; next_state] ->
     Some { curr_state; symbol ; next_state }
  | _ -> None

let invalid_state_msg state_type state =
  Printf.sprintf "invalid %s%sstate: %s"
    state_type (if state_type = "" then "" else " ") state

let find_invalid_transs (transs : NFA.trans array) albet states
    : (int * NFA.trans * string) list =
  let rec go = function
    | [] -> []
    | ((i, trans) : (int * NFA.trans)) :: tl ->
       let reason =
         if not (Array.mem trans.symbol albet) then
           ("symbol not in alphabet " ^ Util.show_string trans.symbol)
         else if not (Array.mem trans.curr_state states) then
           ("invalid current state " ^ Util.show_string trans.curr_state)
         else if not (Array.mem trans.next_state states) then
           ("invalid next state " ^ Util.show_string trans.next_state)
         else ""
       in
       if reason = ""
       then go tl
       else (i, trans, reason) :: go tl
  in go (List.mapi (fun i x -> (i, x)) (Array.to_list transs))

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
  let* transs =
    match Array.map parse_trans trans_strs
          |> Util.array_all_some_res
    with
    | Ok transs -> Ok transs
    | Error indices ->
       let err_strs =
         List.map
           (fun i -> Printf.sprintf "[%d]: %s"
                       i (Util.show_string trans_strs.(i)))
           indices
       in
       Error ("failed to parse transition(s):\n"
              ^ Util.show_list_string_multiline ~fmt_fun:Fun.id err_strs)
  in
  let* () =
    match find_invalid_transs transs albet states with
    | [] -> Ok ()
    | invalid_transs ->
       let rec go = function
         | [] -> []
         | (i, trans, reason) :: tl ->
            Printf.sprintf "[%d] %s (%s)" i (NFA.show_trans trans) reason
            :: go tl
       in
       let body_str =
         go invalid_transs
         |> Util.show_list_string_multiline ~fmt_fun:Fun.id
       in
       Error ("invalid transition(s):\n" ^ body_str)
  in

  if lines <> [] then Error "parse complete but input remaining"
  else Ok ({ albet; states; init_state; accept_states; transs; } : NFA.t)

let find_duplicate_transs (transs : NFA.trans array) : NFA.trans list =
  let dups = ref [] in
  for i = 0 to (Array.length transs) - 1 do
    let trans = transs.(i) in
    if ((Util.array_find_offset trans (i+1) transs)
        && not (List.mem trans !dups))
    then dups := trans :: !dups;
  done;
  List.rev !dups

let parse_DFA (lines : string list) : (DFA.t, string) result =
  let (let*) = Result.bind in
  let* (nfa : NFA.t) = parse_NFA lines in
  match find_duplicate_transs nfa.transs with
  | [] -> Ok nfa
  | ds -> let body_str = Util.show_list_string_multiline
                           ~fmt_fun:Fun.id (List.map NFA.show_trans ds)
          in Error ("duplicate transitions:\n" ^ body_str)

end (* module Parser *)
