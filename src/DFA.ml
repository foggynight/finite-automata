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
  List.find_opt (fun x -> x.id == state) lst

let find_trans (lst : dfa_trans list) (c : char) : dfa_trans option =
  List.find_opt (fun x -> x.c == c) lst

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

let is_comment (line : string) : bool =
  let res = (String.length line > 0) && (String.starts_with ~prefix:"--" line) in
  if res then Printf.printf "Comment: %s\n" line;
  res

let parse_alphabet (lines : string list)
    : (int * string list * string list) option =
  let* (head, tail) = Util.uncons lines in
  let alpha_size = int_of_string head in
  match tail with
  | [] -> None
  | alpha_lines ->
     Some (alpha_size,
           List.take alpha_size alpha_lines,
           List.drop alpha_size alpha_lines)

let print_alphabet (alphabet : string list) : unit =
  let rec print lst =
    match lst with
    | [] -> print_char ']'
    | head :: tail ->
       Printf.printf "%s; " head;
       print tail
  in print_char '['; print alphabet

let parse_states (lines : string list)
    : (int * dfa_state list * string list) option =
  None

let print_states (states : dfa_state list) : unit =
  ()

let parse_lines (lines1 : string list) : dfa option =
  let lines2 =
    List.filter
      (fun x -> String.length x > 0 && not (is_comment x))
      lines1
  in

  let* (alphabet_size, dfa_alphabet, lines4) = parse_alphabet lines2 in
  Printf.printf "Alphabet Size: %d\n" alphabet_size;
  print_alphabet dfa_alphabet; print_char '\n';

  let* (state_count, dfa_states, lines5) = parse_states lines4 in
  Printf.printf "State Count: %d\n" state_count;
  print_states dfa_states; print_char '\n';

  if lines5 == []
  then Some { states = []; accepts = [] }
  else None

end
