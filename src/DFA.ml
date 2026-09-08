(* Deterministic Finite Automata *)
(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module DFA = struct

open Util
open NFA

type symbol = NFA.symbol
type state = NFA.state
type trans = NFA.trans
type t = NFA.t

let find_state (dfa : t) (st : state) : state option =
  if Array.mem st dfa.states then Some st else None

let find_trans (dfa : t) (st : state) (symbol : symbol) : trans option =
  Array.find_opt
    (fun (x : trans) -> x.curr_state = st && x.symbol = symbol)
    dfa.transs

let step (dfa : t) (st : string) (symbol : string)
    : (string, string) result =
  let (let*) = Result.bind in
  let* st =
    match find_state dfa st with
    | None -> Error "DFA.step: failed to find current state"
    | Some st -> Ok st
  in
  let* trans =
    match find_trans dfa st symbol with
    | None -> Error "DFA.step: failed to find transition"
    | Some trans -> Ok trans
  in
  Ok trans.next_state

let eval (dfa : t) (input : string list) : bool =
  let rec go curr_st curr_input =
    match curr_input with
    | [] -> Array.mem curr_st dfa.accept_states
    | head :: tail ->
       match step dfa curr_st head with
       | Error msg -> false
       | Ok next_st -> go next_st tail
  in go dfa.init_state input

let show (dfa : t) : string = NFA.show dfa

(* DFA Parser *****************************************************************)

let multi_trans_p (x : NFA.trans) (y : trans) =
  x.curr_state = y.curr_state && x.symbol = y.symbol

let find_multi_transs (transs : trans array) : trans list =
  let transs_list = Array.to_list transs in
  let multi_transs = ref [] in
  for i = 0 to (Array.length transs) - 1 do
    let trans = transs.(i) in
    if not (List.exists (multi_trans_p trans) !multi_transs)
       && Util.array_find_offset_p multi_trans_p trans (i+1) transs
    then
      let new_transs = List.filter (multi_trans_p trans) transs_list in
      multi_transs := new_transs @ !multi_transs;
  done;
  !multi_transs

let parse (lines : string list) : (t, string) result =
  let (let*) = Result.bind in
  let* (nfa : NFA.t) = NFA.parse lines in
  match find_multi_transs nfa.transs with
  | [] -> Ok nfa
  | multi_trans ->
     let body_str = Util.show_list_string_multiline
                      ~fmt_fun:Fun.id (List.map NFA.show_trans multi_trans)
     in Error ("multiple transitions for state and input:\n" ^ body_str)

end (* module DFA *)
