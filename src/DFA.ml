(* Deterministic Finite Automata *)
(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module DFA = struct

open Util
open NFA

type trans = NFA.trans
type t = NFA.t

let find_state (dfa : t) state : string option =
  if Array.mem state dfa.states then Some state else None

let find_trans (dfa : t) state albet_sym : trans option =
  Array.find_opt
    (fun (x : trans) -> x.curr_state = state && x.albet_sym = albet_sym)
    dfa.transs

let step (dfa : t) (state : string) (albet_sym : string)
    : (string, string) result =
  let (let*) = Result.bind in
  let* state =
    match find_state dfa state with
    | None -> Error "failed to find current state (unreachable)"
    | Some state -> Ok state
  in
  let* trans =
    match find_trans dfa state albet_sym with
    | None -> Error "failed to find transition (unreachable)"
    | Some trans -> Ok trans
  in
  Ok trans.next_state

let eval (dfa : t) (input : string list) : bool =
  let rec go curr_state curr_input =
    match curr_input with
    | [] -> Array.mem curr_state dfa.accept_states
    | head :: tail ->
       match step dfa curr_state head with
       | Error msg -> false
       | Ok next_state -> go next_state tail
  in go dfa.init_state input

let show (dfa : t) : string = NFA.show dfa

end (* module DFA *)
