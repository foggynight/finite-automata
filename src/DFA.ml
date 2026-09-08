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

end (* module DFA *)
