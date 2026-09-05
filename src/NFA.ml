(* Non-Deterministic Finite Automata *)
(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module NFA = struct

open DFA
open Util

type nfa_trans = DFA.dfa_trans
type nfa = DFA.dfa

type world = {
    curr_state : string;
    curr_input : string list;
}

let world_complete (nfa : nfa) (world : world) : bool =
  world.curr_input = []
  || (world.curr_state = "")
  || (Array.mem world.curr_state nfa.accept_states)

(* TODO: Return first incomplete world. Save scanning a second time to find next
 * world to step. *)
let all_worlds_complete (nfa : nfa) (worlds : world Queue.t) : bool =
  Queue.fold (fun acc w -> acc && world_complete nfa w) true worlds

let find_transs (nfa : nfa) state albet_sym : DFA.dfa_trans list =
  Array.fold_left
    (fun acc (x : DFA.dfa_trans) ->
      if x.curr_state = state && x.albet_sym = albet_sym
      then x :: acc
      else acc)
    [] nfa.transs

let step_world (nfa : nfa) (world : world) : world list =
  match world.curr_input with
  | [] -> [world]
  | input_next :: input_rest ->
     let transs = find_transs nfa world.curr_state input_next in
     let next_states = List.map (fun (x : nfa_trans) -> x.next_state) transs in
     List.map
       (fun x -> { curr_state = x; curr_input = input_rest })
       next_states

let show_world ({ curr_state; curr_input } : world) : string =
  let list_str = Util.show_list_string curr_input in
  Printf.sprintf "{ curr_state = \"%s\"; curr_input = %s }" curr_state list_str

let eval (nfa : nfa) (input : string list) : bool =
  let (worlds : world Queue.t) = Queue.create () in
  Queue.push { curr_state = nfa.init_state; curr_input = input } worlds;
  while not (all_worlds_complete nfa worlds) do
    while world_complete nfa (Queue.top worlds) do
      let prev = Queue.pop worlds in
      Queue.push prev worlds
    done;
    let curr_world = Queue.pop worlds in
    let next_worlds = step_world nfa curr_world in
    List.iter (fun w -> Queue.push w worlds) next_worlds
  done;
  Queue.fold
    (fun acc w -> acc || Array.mem w.curr_state nfa.accept_states)
    false worlds

end (* module NFA *)
