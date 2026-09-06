(* Non-Deterministic Finite Automata *)
(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module NFA = struct

open Util

type trans = {
    curr_state : string;
    albet_sym : string;
    next_state : string;
  }

type t = {
    albet : string array;
    states : string array;
    init_state : string;
    accept_states : string array;
    transs : trans array;
  }

type world = {
    curr_state : string;
    curr_input : string list;
}

let world_complete (nfa : t) (world : world) : bool =
  world.curr_input = []
  || (world.curr_state = "")
  || (Array.mem world.curr_state nfa.accept_states)

(* TODO: Return first incomplete world. Save scanning a second time to find next
 * world to step. *)
let all_worlds_complete (nfa : t) (worlds : world Queue.t) : bool =
  Queue.fold (fun acc w -> acc && world_complete nfa w) true worlds

let find_transs (nfa : t) state albet_sym : trans list =
  Array.fold_left
    (fun acc (x : trans) ->
      if x.curr_state = state && x.albet_sym = albet_sym
      then x :: acc
      else acc)
    [] nfa.transs

let step_world (nfa : t) (world : world) : world list =
  match world.curr_input with
  | [] -> [world]
  | input_next :: input_rest ->
     let transs = find_transs nfa world.curr_state input_next in
     let next_states = List.map (fun (x : trans) -> x.next_state) transs in
     List.map
       (fun x -> { curr_state = x; curr_input = input_rest })
       next_states

let eval (nfa : t) (input : string list) : bool =
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

let show_world ({ curr_state; curr_input } : world) : string =
  let list_str = Util.show_list_string curr_input in
  Printf.sprintf "{ curr_state = \"%s\"; curr_input = %s }" curr_state list_str

let show_trans ({ curr_state : string;
                  albet_sym : string;
                  next_state : string;
                } : trans) : string =
  List.fold_left
    (fun acc x -> acc ^ " " ^ x)
    curr_state
    [albet_sym; next_state]

let show ({ albet : string array;
            states : string array;
            init_state : string;
            accept_states : string array;
            transs : trans array;
          } : t) : string =
  let str_albet =
    (Printf.sprintf "Alphabet (%d):\n" (Array.length albet))
    ^ (Util.show_list_string_multiline (Array.to_list albet))
    ^ "\n\n" in

  let str_states =
    (Printf.sprintf "States (%d):\n" (Array.length states))
    ^ (Util.show_list_string_multiline (Array.to_list states))
    ^ "\n\n" in

  let str_init_state = Printf.sprintf "Initial State: \"%s\"\n\n" init_state in

  let str_accept_states =
    (Printf.sprintf "Accepting States (%d):\n" (Array.length accept_states))
    ^ (Util.show_list_string_multiline (Array.to_list accept_states))
    ^ "\n\n" in

  let trans_strs = Array.map show_trans transs in
  let str_trans_strs =
    (Printf.sprintf "Transitions (%d):\n" (Array.length transs))
    ^ (Util.show_list_string_multiline (Array.to_list trans_strs))
    ^ "\n" in

  str_albet ^ str_states ^ str_init_state ^ str_accept_states ^ str_trans_strs

end (* module NFA *)
