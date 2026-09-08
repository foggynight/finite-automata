(* Non-Deterministic Finite Automata *)
(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

(* TODO: Add epsilon transitions. *)

module NFA = struct

open Util

type symbol = string
type state = string

type trans = {
    curr_state : state;
    symbol : symbol;
    next_state : state;
  }

type t = {
    albet : symbol array;
    states : state array;
    init_state : state;
    accept_states : state array;
    transs : trans array;
  }

type world = {
    curr_state : state;
    curr_input : symbol list;
  }

let world_complete (nfa : t) (world : world) : bool =
  world.curr_input = []
  || (world.curr_state = "")
  || (Array.mem world.curr_state nfa.accept_states)

(* TODO: Return first incomplete world. Save scanning a second time to find next
 * world to step. *)
let all_worlds_complete (nfa : t) (worlds : world Queue.t) : bool =
  Queue.fold (fun acc w -> acc && world_complete nfa w) true worlds

let find_transs (nfa : t) state sym : trans list =
  Array.fold_left
    (fun acc (x : trans) ->
      if x.curr_state = state && x.symbol = sym
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
  (* let (completed : world list) = ref [] in *)
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

let show_trans ({ curr_state; symbol; next_state } : trans) : string =
  (* let f = Util.show_string in *)
  let f = Fun.id in
  Printf.sprintf "\"%s %s %s\"" (f curr_state) (f symbol) (f next_state)

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
    ^ (Util.show_list_string_multiline
         ~fmt_fun:Fun.id (Array.to_list trans_strs))
    ^ "\n" in

  str_albet ^ str_states ^ str_init_state ^ str_accept_states ^ str_trans_strs

(* NFA Parser *****************************************************************)

(* TODO: Better error messages, show line number, improve root message, less
 * nesting. *)

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

let rec parse_trans (str : string) : trans option =
  match Util.string_split_whitespace str with
  | [curr_state; symbol; next_state] ->
     Some { curr_state; symbol ; next_state }
  | _ -> None

let invalid_state_msg state_type state =
  Printf.sprintf "invalid %s%sstate: %s"
    state_type (if state_type = "" then "" else " ") state

let find_invalid_transs (transs : trans array) albet states
    : (int * trans * string) list =
  let rec go = function
    | [] -> []
    | ((i, trans) : (int * trans)) :: tl ->
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

let parse (lines : string list) : (t, string) result =
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
            Printf.sprintf "[%d] %s (%s)" i (show_trans trans) reason
            :: go tl
       in
       let body_str =
         go invalid_transs
         |> Util.show_list_string_multiline ~fmt_fun:Fun.id
       in
       Error ("invalid transition(s):\n" ^ body_str)
  in

  if lines <> [] then Error "parse complete but input remaining"
  else Ok ({ albet; states; init_state; accept_states; transs; } : t)

end (* module NFA *)
