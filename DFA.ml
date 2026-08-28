(* DFA.ml - Deterministic Finite Automata in OCaml. *)
(* Copyright (C) 2026 Robert Coffey *)

let explode (str : string) : char list =
  List.init (String.length str) (String.get str)

let implode (lst : char list) : string =
  String.of_seq (List.to_seq lst)

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

let dfa_step (dfa : dfa) (state : int) (c : char) : int =
  match find_state dfa.states state with
  | None -> 0
  | Some st ->
     match find_trans st.transs c with
     | None -> 0
     | Some trans -> trans.next_state

let rec dfa_eval (dfa : dfa) (state : int) (cs : char list) : bool =
  Printf.printf "State = %d; Input = %s\n" state (implode cs);
  match cs with
  | [] -> List.mem state dfa.accepts
  | head :: tail ->
     let next_state = dfa_step dfa state head in
     dfa_eval dfa next_state tail

let main (str : string) =
  let dfa = {
      states = [
        { id = 1; transs = [{ c = 'A'; next_state = 2 }] };
        { id = 2; transs = [{ c = 'B'; next_state = 3 }] };
        { id = 3; transs = [{ c = 'C'; next_state = 4 }] };
      ];
      accepts = [4]
    }
  in
  let result = dfa_eval dfa 1 (explode str) in
  Printf.printf "Result: %b\n" result
