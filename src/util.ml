(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module Util = struct

let uncons = function
  | [] -> None
  | head :: tail -> Some (head, tail)

let explode (str : string) : char list =
  List.init (String.length str) (String.get str)

let implode (lst : char list) : string =
  String.of_seq (List.to_seq lst)

let explode_to_strings (str : string) : string list =
  List.init (String.length str) (fun i -> String.make 1 str.[i])

let string_space_only (str : string) : bool =
  String.for_all
    (function
     | ' ' | '\t' | '\n' | '\r' -> true
     | _ -> false)
    str

let string_split_whitespace (str : string) : string list =
  let split = String.split_on_char ' ' str in
  List.filter (fun x -> String.length x > 0) split

let show_string str = Printf.sprintf "\"%s\"" str

let show_list_string = function
  | [] -> "[]"
  | head :: tail ->
     let body = List.fold_left (fun acc x -> acc ^ "; " ^ x) head tail in
     "[" ^ body ^ "]"

let show_list_string_multiline (lst : string list) : string =
  let rec go lst =
    match lst with
    | [] -> "]"
    | head :: tail ->
       (Printf.sprintf "; \"%s\"\n" head) ^ go tail
  in "[\n" ^ go lst

let newline () : unit = print_char '\n'

end (* module Util *)
