(* Copyright (C) 2026 Robert Coffey *)
(* Released under the MIT license. *)

module Util = struct

let error x = Error x
let ok x = Ok x

let uncons = function
  | [] -> None
  | head :: tail -> Some (head, tail)

let array_find_offset_p
      (p : 'a -> 'a -> bool)
      (target : 'a)
      (offset : int)
      (arr : 'a array)
    : bool =
  let rec loop i =
    if i >= Array.length arr then
      false
    else
      if p arr.(i) target then
        true
      else
        loop (i+1)
  in loop offset

(* Converts array of options to array of Some-d values. If array contains any
 * None-s, fails and returns list containing indices of all None-s. *)
let array_all_some_res (arr : 'a option array) : ('a array, int list) result =
  if Array.mem None arr then
    Array.to_list arr
    |> List.filter_mapi (fun i x -> if x = None then Some i else None)
    |> error
  else
    Array.map
      (function
       | None -> failwith "unreachable"
       | Some x -> x)
      arr
    |> ok

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
     let body =
       List.fold_left
         (fun acc x -> acc ^ "; " ^ show_string x)
         (show_string head)
         tail
     in
     "[" ^ body ^ "]"

let show_list_string_multiline
      ?(fmt_fun = show_string) (lst : string list) : string =
  let rec go lst =
    match lst with
    | [] -> "]"
    | head :: tail ->
       (Printf.sprintf "; %s\n" (fmt_fun head)) ^ go tail
  in "[\n" ^ go lst

let newline () : unit = print_char '\n'

end (* module Util *)
