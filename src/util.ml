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

let print_list_string (lst : string list) : unit =
  let rec go lst =
    match lst with
    | [] -> print_char ']'
    | head :: tail ->
       Printf.printf "; \"%s\"\n" head;
       go tail
  in print_string "[\n"; go lst

end
