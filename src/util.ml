module Util = struct

let uncons = function
  | [] -> None
  | head :: tail -> Some (head, tail)

let explode (str : string) : char list =
  List.init (String.length str) (String.get str)

let implode (lst : char list) : string =
  String.of_seq (List.to_seq lst)

let print_list_str (lst : string list) : unit =
  let rec print lst =
    match lst with
    | [] -> print_char ']'
    | head :: tail ->
       Printf.printf "\"%s\"\n" head;
       print tail
  in print_string "[\n"; print lst

end
