open DFA
open Util

let main () =
  let (str : string) = "ABC"
  and (dfa : DFA.dfa) = {
      states = [
        { id = 1; transs = [{ c = 'A'; next_state = 2 }] };
        { id = 2; transs = [{ c = 'B'; next_state = 3 }] };
        { id = 3; transs = [{ c = 'C'; next_state = 4 }] };
      ];
      accepts = [4]
    }
  in
  let result = DFA.dfa_eval dfa 1 (Util.explode str) in
  Printf.printf "Result: %b\n" result

let () = main ()
