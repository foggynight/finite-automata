# finite-automata

Finite automata simulator.


## Build

Written in OCaml, executable built using the native compiler (no dune).

Run `make` to create the `finite-automata` native executable.


## DFA Description

DFAs are defined using the "DFA Description File Format" shown here:
+<https://student.cs.uwaterloo.ca/~cs241/dfa/DFAfileformat.html>

Quoting the file format description from the webpage above:

> A .dfa file represents a DFA by describing, in order, the alphabet, the
> states, the initial state, the final states, and the transitions. The format
> for each is as follows:
>
> - Alphabet: A line containing n, the number of symbols in the alphabet,
>   followed by n lines, each containing an alphabet symbol. Each alphabet
>   symbol must be a string of printable characters, not including whitespace.
>
> - States: A line containing m, the number of states, followed by m lines, each
>   containing the name of a state. Each state name must be a string of letters
>   and/or digits, but no spaces or other characters.
>
> - Initial state: A line containing the name of the initial state for the DFA.
>   The initial state must be one of the states listed in States above.
>
> - Accepting states: A line containing q, the number of accepting states,
>   followed by q lines, each containing the name of a accepting state. Each
>   accepting state must be one of the states listed in States above.
>
> - Transitions: A line containing r, the number of (non-error) transitions in
>   the transition function δ, followed by r lines, each containing three
>   strings st1, sym, st2 separated by spaces. Each such line indicates a
>   transition from state st1 on symbol sym to state st2; that is,
>   δ(st1,sym)=st2. st1 and st2 must be listed in States and sym must be listed
>   in Alphabet, above. It is not necessary to specify transitions on every
>   state and letter pair; omitted transitions are assumed to go to an implicit
>   error state.

Additionally, empty lines and comments are ignored. comments are lines that
begin with "--".
