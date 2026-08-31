# finite-automata


## Build

Written in OCaml, executable built using the native compiler (no dune).

Running `make` will create the `finite-automata` native executable.


## DFA Description

A DFA is defined using a DFA description file, which is a text file in the
following format:

- Empty lines, and lines composed of only whitespace, are ignored.
//- Comments are lines beginning with the string 'c'. Comment lines are ignored.
//
//The following sections must be in order.
//
//- Section 1: Alphabet: A line containing `n`, the number of symbols in the
//  alphabet. DFAs refer to alphabet symbols by 1-based index, with the "0" symbol
//  being reserved as an invalid symbol. The mapping of index to alphabet is to be
//  handled external to this program.
//
//- Section 2: States: A line containing `m`, the number of states in the DFA.
//  DFAs refer to states by 1-based index, with the "0" state being reserved as an
//  invalid (dead) state. The mapping of index to alphabet is to be handled
//  external to this program.
//
//- Section 3: Initial State: A line containing the initial state for the DFA. The
//  initial state is represented by the 1-based index of the state, or the dead
//  state "0".
//
//- Section 4: Accepting States: A line containing a list of the 1-based indices
//  of the accepting states, terminated by a "0" (this does make the dead state an
//  accepting state). e.g. "2 3 5 0" implies the accepting states are [2, 3, 5].
//
//- Section 5: Transitions: A line containing `q`, the number of transitions in
//  the transition function of the DFA. Followed by `q` lines, each containing
//  three numbers separated by whitespace: st1, sym, st2. These numbers represent ...
//
//```
//c This is an example DFA description file. Building a DFA for the regex: ABC
//
//c Alphabet: For the alphabet { A, B, C } we have 3 symbols, thus...
//3
//
//c States:
//```
//
//More examples of DFA description files are provided in the `examples` directory.

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
