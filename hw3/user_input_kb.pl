is_property(X, Y) :- property(X, Y).
is_property(X, Y) :- property(X, Z), is_property(Z, Y).

run :-
    write("The following inputs are valid: "), nl,
    write("X is a Y."), nl,
    write("X is an Y."), nl,
    write("Is X a Y?"), nl,
    write("Is X an Y?"), nl,
    parse.

remove_char(S,C,X) :-
    atom_concat(L,R,S), atom_concat(C,W,R), atom_concat(L,W,X).

parse :-
    write("Enter another input."), nl,
    write("Wrap your inputs with ''."), nl,
    read(Input), nl,
    split_string(Input, " ", "", Split),
    length(Split, Length),
    % X is a/n Y. A X is a/n Y. Is X a Y?
    ( Length \== 4 -> ( Length \== 5 -> write("Length must be 4 or 5\n"), false; true); true),
    last(Split, Last), atom_string(AtomLast, Last), sub_atom(AtomLast, _, 1, 0, LastChar),
    ( LastChar == ? -> check(Split), true ; ( LastChar == '.' -> add(Split), true ; write("Expected ? or . Got "), write(LastChar), nl, false)).

check(Input) :-
    nth0(0, Input, Is), nth0(2, Input, A),
    ( Is = "Is" -> (A = "a" -> true; (A = "an" -> true; write("Expected a or an, got "), write(A), nl, false); write("Expected a or an, got "), write(A), nl, false); write("Expected Is, got "), write(Is), nl, false),
    nth0(1, Input, Name), nth0(3, Input, Attribute), remove_char(Attribute, '?', AtomAttr), atom_string(AtomAttr, FixedAttr), atom_string(Name, NameString),
    (is_property(NameString, _) -> true; write("Unknown"), nl, run), % Checks if name in dictionary
    (is_property(NameString, FixedAttr) -> write("Yes"); write("No")), nl, parse.

add(Input) :-
    nth0(0, Input, First),
    ( First = "A", nth0(1, Input, Name), nth0(2, Input, Is), nth0(3, Input, A), nth0(4, Input, Attribute)
                 ; nth0(0, Input, Name), nth0(1, Input, Is), nth0(2, Input, A), nth0(3, Input, Attribute)),
    ( Is = "is" -> (A = "a" -> true; write("Expected a, got "), write(A), nl, false); write("Expected is, got "), write(Is), nl, false),
    remove_char(Attribute, '.', AtomAttr), atom_string(AtomAttr, FixedAttr), atom_string(Name, NameString), assert(property(NameString, FixedAttr)), write("OK"), nl, parse.