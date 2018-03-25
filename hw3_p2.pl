remove_char(S,C,X) :-
    atom_concat(L,R,S), atom_concat(C,W,R), atom_concat(L,W,X), atom_string(X, X).

split_at_substring(Query, Split, Result) :-
    sub_string(Query, Before, _, After, Split),
    sub_string(Query, 0, Before, _, BeforeString),
    sub_string(Query, _, After, 0, AfterString),
    append([BeforeString], [AfterString], Result).

run :-
    write("What is your problem?"), nl,
    write("Please format your input with ''. and no apostrophes."), nl,
    read(Input), nl, respond(Input).

continue :-
    write("Please format your input with ''. and no apostrophes."), nl,
    read(Input), nl, respond(Input).

respond(Query) :-
    ( split_at_substring(Query, " is ", Result) -> write("What else do you regard as "),
        nth0(1, Result, Thing), remove_char(Thing, '.', Fixed), write(Fixed), write("?"), nl, continue; true ),
    ( split_at_substring(Query, "mother", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "father", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "brother", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "sister", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "husband", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "wife", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "son", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "daughter", _) -> write("Tell me more about your family."), nl, continue; true ),
    ( split_at_substring(Query, "Why should ", Result) -> write("Why should you what?"), nl, continue; true ),
    ( split_at_substring(Query, "happy", _) -> write("Feeling happy is good!"), nl, continue; true ),
    ( split_at_substring(Query, "sad", _) -> write("Why do you feel sad?"), nl, continue; true ),
    ( split_at_substring(Query, "angry", _) -> write("Why do you feel angry?"), nl, continue; true ),
    ( split_at_substring(Query, "love", _) -> write("Love can be a strong feeling."), nl, false; true),
    ( split_at_substring(Query, "hate", _) -> write("Hate can be a strong feeling."), nl, false; true),
    ( split_at_substring(Query, "job", _) -> write("Tell me more about your job."), nl, false; true),
    ( split_at_substring(Query, "work", _) -> write("Tell me more about your job."), nl, false; true),
    ( split_at_substring(Query, "school", _) -> write("Tell me more about your school."), nl, false; true),
    ( split_at_substring(Query, "college", _) -> write("Tell me more about your school."), nl, false; true),
    ( split_at_substring(Query, "university", _) -> write("Tell me more about your school."), nl, false; true),
    ( split_at_substring(Query, "uni", _) -> write("Tell me more about your school."), nl, false; true),
    ( split_at_substring(Query, "bye", _) -> write("Goodbye."), nl, false; true),
    ( split_at_substring(Query, "Bye", _) -> write("Goodbye."), nl, false; true),
    ( split_at_substring(Query, "goodbye", _) -> write("Goodbye."), nl, false; true),
    ( split_at_substring(Query, "Goodbye", _) -> write("Goodbye."), nl, false; true),
    ( split_at_substring(Query, "done", _) -> write("Goodbye."), nl, false; true),
    write("I see, please continue."), nl, continue.
