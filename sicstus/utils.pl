/** <module> General utility predicates. */
:- module(utils, [writeln/1, pretty_print/1, intersection/3]).

% writeln(+Term)
writeln(Term) :-
    write(Term),
    nl.

% pretty_print(+List)
pretty_print([]).
pretty_print([Row | Rows]) :-
    writeln(Row),
    pretty_print(Rows).

% intersection(+List1, +List2, -Intersection)
intersection([], _, []).
intersection([H | T], List2, [H | Intersection]) :-
    member(H, List2), !,
    intersection(T, List2, Intersection).
intersection([_ | T], List2, Intersection) :-
    intersection(T, List2, Intersection).

