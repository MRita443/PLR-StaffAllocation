:- module(utils, [max_nonzero_diff/3, multiply_lists/3, intersection/3, sublist_from/3, selRandomVar/3, selRandomValue/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

% ############## Utilities ##############

% max_nonzero_diff(+X, +List, -MaxDiff)
% max diff between X and non-zero elements in List
max_nonzero_diff(_, [], 0).
max_nonzero_diff(X, [Y|Ys], MaxDiff) :-
    Diff #= abs(X - Y),
    Y #\= 0 #<=> NonZero,
    max_nonzero_diff(X, Ys, RestMaxDiff),
    Diff #> RestMaxDiff #<=> Larger,
    bool_and([NonZero, Larger], Keep),
    if_then_else(Keep, Diff, RestMaxDiff, MaxDiff).

% multiply_lists(+List1, +List2, -Result)
multiply_lists([], [], []).
multiply_lists([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z #= X * Y,
    multiply_lists(Xs, Ys, Zs).

% intersection(+List1, +List2, -Intersection)
intersection([], _, []).
intersection([H|T], L, [H|R]) :-
    member(H, L), !,
    intersection(T, L, R).
intersection([H|T], L, R) :-
    \+ member(H, L), !,
    intersection(T, L, R).

% sublist_from(+Idx, +List, -Sublist)
sublist_from(Idx, List, Sublist) :-
    Idx #> 0,
    PrefixLen #= Idx - 1,
    length(Prefix, PrefixLen),
    append(Prefix, Sublist, List).

% selRandomValue(+Var, +Rest, +BB0, -BB1)
selRandomValue(Var, Rest, BB0, BB1):-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    ( first_bound(BB0, BB1), Var #= Value ;
      later_bound(BB0, BB1), Var #\= Value ).