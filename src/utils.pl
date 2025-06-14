:- module(utils, [pairs_keys_values/3, forall/2]).

% pairs_keys_values(+Pairs, +Keys, +Values)
pairs_keys_values([], [], []).
pairs_keys_values([K-V | Pairs], [K | Keys], [V | Values]) :-
    pairs_keys_values(Pairs, Keys, Values).
