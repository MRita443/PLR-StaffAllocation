:-module(constraints, [constraint_num_cols/2, ensure_available/1, ensure_no_overlap/1]).

:-use_module(data_utils).
:-use_module(utils).
:-use_module(library(clpfd)).
:-use_module(library(lists)).

% ############## Constraints ##############

% constraint_num_cols(+Matrix, +Num)
constraint_num_cols([], _).
constraint_num_cols([Row|Rest], Num) :-
    length(Row, Num),
    constraint_num_cols(Rest, Num).

% ensure_available(+StaffToActivity)
ensure_available(Allocation) :-
    build_availability_matrix(AvailabilityMatrix),
    ensure_available_rows(Allocation, AvailabilityMatrix).

% ensure_available_rows(+Allocation, +AvailabilityMatrix)
ensure_available_rows([], []).
ensure_available_rows([AllocationRow|RestAllocations], [AvailabilityRow|RestAvailabilities]) :-
    ensure_available_row(AllocationRow, AvailabilityRow),
    ensure_available_rows(RestAllocations, RestAvailabilities).
    
% ensure_available_row(+AllocationRow, +AvailabilityRow)
ensure_available_row([], []).
ensure_available_row([Allocation|RestAllocations], [Availability|RestAvailabilities]) :-
    Allocation #=< Availability,
    ensure_available_row(RestAllocations, RestAvailabilities).

% ensure_no_overlap(+Allocation)
ensure_no_overlap(Allocation) :-
    build_overlap_matrix(OverlapMatrix),
    ensure_no_overlap_rows(Allocation, OverlapMatrix).

% ensure_no_overlap_rows(+Allocation, +OverlapMatrix)
ensure_no_overlap_rows([], _).
ensure_no_overlap_rows([AllocationRow|RestAllocations], OverlapMatrix) :-    
    ensure_no_overlap_row(AllocationRow, 1, OverlapMatrix),
    ensure_no_overlap_rows(RestAllocations, OverlapMatrix).

% ensure_no_overlap_row(+AllocationRow, +Idx, +OverlapRow)
ensure_no_overlap_row([], _, _).    
ensure_no_overlap_row([CurrAllocation|RestAllocations], Idx, OverlapMatrix) :-
    nth1(Idx, OverlapMatrix, OverlapRow), % Get row relative to current activity
    sublist_from(Idx, OverlapRow, CurrOverlap), % No need to pass previously checked cols
    ensure_no_overlap_w(CurrAllocation, [CurrAllocation|RestAllocations], CurrOverlap),
    Idx1 is Idx + 1,
    ensure_no_overlap_row(RestAllocations, Idx1, OverlapMatrix).

% ensure_no_overlap_w(+CurrAllocation, +AllocationRow, +OverlapRow)
% Check overlap for current allocation with the rest of the allocations for that member
ensure_no_overlap_w(_, [], []).
ensure_no_overlap_w(CurrAllocation, [OtherAllocation|RestAllocations], [Overlap|RestOverlaps]) :-
    CurrAllocation #= 1 #<=> A,
    Overlap        #= 1 #<=> B,
    OtherAllocation #= 1 #<=> C,
    OverlapConflict #<=> (A #/\ B #/\ C),
    OverlapConflict #= 0,
    ensure_no_overlap_w(CurrAllocation, RestAllocations, RestOverlaps).









