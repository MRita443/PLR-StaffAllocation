:- use_module(constraints).
:- use_module(data_utils).
:- use_module(optimization).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

allocate(StaffToActivity, ActivityToStaff, Utility):- 
    get_activity_num(NumActivities),
    get_staff_num(NumStaff),

    length(StaffToActivity, NumStaff),
    constraint_num_cols(StaffToActivity, NumActivities),

    append(StaffToActivity, FlatAllocation),
    domain(FlatAllocation, 0, 1),

    transpose(StaffToActivity, ActivityToStaff),

    ensure_available(StaffToActivity),
    ensure_no_overlap(StaffToActivity),

    calc_utility(StaffToActivity, ActivityToStaff, SkillsAlign, PrefAlign, ExpDiversity),

    Utility #= SkillsAlign + PrefAlign + ExpDiversity,

    labeling([maximize(Utility)], FlatAllocation),
    
    write('Skills Alignment: '), write(SkillsAlign), nl,
    write('Preferences Alignment: '), write(PrefAlign), nl,
    write('Experience Diversity: '), write(ExpDiversity), nl,
    write('Total Utility: '), write(Utility), nl.