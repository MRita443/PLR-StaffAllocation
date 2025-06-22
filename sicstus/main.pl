:- use_module(constraints).
:- use_module(data_utils).
:- use_module(optimization).
:- use_module(utils).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(statistics)).

%post_constraints(-StaffToActivity, -ActivityToStaff, -FlatAllocation, -Utility, -SkillsAlign, -PrefAlign, -ExpDiversity)
post_constraints(StaffToActivity, ActivityToStaff, FlatAllocation, Utility, SkillsAlign, PrefAlign, ExpDiversity) :- 
    get_activity_num(NumActivities),
    get_staff_num(NumStaff),

    length(StaffToActivity, NumStaff),
    constraint_num_cols(StaffToActivity, NumActivities),

    append(StaffToActivity, FlatAllocation),
    domain(FlatAllocation, 0, 1),

    transpose(StaffToActivity, ActivityToStaff),

    ensure_available(StaffToActivity),
    ensure_no_overlap(StaffToActivity),
    ensure_min_staff(ActivityToStaff),

    calc_utility(StaffToActivity, ActivityToStaff, SkillsAlign, PrefAlign, ExpDiversity),
    Utility #= SkillsAlign + PrefAlign + ExpDiversity.

%allocate_statistics(+LabelingOptions)
allocate_statistics(LabelingOptions) :- 
    post_constraints(_, _, FlatAllocation, Utility, SkillsAlign, PrefAlign, ExpDiversity),

    append([maximize(Utility)], LabelingOptions, FinalLabelingOptions),

    statistics(runtime, _),
    labeling(FinalLabelingOptions, FlatAllocation),
    statistics(runtime, [_, Time|_]),

    fd_statistics,
    format('RESULT,labeling=~w,utility=~d,skills=~d,prefs=~d,expdiv=~d,time_ms=~d~n',
        [FinalLabelingOptions, Utility, SkillsAlign, PrefAlign, ExpDiversity, Time]).

%allocate(-StaffToActivity, -ActivityToStaff)
allocate(StaffToActivity, ActivityToStaff) :- 
    post_constraints(StaffToActivity, ActivityToStaff, FlatAllocation, Utility, SkillsAlign, PrefAlign, ExpDiversity),

    labeling([maximize(Utility)], FlatAllocation),
    
    write('Skills Alignment: '), write(SkillsAlign), nl,
    write('Preferences Alignment: '), write(PrefAlign), nl,
    write('Experience Diversity: '), write(ExpDiversity), nl,
    write('Total Utility: '), write(Utility), nl.

