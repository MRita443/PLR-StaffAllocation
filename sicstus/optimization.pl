:-module(optimization, [calc_utility/5]).
:-use_module(utils).
:-use_module(data_utils).
:- use_module('../data_pl/staff').
:-use_module(library(clpfd)).

% calc_utility(+StaffToActivity, +ActivityToStaff, -SkillsAlign, -PrefAlign, -ExpDiversity)
calc_utility(StaffToActivity, ActivityToStaff, SkillsAlign, PrefAlign, ExpDiversity) :-
    build_preferences_matrix(PreferencesMatrix),
    build_skills_matrix(SkillsMatrix),
    findall(Exp, staff(_, Exp, _), ExpList),
    calc_utility_staff(StaffToActivity, PreferencesMatrix, SkillsMatrix, SkillsAlign, PrefAlign),
    calc_utility_activity(ActivityToStaff, ExpList, ExpDiversity).

% calc_utility_staff(+ActivityToStaff, +PreferencesMatrix, +SkillsMatrix, -SkillsAlign, -PrefAlign)
calc_utility_staff([], [], [], 0, 0).
calc_utility_staff([AllocationRow|RestAllocations], [PreferencesRow|RestPreferences], [SkillsRow|RestSkills], SkillsAlign, PrefAlign) :-
    scalar_product(PreferencesRow, AllocationRow, #=, PrefAlignRow),
    scalar_product(SkillsRow, AllocationRow, #=, SkillsAlignRow),
    calc_utility_staff(RestAllocations, RestPreferences, RestSkills, RestSkillsAlign, RestPrefAlign),
    SkillsAlign #= SkillsAlignRow + RestSkillsAlign,
    PrefAlign #= PrefAlignRow + RestPrefAlign.

% calc_utility_activity(+ActivityToStaff, +ExperienceList, -ExpDiversity) :
calc_utility_activity([], _, 0).
calc_utility_activity([AllocationRow|RestAllocations], ExpList, ExpDiversity) :-
    multiply_lists(ExpList, AllocationRow, ExperienceRow), % Zero out entries where staff is not allocated
    maximum(MaxExp, ExperienceRow),
    max_nonzero_diff(MaxExp, ExperienceRow, ExpDiversityRow), % Calculate diversity as the difference between max and min experience
    calc_utility_activity(RestAllocations, ExpList, RestExpDiversity),
    ExpDiversity #= ExpDiversityRow + RestExpDiversity.



