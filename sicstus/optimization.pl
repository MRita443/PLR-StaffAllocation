:-module(optimization, [calc_utility/5]).
:-use_module(utils).
:-use_module(data_utils).
:-use_module('../data_pl/staff').
:-use_module(library(clpfd)).
:-use_module(library(lists)).

% calc_utility(+StaffToActivity, +ActivityToStaff, -SkillsUtility, -PreferenceUtility, -ExpDiversity)
calc_utility(StaffToActivity, ActivityToStaff, SkillsUtility, PreferenceUtility, ExpDiversity) :-
    build_preferences_matrix(PreferencesMatrix),
    build_skills_matrix(SkillsMatrix),
    findall(Exp, staff(_, Exp, _), ExpList),
    calc_utility_staff(StaffToActivity, PreferencesMatrix, SkillsMatrix, SkillsUtility, PreferenceUtility),
    calc_utility_activity(ActivityToStaff, ExpList, ExpDiversity).

% calc_utility_staff(+StaffToActivity, +PreferencesMatrix, +SkillsMatrix, -SkillsUtility, -PreferenceUtility)
calc_utility_staff(StaffToActivity, PreferencesMatrix, SkillsMatrix, SkillsUtility, PreferenceUtility) :-
    append(StaffToActivity, FlatAllocations),
    append(SkillsMatrix, FlatSkillsMatrix),
    append(PreferencesMatrix, FlatPreferenceMatrix),
    scalar_product(FlatSkillsMatrix, FlatAllocations, #=, SkillsUtility),
    scalar_product(FlatPreferenceMatrix, FlatAllocations, #=, PreferenceUtility).

% calc_utility_activity(+ActivityToStaff, +ExperienceList, -ExpDiversity) :
calc_utility_activity([], _, 0).
calc_utility_activity([AllocationRow|RestAllocations], ExpList, ExpDiversity) :-
    multiply_lists(ExpList, AllocationRow, ExperienceRow), % Zero out entries where staff is not allocated
    maximum(MaxExp, ExperienceRow),
    max_nonzero_diff(MaxExp, ExperienceRow, ExpDiversityRow), % Calculate diversity as the difference between max and min experience
    calc_utility_activity(RestAllocations, ExpList, RestExpDiversity),
    ExpDiversity #= ExpDiversityRow + RestExpDiversity.



