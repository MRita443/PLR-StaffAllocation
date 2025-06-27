:- module(test_data, [staff/3, activity/5, available/2, preference/3]).

% ############## Test Data ##############

% staff(+name, +experience, +skills)
% experience is an integer value [1, 5], skills is a list of skills
staff(alice, 4, [logistics]).
staff(john, 2, [logistics]).
staff(jane, 3, [program, cocktails]).

% activity(slug, min_staff, start_time, duration, skills)
activity(coffee_break, 2, 10, 2, [logistics, cocktails]).
activity(workshop_1, 1, 9, 2, [program]).

% available(staffName, activitySlug)
available(alice, coffee_break).
available(alice, workshop_1).
available(john, coffee_break).
available(jane, workshop_1).

% preference(staffName, activitySlug, preference) -> [0, 5] integer values
% Omitted preferences defaults to 3
preference(alice, workshop_1, 4).
preference(john, coffee_break, 1).
preference(john, workshop_1, 0).
preference(jane, coffee_break, 2).
preference(jane, workshop_1, 5).
