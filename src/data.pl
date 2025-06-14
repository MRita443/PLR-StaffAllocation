:- module(data, [staff/3, activity/5, preference/3, available/2]).

% ############## Test Data ##############

% staff(+name, +experience, +skills)
% experience is an integer value [1, 5], skills is a list of skills
staff(alice, 4, [logistics]).
staff(john, 2, [logistics]).
staff(jane, 3, [program, cocktails]).

% activity(slug, min_staff, start_time, duration, skills)
activity(coffee_break, 2, 10.5, 2, [logistics, cocktails]).
activity(workshop_1, 1, 9, 2, [program]).

% available(staffName, activitySlug)
available(alice, coffee_break).
available(alice, workshop_1).
available(john, coffee_break).
available(jane, workshop_1).

% preference(staffName, activitySlug, preference) -> [0, 5] integer values
% Ommitted preferences defaults to 3
preference(alice, coffee_break, 3).
preference(alice, workshop_1, 4).
preference(john, coffee_break, 1).
preference(john, workshop_1, 0).
preference(jane, coffee_break, 2).
preference(jane, workshop_1, 5).

% availability([staff x activity]) -> 0 unavailable, 1 available
/* availability([
    [1, 1],  % Staff 1 is available for both activities
    [1, 0],  % Staff 2 is only available for activity 1
    [0, 1]   % Staff 3 is only available for activity 2
]). */

% preferences([staff x activity]) -> [0, 5] integer values for preference
% preferences([
%     [3, 4],  
%     [1, 0],  
%     [2, 5]
% ]).