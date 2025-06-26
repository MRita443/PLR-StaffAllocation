include "globals.mzn";

% ==============================================================================
% Sets
% ==============================================================================
enum STAFF;
enum ACTIVITIES;
enum SKILLS;

% ==============================================================================
% Parameters
% =========================kk=====================================================
% available[s, a]: true if staff member 's' is available for activity 'a'
array[STAFF, ACTIVITIES] of bool: available;

% minStaff[a]: The minimum number of staff required for activity 'a'
array[ACTIVITIES] of int: minStaff;

% preference[s, a]: The preference value (float) of staff 's' for activity 'a'
array[STAFF, ACTIVITIES] of float: preference;

% staffSkills[s, k]: true if staff member 's' has skill 'k'
array[STAFF, SKILLS] of bool: staffSkills;

% activitySkills[a, k]: true if activity 'a' requires skill 'k'
array[ACTIVITIES, SKILLS] of bool: activitySkills;

% experience[s]: The experience level (1-5) of staff member 's'
array[STAFF] of int: experience;

% start[a]: The start time block for activity 'a'
array[ACTIVITIES] of int: start;

% duration[a]: The duration in time blocks for activity 'a'
array[ACTIVITIES] of int: duration;


% ==============================================================================
% Decision Variables
% ==============================================================================
% x[s, a]: true if staff member 's' is assigned to activity 'a'
array[STAFF, ACTIVITIES] of var bool: x;


% ==============================================================================
% Hard Constraints
% ==============================================================================
% An assigned staff member must be available for the activity.
constraint forall(s in STAFF, a in ACTIVITIES) (
  x[s, a] -> available[s, a]
);

% Each activity must meet its minimum staff requirement.
constraint forall(a in ACTIVITIES) (
  sum(s in STAFF) (x[s, a]) >= minStaff[a]
);

% A staff member cannot be assigned to two overlapping activities.
% This is enforced using the cumulative global constraint. Each staff member is a
% resource with a capacity of 1.
constraint forall(s in STAFF) (
  let {
    % Create local, integer-indexed arrays to avoid type confusion errors.
    array[1..card(ACTIVITIES)] of int: s_starts = [start[a] | a in ACTIVITIES];
    array[1..card(ACTIVITIES)] of int: s_durations = [duration[a] | a in ACTIVITIES];
    array[1..card(ACTIVITIES)] of var 0..1: s_reqs = [bool2int(x[s,a]) | a in ACTIVITIES];
  } in
  cumulative(
    s_starts,   % array of start times
    s_durations,% array of durations
    s_reqs,     % resource usage for staff 's'
    1           % resource capacity for staff 's'
  )
);


% ==============================================================================
% Soft Constraints (Objective Components)
% ==============================================================================
% u1: Maximize the alignment of skills between staff and activities.
var int: u1 = sum(s in STAFF, a in ACTIVITIES, k in SKILLS where x[s,a]) (
    bool2int(staffSkills[s,k] /\ activitySkills[a,k])
);

% u2: Maximize the sum of preferences for assignments.
var float: u2 = sum(s in STAFF, a in ACTIVITIES) (x[s, a] * preference[s, a]);

% u3: Maximize the experience diversity within each activity.
% This is the sum of (max_experience - min_experience) for each activity.
var int: u3 = sum(a in ACTIVITIES)(
    (max(s in STAFF where x[s, a])(experience[s])) -
    (min(s in STAFF where x[s, a])(experience[s]))
);

% u4: The total number of assignments made. We want to minimize this.
var int: u4 = sum(s in STAFF, a in ACTIVITIES) (bool2int(x[s, a]));

% ==============================================================================
% Objective Function
% ==============================================================================
float: w1;
float: w2;
float: w3;
float: w4;

solve :: int_search(
    x,                % The array of variables to search on
    first_fail,       % Variable selection heuristic
    indomain_max,     % Value selection heuristic
    complete          % Specifies a complete search
)
maximize w1 * u1 + w2 * u2 + w3 * u3 - w4 * u4;
