% Sets
enum STAFF;
enum ACTIVITIES;
enum DEPARTMENTS;

% Parameters
array[STAFF, ACTIVITIES] of bool: available;
array[ACTIVITIES] of int: minStaff;
array[STAFF, ACTIVITIES] of float: preference;
array[STAFF] of DEPARTMENTS: staffDept;
array[ACTIVITIES] of DEPARTMENTS: activityDept;
array[STAFF] of bool: experience;
array[ACTIVITIES] of int: start;
array[ACTIVITIES] of int: duration;

% Decision Variables 
array[STAFF, ACTIVITIES] of var bool: x;

% Hard Constraints 
constraint forall(s in STAFF, a in ACTIVITIES) ( 
  x[s, a] -> available[s, a] 
);

constraint forall(a in ACTIVITIES) (
  sum(s in STAFF) (x[s, a]) >= minStaff[a]
);

constraint forall(s in STAFF, a1, a2 in ACTIVITIES where a1 < a2) (
  if (start[a1] < start[a2] + duration[a2] /\ start[a2] < start[a1] + duration[a1]) then
    x[s, a1] + x[s, a2] <= 1
  endif
);

% Soft Constraints 
var float: u1 = sum(s in STAFF, a in ACTIVITIES where staffDept[s] == activityDept[a]) (x[s, a]);

var float: u2 = sum(s in STAFF, a in ACTIVITIES) (x[s, a] * preference[s, a]);

var float: u3 = sum(a in ACTIVITIES) (
    (max(s in STAFF) (bool2int(x[s, a] /\ not experience[s]))) * 
    (max(s in STAFF) (bool2int(x[s, a] /\ experience[s])))       
);


% Objective Function 
float: w1;
float: w2;
float: w3;

solve maximize w1 * u1 + w2 * u2 + w3 * u3;
