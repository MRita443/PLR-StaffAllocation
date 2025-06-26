/** <module> Data for the Staff Scheduling problem */
:- module(data, [
    staff/3,
    activity/5,
    preference/3,
    available/2
]).

staff('André Lima', 3, [next, teaching, program, operations]).
preference('André Lima', escape_room, 3).
preference('André Lima', panel_pi, 0).
preference('André Lima', talk_chess, 3).
preference('André Lima', talk_visual_design, 5).
preference('André Lima', workshop_github_actions, 2).
preference('André Lima', workshop_react_next_1, 1).
preference('André Lima', workshop_react_next_2, 1).

staff('António Santos', 3, [next, teaching, operations]).
preference('António Santos', escape_room, 3).
preference('António Santos', panel_pi, 5).
preference('António Santos', talk_chess, 4).
preference('António Santos', talk_visual_design, 2).
preference('António Santos', workshop_github_actions, 2).
preference('António Santos', workshop_react_next_1, 0).
preference('António Santos', workshop_react_next_2, 0).

staff('Beatriz Ferreira', 1, [next, program, github_actions]).
preference('Beatriz Ferreira', coffee_break_3, 4).
preference('Beatriz Ferreira', escape_room, 4).
preference('Beatriz Ferreira', panel_pi, 5).
preference('Beatriz Ferreira', talk_chess, 4).
preference('Beatriz Ferreira', talk_visual_design, 1).
preference('Beatriz Ferreira', workshop_github_actions, 2).
preference('Beatriz Ferreira', workshop_react_next_1, 2).
preference('Beatriz Ferreira', workshop_react_next_2, 2).

staff('Bruno Oliveira', 3, [teaching, operations, github_actions]).
preference('Bruno Oliveira', coffee_break_3, 3).
preference('Bruno Oliveira', escape_room, 2).
preference('Bruno Oliveira', panel_pi, 2).
preference('Bruno Oliveira', talk_visual_design, 1).
preference('Bruno Oliveira', workshop_github_actions, 4).
preference('Bruno Oliveira', workshop_react_next_1, 3).
preference('Bruno Oliveira', workshop_react_next_2, 3).

staff('Clara', 1, [react, next, teaching, program, operations, github_actions]).
preference('Clara', coffee_break_3, 5).
preference('Clara', escape_room, 1).
preference('Clara', panel_pi, 2).
preference('Clara', talk_chess, 3).
preference('Clara', talk_visual_design, 2).
preference('Clara', workshop_github_actions, 3).
preference('Clara', workshop_react_next_1, 1).
preference('Clara', workshop_react_next_2, 1).

staff('Diogo Fernandes', 5, [next]).
preference('Diogo Fernandes', coffee_break_3, 0).
preference('Diogo Fernandes', escape_room, 0).
preference('Diogo Fernandes', panel_pi, 0).
preference('Diogo Fernandes', talk_chess, 1).
preference('Diogo Fernandes', talk_visual_design, 5).
preference('Diogo Fernandes', workshop_github_actions, 0).
preference('Diogo Fernandes', workshop_react_next_1, 0).
preference('Diogo Fernandes', workshop_react_next_2, 0).

staff('Eduarda Magno', 4, [react, next, operations, github_actions]).
preference('Eduarda Magno', escape_room, 4).
preference('Eduarda Magno', panel_pi, 5).
preference('Eduarda Magno', talk_chess, 0).
preference('Eduarda Magno', talk_visual_design, 4).
preference('Eduarda Magno', workshop_github_actions, 2).
preference('Eduarda Magno', workshop_react_next_1, 0).
preference('Eduarda Magno', workshop_react_next_2, 0).

staff('Francisco Cardoso', 1, [next, teaching, program]).
preference('Francisco Cardoso', coffee_break_3, 3).
preference('Francisco Cardoso', escape_room, 1).
preference('Francisco Cardoso', panel_pi, 1).
preference('Francisco Cardoso', talk_chess, 2).
preference('Francisco Cardoso', talk_visual_design, 5).
preference('Francisco Cardoso', workshop_github_actions, 3).
preference('Francisco Cardoso', workshop_react_next_1, 1).
preference('Francisco Cardoso', workshop_react_next_2, 1).

staff('Goiana', 3, [react, next, teaching, program, operations, github_actions]).
preference('Goiana', coffee_break_3, 5).
preference('Goiana', escape_room, 3).
preference('Goiana', panel_pi, 0).
preference('Goiana', talk_chess, 4).
preference('Goiana', talk_visual_design, 5).
preference('Goiana', workshop_github_actions, 1).
preference('Goiana', workshop_react_next_1, 2).
preference('Goiana', workshop_react_next_2, 2).

staff('Guilherme Santos', 4, [react, teaching, program, operations]).
preference('Guilherme Santos', coffee_break_3, 1).
preference('Guilherme Santos', escape_room, 0).
preference('Guilherme Santos', panel_pi, 4).
preference('Guilherme Santos', talk_chess, 3).
preference('Guilherme Santos', talk_visual_design, 1).
preference('Guilherme Santos', workshop_github_actions, 2).
preference('Guilherme Santos', workshop_react_next_1, 0).
preference('Guilherme Santos', workshop_react_next_2, 0).

activity(workshop_react_next_1, 2, 40, 8, [react, next, teaching, program]).
activity(escape_room, 2, 52, 3, [program]).
activity(workshop_react_next_2, 2, 58, 8, [react, next, teaching, program]).
activity(talk_visual_design, 1, 154, 4, [program]).
activity(panel_pi, 1, 164, 4, [program]).
activity(talk_chess, 1, 254, 3, [program]).
activity(workshop_github_actions, 2, 248, 6, [github_actions, teaching, program]).
activity(coffee_break_3, 3, 258, 2, [operations]).

available('André Lima', workshop_react_next_1).
available('André Lima', escape_room).
available('André Lima', workshop_react_next_2).
available('André Lima', workshop_github_actions).
available('António Santos', talk_chess).
available('António Santos', workshop_github_actions).
available('Beatriz Ferreira', talk_visual_design).
available('Bruno Oliveira', escape_room).
available('Bruno Oliveira', workshop_react_next_2).
available('Bruno Oliveira', talk_visual_design).
available('Bruno Oliveira', panel_pi).
available('Bruno Oliveira', talk_chess).
available('Bruno Oliveira', workshop_github_actions).
available('Bruno Oliveira', coffee_break_3).
available('Clara', escape_room).
available('Clara', workshop_react_next_2).
available('Diogo Fernandes', workshop_react_next_1).
available('Diogo Fernandes', escape_room).
available('Diogo Fernandes', talk_visual_design).
available('Diogo Fernandes', panel_pi).
available('Eduarda Magno', escape_room).
available('Francisco Cardoso', workshop_react_next_1).
available('Francisco Cardoso', escape_room).
available('Francisco Cardoso', talk_visual_design).
available('Francisco Cardoso', talk_chess).
available('Francisco Cardoso', workshop_github_actions).
available('Francisco Cardoso', coffee_break_3).
available('Goiana', escape_room).
available('Goiana', talk_visual_design).
available('Goiana', talk_chess).
available('Goiana', workshop_github_actions).
available('Goiana', coffee_break_3).
available('Guilherme Santos', talk_visual_design).
available('Guilherme Santos', panel_pi).
available('Guilherme Santos', talk_chess).
available('Guilherme Santos', workshop_github_actions).
available('Guilherme Santos', coffee_break_3).
