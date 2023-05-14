% AEM: 3770, Φυσέκης Θωμάς
% AEM: 3620, Στέργιος Μουμτζής
/*
len_combination(Len, Lst, Comb) :-
    length(Comb, Len),
    len_combination_(Comb, Lst).

combinations([], []).
combinations([H|T], [H|T2]) :-
    combinations(T, T2).
combinations([_|T], T2) :-
    combinations(T, T2).

all_combinations(L) :- L = [A,B,C,D,E,F,G,H],
                       member(A,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       member(B,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       B \= A,
                       member(C,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       C \= A, C \= B,
                       member(D,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       D \= A, D \= B, D \= C,
                       member(E,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       E \= A, E \= B, E \= C, E \= D,
                       member(F,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       F \= A, F \= B, F \= C, F \= D, F \= E,
                       member(G,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       G \= A, G \= B, G \= C, G \= D, G \= E, G \= F,
                       member(H,[im204, im209, im210, im212, im214, im216, im217, im218]),
                       H \= A, H \= B, H \= C, H \= D, H \= E, H \= F, H \= G.

*/

%% Define the lessons
lessons([im204, im209, im210, im212, im214, im216, im217, im218]).

schedule(A, B, C) :-
    % Get the list of all lessons
    lessons(L),
    % Try all possible schedules using the remaining lessons
    schedule_(L, A, B, C).
    % Check that each group has the right number of lessons
    length(A,3),
    length(B,3),
    length(C,2).
    % Check that the schedule satisfies the constraints
    valid_schedule(A, B, C).

% Define the schedule_ predicate for backtracking
schedule_([], [], [], []).
schedule_([L|Ls], A, B, C) :-
    % Try adding the lesson to the first week
    schedule_(Ls, A1, B, C),
    length(A1, Na),
    Na < 3,
    \+ member(L, A1),
    A = [L|A1].
schedule_([L|Ls], A, B, C) :-
    % Try adding the lesson to the second week
    schedule_(Ls, A, B1, C),
    length(B1, Nb),
    Nb < 3,
    \+ member(L, B1),
    B = [L|B1].
schedule_([L|Ls], A, B, C) :-
    % Try adding the lesson to the third week
    schedule_(Ls, A, B, C1),
    length(C1, Nc),
    Nc < 2,
    \+ member(L, C1),
    C = [L|C1].


% Define the valid_schedule predicate
valid_schedule(A, B, C) :-
    % Check that no lesson appears more than once in the schedule
    setof(L, (member(L, A); member(L, B); member(L, C)), Ls),
    length(Ls, N),
    length(A, Na),
    length(B, Nb),
    length(C, Nc),
    % Check that all three weeks have the correct number of unique lessons
    N =:= Na + Nb + Nc.

