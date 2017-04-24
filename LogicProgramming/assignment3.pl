%% Assignment3
%% 1. Problem 1 in Assignment 6, Exercises 8.3.1 (i), (iii), (vi), (vii)

%% (i)
factorial(N,F):- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.
factorial(0,1).

triangle(N,T) :- triangle(N, 0, T).
triangle(0, S, S).
triangle(N, T, S) :- N > 0, N1 is N - 1, factorial(N,F), T1 is T + F, triangle(N1, T1, S).

%%(iii)
between(I,J,J) :- I =< J.
between(I,J,K) :- I < J, J1 is J - 1, between(I,J1,K).

%%(vi)
minlist([X|Xs],M):-minlist(Xs,X,M).
minlist([X|Xs],Y,M):-minimum(X,Y,Y1),minlist(Xs,Y1,M).
minlist([],M,M).

minimum(X,Y,X):- X =< Y, !.
minimum(X,Y,Y):- X > Y.

%%(vii)
length_list(Xs,L):-length_list(Xs,0,L).
length_list([],L,L).
length_list([X|Xs],T,L):-T1 is T+1, length_list(Xs,T1,L).


%% Problem 3 in Assignment 6, Exercises 11.3 (i) and (ii)
%%(i)
notequalvar(A,B):- equalvar(A,B),!,fail.
notequalvar(A,B).

equalvar(A,B):- A==B.


%%(ii)
nonvariants(A):-var(A),!,fail.
nonvariants(A).


%% ------------------------------------------------------------------------------------------------------------
%% 2. Belgian snake problem (As 6, continuation)
snake(Pattern,Cols,Rows):- infinitesnake(Pattern,InfiniteS,InfiniteS),
                           producesnake(Rows,Cols,InfiniteS,Snake),
                           coilstate(Snake,odd).
						   
infinitesnake([],S,S).
infinitesnake([A|R],[A|T],S):-infinitesnake(R,T,S).

producesnake([],_,_,[]).
producesnake([_|Rows],Cols,InfiniteS,[Part|Tail]):-partsnake(Cols,InfiniteS,NewInfiniteS,Part),
producesnake(Rows,Cols,NewInfiniteS,Tail).

partsnake([],RestSnake,RestSnake,[]).
partsnake([_|R],[Ring|Rings],RestSnake,[Ring|RestRings]):-partsnake(R,Rings,RestSnake,RestRings).

coilstate([],_).
coilstate([Line|Lines],odd):-write_elements(Line), nl,
coilstate(Lines,even).
coilstate([Line|Lines],even):-reverse_list(Line,Line1),
                            write_elements(Line1),nl,
							coilstate(Lines,odd).
					

write_elements([]).
write_elements([X|R]):-write(X),write_elements(R).


reverse_list(L,R):-reverse_list(L,[],R).
reverse_list([],R,R).
reverse_list([H|T],P,R):-reverse_list(T,[H|P],R).


%% -------------------------------------------------------------------------------------------------------------
%% 3. Assignment 7: problems 1-4; Problem 5 will earn you bonus credit, if you solve it, but is not required
%% Problem1: N - Queens
queens(N,Qs):-range(1,N,Ns),perm(Ns,Qs),safe(Qs).

range(M,N,[M|Ns]):- M<N,M1 is M+1, range(M1,N,Ns).
range(N,N,[N]).

perm([],[]).
perm(L,[X|R]):-select(X,L,T),perm(T,R).

select(X,[X|T],T).
select(X,[Y|T],[Y|R]):-select(X,T,R).

safe([]).
safe([Q|Qs]):-safe(Qs),noattack(Q,Qs,1).


noattack(_X,[],_N).
noattack(X,[F|T],N) :-
        X=\=F,
        X=\=F+N,
        F=\=X+N,
        N1 is N+1,
        noattack(X,T,N1).
             
%% Problem2: Send more money
sumcryptarithmetic1(N1,N2,N):-
 sumcryptarithmetic1_1(N1,N2,N,0,0,[0,1,2,3,4,5,6,7,8,9],_).
 
sumcryptarithmetic1_1([],[],[],C,C,Digits,Digits).
 
sumcryptarithmetic1_1([D1|N1],[D2|N2],[D|N],C1,C,Digs1,Digs):-
 sumcryptarithmetic1_1(N1,N2,N,C1,C2,Digs1,Digs2),
 digitsum(D1,D2,C2,D,C,Digs2,Digs).
 
digitsum(D1,D2,C1,D,C,Digs1,Digs):-
 select_cryptarithmetic(D1,Digs1,Digs2),
 select_cryptarithmetic(D2,Digs2,Digs3),
 select_cryptarithmetic(D,Digs3,Digs),
 S is D1+D2+C1,
 D is S mod 10,
 C is S // 10.
 
select_cryptarithmetic(A,L,L):-
 nonvar(A),!.

select_cryptarithmetic(A,[A|L],L).
select_cryptarithmetic(A,[B|L],[B|L1]):-
 select_cryptarithmetic(A,L,L1).
 
puzzle1([D,O,N,A,L,D],[G,E,R,A,L,D],[R,O,B,E,R,T]).
puzzle2([0,S,E,N,D], [0,M,O,R,E],[M,O,N,E,Y]).

%% Problem3: block worlds problem

:- dynamic on/2.

on(a,b).    
on(b,c).    
on(c,table).


putblockon(A,B) :- on(A,B).
putblockon(A,B) :- not(on(A,B)),
                   A \== table,
                   A \== B,
                   deleterelation(A),        
                   deleterelation(B),
                   on(A,X),
                   retract(on(A,X)),
                   assert(on(A,B)),
                   assert(move(A,X,B)).
     

deleterelation(table).                             /*If there is a room for table, do nothing*/
deleterelation(A) :- not(on(_X,A)).                /* Clear the block A */
deleterelation(A) :- A \== table,
                     on(X,A),
                     deleterelation(X),      
                     retract(on(X,A)),
                     assert(on(X,table)),
                     assert(move(X,A,table)).
 
do(AllList) :-  valid(AllList), steptogoal(AllList,AllList). 

valid(_).                         
   
steptogoal([G|R],Allgoals) :- call(G), steptogoal(R,Allgoals),!.  /* True of first clause*/
                                                                  /* The reminds blocks */

steptogoal([G|_],Allgoals) :- achieve(G), steptogoal(Allgoals,Allgoals).        
         /* To achieve the first goal of the remaining blocks */
         /* go back and check previous goals */
steptogoal([],_Allgoals).              /* finished all the given goals */

achieve(on(A,B)) :-  putblockon(A,B).

%% Problem4: missionary-cannibal problem
% to run the code in SWI-Prolog, do
%        ?- ['missionaries_and_cannibals.pl'].
%        ?- find_mission.

% Represent a state as [CL,ML,B,CR,MR]
start_mission([3,3,left,0,0]).
goal_mission([0,0,right,3,3]).

legal_mission(CL,ML,CR,MR) :-
	% is this state a legal_mission one?
	ML>=0, CL>=0, MR>=0, CR>=0,
	(ML>=CL ; ML=0),
	(MR>=CR ; MR=0).

% Possible moves:
move_mission([CL,ML,left,CR,MR],[CL,ML2,right,CR,MR2]):-
	% Two missionaries cross left to right.
	MR2 is MR+2,
	ML2 is ML-2,
	legal_mission(CL,ML2,CR,MR2).

move_mission([CL,ML,left,CR,MR],[CL2,ML,right,CR2,MR]):-
	% Two cannibals cross left to right.
	CR2 is CR+2,
	CL2 is CL-2,
	legal_mission(CL2,ML,CR2,MR).

move_mission([CL,ML,left,CR,MR],[CL2,ML2,right,CR2,MR2]):-
	%  One missionary and one cannibal cross left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	legal_mission(CL2,ML2,CR2,MR2).

move_mission([CL,ML,left,CR,MR],[CL,ML2,right,CR,MR2]):-
	% One missionary crosses left to right.
	MR2 is MR+1,
	ML2 is ML-1,
	legal_mission(CL,ML2,CR,MR2).

move_mission([CL,ML,left,CR,MR],[CL2,ML,right,CR2,MR]):-
	% One cannibal crosses left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	legal_mission(CL2,ML,CR2,MR).

move_mission([CL,ML,right,CR,MR],[CL,ML2,left,CR,MR2]):-
	% Two missionaries cross right to left.
	MR2 is MR-2,
	ML2 is ML+2,
	legal_mission(CL,ML2,CR,MR2).

move_mission([CL,ML,right,CR,MR],[CL2,ML,left,CR2,MR]):-
	% Two cannibals cross right to left.
	CR2 is CR-2,
	CL2 is CL+2,
	legal_mission(CL2,ML,CR2,MR).

move_mission([CL,ML,right,CR,MR],[CL2,ML2,left,CR2,MR2]):-
	%  One missionary and one cannibal cross right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	MR2 is MR-1,
	ML2 is ML+1,
	legal_mission(CL2,ML2,CR2,MR2).

move_mission([CL,ML,right,CR,MR],[CL,ML2,left,CR,MR2]):-
	% One missionary crosses right to left.
	MR2 is MR-1,
	ML2 is ML+1,
	legal_mission(CL,ML2,CR,MR2).

move_mission([CL,ML,right,CR,MR],[CL2,ML,left,CR2,MR]):-
	% One cannibal crosses right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	legal_mission(CL2,ML,CR2,MR).


% Recursive call to solve the problem
path_mission([CL1,ML1,B1,CR1,MR1],[CL2,ML2,B2,CR2,MR2],Explored,MovesList) :- 
   move_mission([CL1,ML1,B1,CR1,MR1],[CL3,ML3,B3,CR3,MR3]), 
   not(member([CL3,ML3,B3,CR3,MR3],Explored)),
   path_mission([CL3,ML3,B3,CR3,MR3],[CL2,ML2,B2,CR2,MR2],[[CL3,ML3,B3,CR3,MR3]|Explored],[ [[CL3,ML3,B3,CR3,MR3],[CL1,ML1,B1,CR1,MR1]] | MovesList ]).

% Solution found
path_mission([CL,ML,B,CR,MR],[CL,ML,B,CR,MR],_,MovesList):- 
	output_mission(MovesList).

% Printing
output_mission([]) :- nl. 
output_mission([[A,B]|MovesList]) :- 
	output_mission(MovesList), 
   	write(B), write(' -> '), write(A), nl.

% Find the solution for the missionaries and cannibals problem
find_mission :- 
   path_mission([3,3,left,0,0],[0,0,right,3,3],[[3,3,left,0,0]],_).

%% ------------------------------------------------------------------------------------------------------------
%% 4. Assignment 8: solve all problems
%% See the other .pl document 

