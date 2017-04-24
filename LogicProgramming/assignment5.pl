/*Exercise 3.2.1*/
%/* (ii) adjacent and last*/
adjacent(X, Y, [X, Y| Zs]).
adjacent(X, Y, [Z| Zs]):- adjacent(X, Y, Zs). 

last(X, [X]).
last(X, [Y|Xs]):-last(X,Xs).
/*(iii) double, appear twice*/
double([], []).
double([X| Xs],[X, X| Ys]) :- double(Xs, Ys).

/*Exercise 3.3.1*/
%/*(i)substitute(X,Y,L1,L2)*/
substitute(X,Y,[],[]).
substitute(X,Y,[X|Xs],[Y|Ys]):-substitute(X,Y,Xs,Ys).
substitute(X,Y,[Z|Xs],[Z|Ys]):-X\=Z, substitute(X,Y,Xs,Ys).

/*(ii)select*/
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]):- X\=Y,select(X,Ys,Zs).

/*(iii)no_doubles*/
no_doubles([],[]).
no_doubles([X|Xs], Ys):-member(X, Xs), no_doubles(Xs, Ys).
no_doubles([X|Xs], [X|Ys]):-not_member(X, Xs),no_doubles(Xs, Ys).

member(X,[X|Xs]).
member(X,[Y|Ys]):-member(X,Ys).

not_member(_, []).
not_member(X, [Y|Ys]):-X \= Y, not_member(X, Ys).

/*(vi)kth largest*/

kth_largest(Xs,K,E):-length(Xs,LXs),LXs > 5,
                     breakinto5(Xs,ListOfFive),
					 tomedianList(ListofFive,Medianslist),
                     median(Medianslist,MeOfMe),
					 partition(Xs,MeOfMe,Small,Large),
					 length(Small,LSmall),
					 (
					   K > LSmall ->
					   J is K - LengthOfR, kth_largest(Large,J,E);
% K =< lengthOfR,
					   kth_largest(Small,K,E) 
					  ),!.

kth_largest(Xs,K,E):-length(Xs,LXs),LXS =< 5, 
                     qsort(Xs,R),
					 (
					   LXs >= K, L is LXs - K + 1;
					   LXs < K, L is 1
					 ),
					 nthElement(L,R,E),!.
					 
breakinto5([],[]).
breakinto5(Xs,[L|Ls]):-firstNumElement(5,Xs,L),append(L,B,Xs),breakinto5(B,Ls),!.

firstNumElement(0,_,[]).
firstNumElement(_,[],[]).
firstNumElement(N,[T|Xs],[T|L]):- N > 0, M is N-1, firstNumElement(M,Xs,L).

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):-append(Xs,Ys,Zs).

median(L,M):-qsort(L,L1),length(L1,Length),N is ceiling(L/2),nthElement(N,L1,M).

nthElement(1,[X|_],X).
nthElement(N,[_|Xs],E):-N>1, M is N-1, nthElement(M,Xs,E).

tomedianList([],[]).
tomedianList([X|Xs],[Y|Ys]) :- median(X,M),tomedianList(Xs,Ys).

partition([],_,[],[]).
partition([H|T],P,[H|S1],B1):- H =< P, partition(T,P,S1,B1).
partition([H|T],P,S,[H1|B1]):- H > P, partition(T,P,S1,B1).

qsort([],[]).
qsort([P|T],R):-partition(T,P,S,B),qsort(S,S1),qsort(B,B1),append(S1,[P|B1],R).


					 
/*(vii)*/
faces([2,3,4,5,6,7,8,9,10,j,q,k,a])

face(F):- faces(Fs),member(F,Fs).


					 
					 
%/*Question 3 sum & delete*/

%/*(1)sum*/
tree(Element,Left,Right).

binary_tree(void).
binary_tree(tree(Element,Left,Right)):-binary_tree(Left),binary_tree(Right).

sumtree(nil, 0).
sumtree(tree(s(X),T1,T2), S) :- sumtree(T1, S1), sumtree(T2, S2), plus(S1,S2,S0),plus(S0,s(X),S).

plus(0,X,X).
plus(s(X),Y,s(Z)):-plus(X,Y,Z).

/*(2)delete*/
delete(X,tree(X,nil,Right),Right).
delete(X,tree(X,Left,nil),Left).
delete(X,tree(X,Left,Right),tree(Y,Left,Right1)):-deletemin(Y,Right,Right1).

delete(X,tree(Root,Left,Right),tree(Root,Left1,Right)):-gt(Root,X),delete(X,Left,Left1).

delete(X,tree(Root,Left,Right),tree(Root,Left,Right1)):-gt(X,Root),delete(X,Right,Right1).

deletemin(Y,tree(Y,nil,Right),Right).
deletemin(Y,tree(Root,Left,Right),tree(Root,Left1,Right)):-deletemin(Y,Left,Left1).

gt(X,Y):-X>Y.




/*Exercise 8.3.1*/
%/*(i)triangle*/
triangle(N,T):-triangle(N,0,T).
triangle(0,M,M).
triangle(N,M,T):-N>0, N1 is N-1, B is M+N, triangle(N1,B,T).

/*(iii)a range of integers in descending order */
between(I,J,J):-J >= I.
between(I,J,K):-J > I, J1 is J-1, between(I,J1,K).

/*(vi)minimum*/
minlist([X|Xs],S):-minlist(Xs,X,S).
minlist([X|Xs],Y,S):-minimum(X,Y,Y1),minlist(Xs,Y1,S).
minlist([],S,S).

minimum(X,Y,X):-X =< Y.
minimum(X,Y,Y):-X > Y.

/*(vii)length of list*/
length_list(Xs, L):-length_list(Xs,0,L).
length_list([],L,L).
length_list([X|Xs],L0,L):-L1 is L0+1, length_list(Xs,L1,L).