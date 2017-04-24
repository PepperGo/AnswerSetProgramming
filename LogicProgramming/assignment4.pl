%% Assignment4 Family tree
%% There are two niece definitions, so please make sure one is blocked.



/*1. Transcribe diagram into father/mother relationships*/
father(tony,abe).
father(abe,john).
father(john,jill).
father(bill,susan).
father(rob,jack).
father(rob,phil).
father(jack,jim).

male(tony).
male(abe).
male(john).
male(jill).
male(bill).
male(rob).
male(jack).
male(phil).
male(jim).

mother(lisa,sarah).
mother(lisa,abe).
mother(nancy,john).
mother(mary,jill).
mother(sarah,susan).
mother(susan,jack).
mother(susan,phil).

female(lisa).
female(nancy).
female(mary).
female(sarah).
female(susan).

parent(X,Y):-father(X,Y).
parent(X,Y):-mother(X,Y).

sibling(X,Y):-parent(Z,X),parent(Z,Y),X\=Y.



/*2.Define rules for relationships*/
/*(a)fcousin*/
fcousin(X,Y):-parent(Z1,X),parent(Z2,Y),sibling(Z1,Z2).
/*(b)scousin*/
scousin(X,Y):-parent(Z1,X),parent(Z2,Y),fcousin(Z1,Z2).
/*(c)grnephew*/
%%nephew(X,Y):-parent(Z,X),sibling(Z,Y),male(X).
%%grnephew(X,Y):-sibling(W,Y),parent(W,Z),parent(Z,X),male(X).
/*(d)niece*/
%%niece(X,Y):-parent(Z,X),sibling(Z,Y),female(X).

/*(e)manc)*/

anc(X,Y):-parent(X,Y).
anc(X,Y):-parent(X,Z),anc(Z,Y).
%%manc(X,Y):-parent(X,Y),male(X).
manc(X,Y):-anc(X,Y),male(X).


/* left recursion
manc(X,Y):-father(X,Y).
manc(X,Y):-manc(X,Z),parent(Z,Y).
*/

/* 3. X,Y are cousins of the same generation*/
samegencousin(X,Y):-fcousin(X,Y).
samegencousin(X,Y):-parent(Z1,X),parent(Z2,Y),samegencousin(Z1,Z2).

/* 4. married, redifine grnephew, niece */
married(jill,rick).
married(jack,kim).
married(jim,martha).
married(phil,ann).
married(rick,jill).
married(kim,jack).
married(martha,jim).
married(ann,phil).
married(X,Y):-father(X,Z),mother(Y,Z).
married(X,Y):-mother(X,Z),father(Y,Z).
niece(X,Y):-parent(Z,X),sibling(Z,Y),female(X).
niece(X,Y):-parent(Z,X),sibling(Z,W),female(X),married(W,Y).

%%grnephew:
grnephew(X,Y):-sibling(W,Y),parent(W,Z),parent(Z,X),male(X).
grnephew(X,Y):-sibling(W,H),parent(W,Z),parent(Z,X),male(X),married(H,Y).