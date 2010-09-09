:- initialization((atom_chars('abcdefghijklmnopqrstuvwxyz',L), asserta(letters(L)))).
cs(H) :- letters(L), member(H,L).

% remove any element from the list
rem([X|R],X,R).
rem([F|R],X,[F|S]) :- rem(R,X,S).

% replace any given element in a list 
rep([X|R],Y,[Y|R]).
rep([X|R],Y,[X|S]) :- rep(R,Y,S).

% insert any given element into a list 
ins([X|R],Y,[X,Y|R]).
ins([X|R],Y,[Y,X|R]).
ins([X|R],Y,[X|S]) :- ins(R,Y,S).

% Substitution: A only has 1 element different from B
t1(A,B) :- cs(X), rep(A,X,B).

% Deletion: A is equal to B except for 1 additional element
t2(A,B) :- cs(X), rem(A,X,B).

% Insertion: B has one more element than A and all other elements are equal
t3(A,B) :- cs(X), ins(A,X,B).

% calculates the distance between two words in terms of characters
% that are different
diff([X|T1],[Y|T2],N) :- (X == Y,diff(T1,T2,N); diff(T1,T2,N2),N is N2 + 1), !.
diff(X,[],N) :- length(X,N), !.
diff([],X,N) :- length(X,N), !.

% pretty printing the converts the [[a,b,c]] back into [abc] which is easier
% for the humanoids
pp([],[]).
pp([X|T],[H|T2]) :- atom_chars(H,X), pp(T,T2).

calc(A,B,(C,R2)) :- atom_chars(A,A1), atom_chars(B,B1), edist(A1,B1,R,C), pp(R,R2).

edist(A,B,[],0) :- diff(A,B,0).
edist(A,B,[X|R],C) :- t1(A,X), diff(A,B,N1), diff(X,B,N2), N2 < N1, edist(X,B,R,C1), C is C1 + 1.
edist(A,B,[X|R],C) :- t2(A,X), diff(A,B,N1), diff(X,B,N2), N2 < N1, edist(X,B,R,C1), C is C1 + 2.
edist(A,B,[X|R],C) :- t3(A,X), diff(A,B,N1), diff(X,B,N2), N2 < N1, edist(X,B,R,C1), C is C1 + 3.

valid(B,R) :- best(B1,R1), R < R1, retract(best(_,_)), asserta(best(B,R)), !.

gen(A,B,_) :- calc(A,B,(C,R1)), valid(R1,C), fail.
gen(_,_,(C,R)) :- best(R,C).

calcall(A,B,R) :- asserta(best([], 999999)), gen(A,B,R), retract(best(_,_)).

