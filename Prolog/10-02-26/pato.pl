pato(lucas).
pato(donald).
pato(juan).

padre(daniel, lucio).
padre(daniel, kevin).
padre(efrain, daniel).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).
hermano(X,Y) :- padre(Z,X), padre(Z,Y), X \= Y.

