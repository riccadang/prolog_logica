%%% Esercizi Prolog relativi al corso di Logica per l'informatica 
%%% Non è garantita la correttezza delle soluzioni proposte.

%%%%%%%%%%%%%%%%%%%%%%

% true se gli elementi della lista sono tutti minori del numero N
tutti_minori([A|_],N):- A>N,!,fail.
tutti_minori([],_).
tutti_minori([_|Resto],N):-tutti_minori(Resto,N).

%%%%%%%%%%%%%%%%%%%%%%%

%riporta la lista di lunghezza maggiore nella lista di liste.
lunghezza([],0):-!.
lunghezza([_|Resto],L):-lunghezza(Resto,L1),L is L1+1.
lunghezza_massima([],[]):-!.
lunghezza_massima([A|Resto],L):- lunghezza(A,M),lunghezza_massima(Resto,L1),lunghezza(L1,M1), (M>M1->L=A;L=L1).

%%%%%%%%%%%%%%%%%%%%%%%

%sumbetween: somma gli elementi tra l'elemento x ed y. fallisce se non ci sono.
sumbetween(A,X,_,_):- not(member(X,A)),!,fail.
sumbetween(A,_,Y,_):- not(member(Y,A)),!,fail.
sumbetween([A|Resto],X,Y,S):- (not(A=X)->sumbetween(Resto,X,Y,S);somma(Resto,Y,S1),S is S1+A).
somma([Y|_],Y,Y).
somma([A|Resto],Y,S2):- somma(Resto,Y,S3),S2 is S3+A.

%%%%%%%%%%%%%%%%%%%%%%%

%lista che si ottiene da lst togliendone i primi n elementi. 
%Se il numero di elementi di lst è minore di n (oppure uguale a n), allora cut_n lst = [].
cut_n(L,N,[]):- lunghezza(L,M), M=<N,!.
cut_n([_|Resto],N,L):- N>0,!,N1 is N-1,cut_n(Resto,N1,L).
cut_n(A,N,A):- N=0.

%%%%%%%%%%%%%%%%%%%%%%%

%torna l'elemento in pos X della lista. se X è negativo o non c'è fallisce.
posizione(L,N,_):-lunghezza(L,M),M<N,!,fail.
posizione(_,N,_):-N<0,!,fail.
posizione([A|_],0,A).
posizione([_|Resto],M,B):- N is M-1,posizione(Resto,N,B).

%%%%%%%%%%%%%%%%%%%%%%%

%ritorna una lista lunga N con tutti elementi = X.
crea_lista(N,_,_):- N<0,!,fail.
crea_lista(0,_,[]).
crea_lista(N,X,[X|L]):- M is N-1, crea_lista(M,X,L).

%%%%%%%%%%%%%%%%%%%%%%%

%data una lista di liste, ritorna il minimo tra i massimi delle liste
min_del_max([A],M):-max(A,M).
min_del_max([A|Resto],Minimo):- max(A,MaxA),Minimo1=MaxA,min_del_max(Resto,MaxResto),((Minimo1>MaxResto)->Minimo=MaxResto;Minimo=MaxA).
max([A],A).
max([A|Resto],M):- max(Resto,M1),(M1>A ->M=M1;M=A).

%%%%%%%%%%%%%%%%%%%%%%%

%enumera: prende lista [x1, x2, x3] e un valore da cui enumerare e ritorna lista enumerata [(0,x1),(1,x2)...]
enumera([],[],_).
enumera([A|Resto],L,M):- M1 is M+1, enumera(Resto,L1,M1),append([(A,M)],L1,L).

%%%%%%%%%%%%%%%%%%%%%%%

%Una funzione duplica che, applicata a una lista xs = [x1;x2;...;xn],riporti la lista [x1;x1;x2;x2;...;xn;xn].
duplica_lista([A],[A,A]).
duplica_lista([A|Resto],[A,A|L]):-duplica_lista(Resto,L).


%%%%%%%%%%%%%%%%%%%%%%%
%applicata a un intero positivo k e una lista L, riporta una lista 
%contenente tutte le sottoliste di L di lunghezza k. 
sottoliste([],_,[]).
sottoliste([A|Resto],K,Sottoliste) :- lunghezza_lista([A|Resto],K,Sotto1),lung(Resto,Lunghezza),((Lunghezza>=K)->sottoliste(Resto,K,Sotto2),append([Sotto1],[Sotto2],Sottoliste);append([Sotto1],[],Sottoliste)).
lunghezza_lista(_,0,[]).
lunghezza_lista([A|Resto],L,Lista):- L1 is L-1, lunghezza_lista(Resto,L1,Lista1),append([A],Lista1,Lista).
lung([],0).
lung([_|Resto],L):-lung(Resto,L1), L is L1+1.

%%%%%%%%%%%%%%%%%%%%%%%

%applicata ad una lista lst, riporti la lista
%di tutte le triple adiacenti di elementi di lst (lista vuota se lst ha meno di 3 elementi).
triplette([A,B,C],[A,B,C]):-!.
triplette([A|Resto],L):- lung([A|Resto],Lung) ,(Lung>=3 -> lunghezza_lista([A|Resto],3,L1),triplette(Resto,L2),append([L1],[L2],L);L=[]).


%%%%%%%%%%%%%%%%%%%%%%%

%determina se due alberi binari hanno la stessa struttura 
%(cioè se essi sono uguali quando si ignorano le rispettive etichette; 
struttura_tree(empty,empty).
struttura_tree(t(_,L1,R1),t(_,L2,R2)):-struttura_tree(L1,L2),struttura_tree(R1,R2).

%%%%%%%%%%%%%%%%%%%%%%%

%dato un albero binario B e una lista L, determina se ogni foglia è un elemento della lista
foglia_in_list(empty,_).
foglia_in_list(t(Root,empty,empty),Lista):- not(member(Root,Lista)),!,fail.
foglia_in_list(t(_,L,R),Lista):-foglia_in_list(L,Lista),foglia_in_list(R,Lista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Crea l'insieme S a partire dai nodi dell'albero B
set_tree(empty,[]).
set_tree(t(Root,L,R),Set):- set_tree(L,Set1),set_tree(R,Set2),(member(Root,Set1);member(Root,Set2)->append(Set1,Set2,Set);append([Root|Set1],Set2,Set)).

%%%%%%%%%%%%%%%%%%%%%%%%%%

%Crea l'insieme S a partire dalla lista L
make_set([A],[A]).
make_set([A|Resto],Lista):- make_set(Resto,Lista1),(member(A,Lista1)->Lista=Lista1;append([A],Lista1,Lista)).

%%%%%%%%%%%%%%%%%%%%%%

%Data la lista L con un elemento X, restistuisce due liste L1 e L2
%L1 contiene tutti gli elementi fino ad X, L2 gli elementi dopo X
spezza_lista([],_,[],[]).
spezza_lista(Lista,X,_,_):-not(member(X,Lista)),!,fail.
spezza_lista([A|Resto],X,P,S):- (not(A=X)->spezza_lista(Resto,X,P1,S),append([A],P1,P);P=[],S=Resto).


%%%%%%%%%%%%%%%%%%%%%%
arc(1,a,2).
arc(3,b,5).
arc(5,c,4).
arc(1,b,3).
arc(3,c,5).
arc(6,b,5).
arc(1,c,4).
arc(4,b,1).
arc(2,a,6).
arc(4,c,6).

%actions(Start,Goal,Arcs):- arcs è lista delle etichette del cammino SENZA
%RIPETIZIONE DEI NODI da start a goal.
actions(Start,Goal,Arcs):-actions(Start,Goal,Arcs,[]).
actions(N,_,_,Visited):-member(N,Visited),!,fail.
actions(Start,Goal,[X],_):- arc(Start,X,Goal).
actions(Start,Goal,[X|Archi],Visited):- arc(Start,X,N),actions(N,Goal,Archi,[Start|Visited]).

%%%%%%%%%%%%%%%%%%%%%%

%restituisce il prodotto cartesiano di due liste
cartesiano(_,[],[]).
cartesiano([],_,[]).
cartesiano([A|Resto],[B|Resto1],[(A,B)|L]):- cartesiano([A],Resto1,L2),cartesiano(Resto,[B|Resto1],L3),append(L2,L3,L).

%%%%%%%%%%%%%%%%%%%%%%

%trasforma la lista L, come indicato con la lista B
%se nella lista L è presente l'elemento X, presente anche in B con (X,Y), sostituisci
%X con Y
trasforma_lista([],_,[]).
trasforma_lista(A,[],A).
trasforma_lista([A|Resto],C,L):- (member((A,X),C)-> trasforma_lista(Resto,C,L1),append([X],L1,L);trasforma_lista(Resto,C,L1),append([A],L1,L)).

%%%%%%%%%%%%%%%%%%%%%%

%lista degli elementi dell'albero maggiori di X
tree_maggiore(empty,_,[]).
tree_maggiore(t(Root,L,R),X,Lista):- tree_maggiore(L,X,Lista1),tree_maggiore(R,X,Lista2),(Root>X -> append([Root|Lista1],Lista2,Lista);append(Lista1,Lista2,Lista)).

%%%%%%%%%%%%%%%%%%%%%%

%modifica albero: simile a trasforma_lista.
mod_tree(empty,_,empty).
mod_tree(t(Root,L,R),[],t(Root,L,R)).
mod_tree(t(Root,L,R),Lista,t(X,L1,R1)):-member((Root,X),Lista),!,mod_tree(L,Lista,L1),mod_tree(R,Lista,R1).
mod_tree(t(Root,L,R),Lista,t(Root,L1,R1)):-mod_tree(L,Lista,L1),mod_tree(R,Lista,R1).

%%%%%%%%%%%%%%%%%%%%%%

%ribalta l'albero: l'albero che c'è a sx va a dx e viceversa.
ribarta(empty,empty).
ribarta(t(Root,L,R),t(Root,L1,R1)):-ribarta(L,R1),ribarta(R,L1).

%%%%%%%%%%%%%%%%%%%%%%

%restituisce il numero delle foglie dell'albero binario
num_foglie(empty,0):-!.
num_foglie(t(_,empty,empty),1):-!.
num_foglie(t(_,L,R),F):-num_foglie(L,F1),num_foglie(R,F2),F is F1+F2.


%%%%%%%%%%%%%%%%%%%%%%

%true se esiste un cammino dalla radice alla foglia X e il 
%cammino non contiene elementi della lista.
no_path(t(Root,empty,empty),_,Root):-!.
no_path(t(Root,_,_),Lista,_):- member(Root,Lista),!,fail.
no_path(t(_,L,R),Lista,Foglia):-no_path(L,Lista,Foglia);no_path(R,Lista,Foglia).

%%%%%%%%%%%%%%%%%

%restituisci la lista dei nodi del cammino dalla radice al nodo X.
branch(t(Root,empty,empty),Root,[Root]).
branch(t(Root,L,R),Foglia,Lista):- branch(L,Foglia,Lista1),append([Root],Lista1,Lista);branch(R,Foglia,Lista2),append([Root],Lista2,Lista).

%%%%%%%%%%%%%%%%%%%%%%

%sostituisci il nodo N con il nodo Nuovo nell'albero T per creare l'albero T1.
%se ci sono più occorrenze nel nodo, ne cambia una alla volta.

sostituisci_nodo(N,Nuovo,t(N,L,R),t(Nuovo,L,R)).
sostituisci_nodo(N,Nuovo,t(Root,L1,R),t(Root,L2,R)):-sostituisci_nodo(N,Nuovo,L1,L2).
sostituisci_nodo(N,Nuovo,t(Root,L,R1),t(Root,L,R2)):-sostituisci_nodo(N,Nuovo,R1,R2).

%%%%%%%%%%%%%%%%%%%%%%

%Cancella la foglia con etichetta X dell'albero. Se ci sono piu' foglie con questa etichetta 
%ne cancella una alla volta.
togli_foglia(X,t(X,empty,empty),empty).
togli_foglia(X,t(Root,L1,R),t(Root,L2,R)):-togli_foglia(X,L1,L2).
togli_foglia(X,t(Root,L,R1),t(Root,L,R2)):-togli_foglia(X,R1,R2).

%%%%%%%%%%%%%%%%%%%%%%

%Cancella tutte le foglie con etichetta X dell'albero.
togli_all_foglie(_,empty,empty).
togli_all_foglie(X,t(X,empty,empty),empty):-!.
togli_all_foglie(X,t(Root,L,R),t(Root,L1,R1)):-togli_all_foglie(X,L,L1),togli_all_foglie(X,R,R1).


%%%%%%%%%%%%%%%%%%%%%%

%elimina la prima occorrenza dell'elemento X dalla lista
del_first(X,L,_):-not(member(X,L)),!,fail.
del_first(X,[X|Resto],Resto):-!.
del_first(X,[A|Resto],[A|L]):-del_first(X,Resto,L).

%%%%%%%%%%%%%%%%%%%%%%

%elimina tutte le occorrenze dell'elemento X dalla lista
del_all(X,A,A):-not(member(X,A)),!.
del_all(_,[],[]).
del_all(X,[A|Resto],L):- (X=A->del_all(X,Resto,L);del_all(X,Resto,L1),append([A],L1,L)).


%%%%%%%%%%%%%%%%%%%%%%
%true se tutti gli elemeni della prima lista sono anche elementi della seconda lista.
occorrono([],_).
occorrono([A|_],L):-not(member(A,L)),!,fail.
occorrono([_|Resto],L):-occorrono(Resto,L).


%%%%%%%%%%%%%%%%%%%%%%
%riporta la lunghezza del cammino dalla radice al nodo X
lunghezza_cammino(t(Root,empty,empty),0,Root).
lunghezza_cammino(t(_,L,_),M,Etichetta):-lunghezza_cammino(L,M1,E),M is M1+1,Etichetta=E.
lunghezza_cammino(t(_,_,R),P,Etichetta):-lunghezza_cammino(R,M2,E),P is M2+1,Etichetta=E.

%%%%%%%%%%%%%%%%%%%%%%

%restituisce il minimo della lista
calcola_minore([A],A).
calcola_minore([A|Resto],Min):- calcola_minore(Resto,Min1), (A<Min1 -> Min=A;Min=Min1).


