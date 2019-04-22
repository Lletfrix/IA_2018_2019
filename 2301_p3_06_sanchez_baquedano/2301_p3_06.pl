%%%%%%%%%%%%%%%
% Ejercicio 1 %
%%%%%%%%%%%%%%%
duplica([],[]) :- !.
duplica([X | L], [X, X | L1]) :- duplica(L, L1).
%%%%%%%%%%%%%%%
% Ejercicio 2 %
%%%%%%%%%%%%%%%
invierte(L, R) :- invierte(L, [], R, R). % en [] vamos a ir creando la lista inversa.
invierte([], R, R, []) :- !.
invierte([X|L], Q, R, [_|SZR]) :- invierte(L, [X|Q], R, SZR). %SZR es el tamaño de lo que queda de R
%%%%%%%%%%%%%%%
% Ejercicio 3 %
%%%%%%%%%%%%%%%
igual(X, X).
palindromo(L1) :- igual(L1, L2), invierte(L1, L2).
%%%%%%%%%%%%%%%
% Ejercicio 4 %
%%%%%%%%%%%%%%%
divide([], _, [], []) :- !.
divide([X|L], N, [X|L1], L2) :- N>0, divide(L, N-1, L1, L2).
divide([X|L], N, L1, [X|L2]) :- N=<0, divide(L, N-1, L1, L2).
%%%%%%%%%%%%%%%
% Ejercicio 5 %
%%%%%%%%%%%%%%%
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

aplasta([], []) :- !.
aplasta([X|L], L1) :- !, aplasta(X, F), aplasta(L, F1), concatena(F,F1, L1).
aplasta(L, [L]).
%%%%%%%%%%%%%%%
% Ejercicio 6 %
%%%%%%%%%%%%%%%
next_factor(_,2,3) :- !.
next_factor(N, F, NF) :- NF is F+2, F<N.

es_divisible(X, Y) :- X mod Y =:= 0.

prime(X) :- crypto_is_prime(X, []).

primos(N, L) :- N > 0, primos(N, L, 2).
primos(1, [], _):- !.
primos(N, [X|L], X) :- es_divisible(N, X), prime(X),  C is N/X, primos(C, L, X), !.
primos(N, L, X) :- next_factor(N, X, NF), primos(N, L, NF).
%%%%%%%%%%%%%%%
% Ejercicio 7 %
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%
% Ejercicio 7.1 %
%%%%%%%%%%%%%%%%%
cod_primero(X, [], [], [X]).
cod_primero(X, [X|L], LRem, [X|LFront]) :- cod_primero(X, L, LRem, LFront), !.
cod_primero(X1, [X2|L], [X2|LRem], LFront) :- cod_primero(X1, L, LRem, LFront), !.

%%%%%%%%%%%%%%%%%
% Ejercicio 7.2 %
%%%%%%%%%%%%%%%%%
cod_all([], []).
cod_all([X1|L1], [X2|L2]) :- cod_primero(X1, L1, LRest, X2), cod_all(LRest, L2).

%%%%%%%%%%%%%%%%%
% Ejercicio 7.3 %
%%%%%%%%%%%%%%%%%
list_to_rl([X|L], [Len,X]) :- length([X|L], Len).

transform_rl([], []).
transform_rl([X1|L1], [X2|L2]) :- list_to_rl(X1, X2), transform_rl(L1, L2).

run_length(L, L1) :- cod_all(L, L2),
                     transform_rl(L2, L1).
%%%%%%%%%%%%%%%
% Ejercicio 8 %
%%%%%%%%%%%%%%%
get_symbol(Comp, X) :- compound_name_arguments(Comp, _, [X|_]).
build_tree([], nil) :- !.
build_tree([X], tree(Sym, nil, nil)) :- get_symbol(X, Sym).
build_tree([X|L], tree(1, T1, T2)) :- build_tree([X], T1), build_tree(L, T2), !.
%%%%%%%%%%%%%%%%%
% Ejercicio 8.1 %
%%%%%%%%%%%%%%%%%
info(tree(I, _, _), I).
left(tree(_, L, _), L).
right(tree(_, _, R), R).

encode_elem(E, [0], Tree) :- left(Tree, Elem), info(Elem, E).
encode_elem(E, [1], Tree) :- right(Tree, Elem), info(Elem, E), !.
encode_elem(E, [1|L], Tree) :- right(Tree, Elem), encode_elem(E, L, Elem), !.
%%%%%%%%%%%%%%%%%
% Ejercicio 8.2 %
%%%%%%%%%%%%%%%%%
encode_list([], [], _).
encode_list([X1|L1], [X2|L2], Tree) :- encode_elem(X1, X2, Tree), encode_list(L1, L2, Tree), !.
%%%%%%%%%%%%%%%%%
% Ejercicio 8.3 %
%%%%%%%%%%%%%%%%%
% Comprobamos que la cadena sea válida
dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
string_valid([]).
string_valid([X|L]) :- dictionary(Dict), member(X, Dict),  string_valid(L), !.

% Generamos una lista con las apariciones de cada letra
runlen_text(L, L1) :- msort(L, SL), run_length(SL, L1).

% Transformamos una lista listas con dos valores en una lista de clave-valor
list_to_comps([], []).
list_to_comps([X|L], [XC| LC]) :- compound_name_arguments(XC, -, X), list_to_comps(L, LC).

% Invertimos el orden de los compounds
invierte_comp(C, C1) :- compound_name_arguments(C, Functor, L), invierte(L, L1), compound_name_arguments(C1, Functor, L1).

invierte_comp_list([], []).
invierte_comp_list([X1|L1], [X2|L2]) :- invierte_comp(X1, X2), invierte_comp_list(L1, L2).

% Construimos una lista preparada para llamar a build_tree
build_list(L1, L2) :- string_valid(L1), runlen_text(L1, RL), list_to_comps(RL, LC), keysort(LC, SL), invierte_comp_list(SL, IL), invierte(IL, L2).

encode(L1, L2) :- build_list(L1, L), build_tree(L, Tree), encode_list(L1, L2, Tree).
