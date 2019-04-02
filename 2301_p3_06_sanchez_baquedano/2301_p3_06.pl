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
%TODO: Entender por que funciona
aplasta([], []) :- !.
aplasta([X|L], L1) :- !, aplasta(X, F), aplasta(L, F1), concatena(F,F1, L1).
aplasta(L, [L]).
%%%%%%%%%%%%%%%
% Ejercicio 6 %
%%%%%%%%%%%%%%%
next_factor(N, F, NF) :- (F=2, NF=3) ; (F<sqrt(N), NF=F+2).
