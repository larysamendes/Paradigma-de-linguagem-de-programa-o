
%Dada uma lista de números, apenas um não terá seu par. Encontre o número solitário.


contElem(_, [], 0).
contElem(N, [N|Y], Total):-
    contElem(N, Y, NewT),
    Total is NewT +1.

contElem(N, [X|Y], R):-
    N \= X,
    contElem(N, Y, R).

encontraPares(_,[X], X).
encontraPares(List, [X|Y] , R):-
    contElem(List, X, Total),
    Total mod 2 =\= 0,
    R is Total, !.
encontraPares(List, [X|Y], R):-
    encontraPares(List, Y, R).


maiorLista([X], 1, X).
maiorLista([X|Z],P, Maior):-
    maiorLista(Z,Posi, NewMaior),
    (X > NewMaior -> Maior = X, P = 1;
        Maior = NewMaior, P is Posi +1).

montaLista([],[],[]).
montaLista([L|LX], [P|PX], [N|NX]):-
    maiorLista(L,P,N),
    montaLista(LX,PX,NX).

main:-
    read(X),
    montaLista(X, P, N),
    maiorLista(N, Pos,_),
    Posicao is Pos -1,
    nth0(Posicao, P, R),
    write(Pos),
    write(R).

%shift pra esquerda
list_concat([],L,L).
list_concat([X1|L1],L2,[X1|L3]) :- list_concat(L1,L2,L3).
list_shift([Head|Tail],Shifted) :- list_concat(Tail, [Head],Shifted).

shift([H|T], S):-
    concanten(Tail, [Head],S).

concanten([],L,L).
concanten([X|L1], L2, [X|L3]):-
    concanten(L1,L2,L3).

%% fazendo do shift usando o append

shiftRight([X|Z], Result):-
    append(Z, [X], Result).



shiftTotal(X, 0, _, X).
shiftTotal([X|Z], Total, [X|A], Result) :-
    NewTotal is Total - 1,
    shiftTotal(Z, NewTotal, A, Result).

chama(L,T, A, R):-
    shiftTotal(L,T,A,NewR),
    append(NewR,A, R).

% Verifica se um elemento pertence a uma lista
pertence(X ,[X|_]).
pertence(X,[_|Y]):-
    pertence(X,Y).

% retorna o tamanho de uma lista
tamanho([], 0).
tamanho([L|LX], Tam):-
    tamanho(LX, NewTam),
    Tam is NewTam +1.

% Método que faz a lista ficar apenas com valores positivos.
abs(X, X).
abs(X, Y):-
    X < 0,
    Y is X * -1,!.

converte([],[]).
converte([X|Y], [Z|W]):-
    abs(X,Z),
    converte(Y,W).

% encontrar o ultimo elemento.

ultimo([X], X):-!.
ultimo([X|Y], R):-
    ultimo(Y,R).

%Remove um elemento da lista

removeElem(X, [X|Y], Y):-!.
removeElem(X, [E|Y], [E|Z]):-
    removeElem(X, Y, Z).

%Concatena duas listas

concatena([],Z, Z):-!.
concatena([X|Y],Z, [X|K]):-
    concatena(Y, Z, K).

%Verifica se uma string é palindromo

isPalindrome([]).
isPalindrome([_]).
isPalindrome([H|T]):-
    append(Middle, [H], T),
    write(Middle),
    write(' '),
    isPalindrome(Middle).

%remove todas as ocorrencias de um elemento na lista

removeAll(_, [], []).
removeAll(X, [X|Z], L):-
    removeAll(X,Z ,L).

removeAll(X, [Y|Z], [Y|K]):-
    X \= Y,
    removeAll(X,Z,K).

%Escreva um programa que encontra o k-elsimo elemento

kesimo([H|_], 1,H).
kesimo([_|T], I, R):-
    I1 is I -1,
    kesimo(T, I1, R),
    write(R).

%Mostra qual é o mair elemento

maior([X],X).

maior([X|Y],R):-
    maior(Y, NewR),
    write(X),
    write(' NEW ='),
    write(NewR),
    (NewR > X -> R = NewR ;
        R = X
    ),
    write(' R ='),
    write(R),
    write(' X').

%Reverte os elementos de uma lista 

reverteLista([],[]).
reverteLista([X|Y], R):-
    reverteLista(Y, NewR),
    write(' X ='),
    write([X]),
    write(' New ='),
    write(NewR),
    append(NewR,[X],R),
    write('  R   ='),
    writeln(R).

%Divide uma lista númerica em duas sublistas que ontenham os elementos
%menores ou iguais e amioes que um dadao elemento.

divide(X, [], [], []).
divide(X, [H|T], [Maior|M],N):-
    H >= X,
    Maior = H,
    divide(X, T, M,N).

divide(X, [H|T], M, [Menor|N]):-
    H < X,
    Menor = H,
    divide(X, T, M , N).

%Escreva um programa que verifica se uma lista é sublista de outra.
  
sublista([], _).
sublista(L2, L1):- 
    append(_, L3, L1),
    append(L2, _, L3).



is_sublist([], _).
is_sublist([X|Xs], [X|Ys]):- is_sublist(Xs, Ys).
is_sublist([X|Xs], [_|Ys]):- is_sublist([X|Xs], Ys).


flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

somaAte([], _, 0).
somaAte([H|T], Limite, Output) :- 
    somaAte(T, Limite, Resto),
    SomaTemp is H + Resto,
    SomaTemp =< Limite,
    Output is SomaTemp.
somaAte([_|T], Limite, Output) :- 
    somaAte(T, Limite, Output).

% [[1,2,3],[4,5,6]] ->> [1,2,3,4,5,6]

soma([], _, 0).
soma([H|T], Limite, Total):-
    soma(T, Limite, NewTotal),
    SomaTem is H + NewTotal,
    SomaTem =< Limite,
    Total = SomaTem.
soma([_|T], Limite, Total):-
    soma(T, Limite,Total).


