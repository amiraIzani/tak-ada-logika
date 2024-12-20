% :- include('fakta.pl').
% :- include('loop.pl').
% :- include('action.pl').
% :- include('game.pl').

% Random permutation helper
randomPermutation([], []).
randomPermutation(List, [H|T]) :-
    randomSelect(H, List, Rest),
    randomPermutation(Rest, T).

randomSelect(X, [X|Xs], Xs).
randomSelect(R, [X|Xs], [X|Ys]) :-
    randomSelect(R, Xs, Ys).

randomBetween(L, U, R) :-
    random(L, U, R).


% Convert linear position to 2D coordinates
konversi_koordinat(Pos, (X, Y)) :-
    PosisiKoordinat = [
        (1,1), (2,1), (3,1), (4,1), (5,1), % Baris 1
        (5,2), (5,3), (5,4), (5,5),        % Kolom 5 ke bawah
        (4,5), (3,5), (2,5), (1,5),        % Baris 5 ke kiri
        (1,4), (1,3), (1,2)                % Kolom 1 ke atas
    ],
    nth0(Pos, PosisiKoordinat, (X, Y)).

% Check if position is valid on the board
is_valid_position(X, Y) :-
    X >= 1, X =< 5,
    Y >= 1, Y =< 5.
