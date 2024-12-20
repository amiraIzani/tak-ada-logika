% :- include('game.pl').
% :- include('loop.pl').
% :- include('action.pl').
% :- include('utils.pl').

% Facts and dynamic predicates
:- dynamic(posisi/2). % (Warna, Posisi)
:- dynamic(pemain/4). % (Nama, Poin, Kartu, Jebakan)
:- dynamic(investasi/3). % (Pemain, Warna, Urutan)
:- dynamic(jebakan/2). % (Lokasi, Jenis)
:- dynamic(sudah_mulai/1). % Flag to check if the game has started
:- dynamic(unta_beres/1). % List of camels that have finished
:- dynamic(putaran_unta/2). % Number of laps completed by each camel
:- dynamic(dadu_sudah_keluar/1). % List of camels that have moved in the current round
:- dynamic(urutan_player_sekarang/1). % List of players in order of turns

% Edge movement mapping - clockwise from 'S'
pos_next('S', 'A'). 
pos_next('A', 'B'). 
pos_next('B', 'C'). 
pos_next('C', 'D').
pos_next('D', 'E'). 
pos_next('E', 'F'). 
pos_next('F', 'G').
pos_next('G', 'H'). 
pos_next('H', 'I'). 
pos_next('I', 'J').
pos_next('J', 'K'). 
pos_next('K', 'L').
pos_next('L', 'M').
pos_next('M', 'N').
pos_next('N', 'O').
pos_next('O', 'S').

% Symbol representation for traps
trap_symbol(maju, '(Trap>)').
trap_symbol(mundur, '(<Trap)').

% Symbol representation for camels
unta_symbol(merah, '[UM]').
unta_symbol(kuning, '[UY]').
unta_symbol(hijau, '[UH]').
unta_symbol(biru, '[UB]').
unta_symbol(hitam, '[UK]').
unta_symbol(putih, '[UP]').

% Helper predicates
sumList([], 0).
sumList([H|T], Sum) :-
    sumList(T, Rest),
    Sum is H + Rest.

% Custom subset predicate
subset_custom([], _).
subset_custom([H|T], Set) :-
    member(H, Set),
    subset_custom(T, Set).

% Helper predicate to rotate list
rotate_list([], []).
rotate_list([H|T], Rotated) :- 
    append(T, [H], Rotated).
