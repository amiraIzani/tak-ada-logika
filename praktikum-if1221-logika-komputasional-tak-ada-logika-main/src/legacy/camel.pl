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
next_pos('S','O'). 
next_pos('O','N'). 
next_pos('N','M'). 
next_pos('M','L').
next_pos('L','K'). 
next_pos('K','J'). 
next_pos('J','I').
next_pos('I','H'). 
next_pos('H','G'). 
next_pos('G','F').
next_pos('F','E'). 
next_pos('E','D').
next_pos('D','C').
next_pos('C','B').
next_pos('B','A').
next_pos('A','S').

% Position to coordinate mapping (clockwise)
konversi_koordinat('S', (1,1)). 
konversi_koordinat('O', (2,1)). 
konversi_koordinat('N', (3,1)).
konversi_koordinat('M', (4,1)). 
konversi_koordinat('L', (5,1)). 
konversi_koordinat('K', (5,2)).
konversi_koordinat('J', (5,3)). 
konversi_koordinat('I', (5,4)). 
konversi_koordinat('H', (5,5)).
konversi_koordinat('G', (4,5)). 
konversi_koordinat('F', (3,5)). 
konversi_koordinat('E', (2,5)).
konversi_koordinat('D', (1,5)). 
konversi_koordinat('C', (1,4)). 
konversi_koordinat('B', (1,3)).
konversi_koordinat('A', (1,2)).

% Start the game
startGame :-
    write('Masukkan jumlah pemain (2-4): '),
    read(Jumlah),
    ((Jumlah < 2; Jumlah > 4) ->
        (write('Jumlah pemain tidak valid!'), nl, fail);
        true),
    retractall(pemain(_, _, _, _)),
    inisiasi_pemain(Jumlah),
    tampilkan_urutan_pemain,
    tampilkan_atribut_pemain,
    buat_papan,
    buat_jejak_dadu,
    nl, write('Permainan dimulai!'), nl,
    jalankan_giliran.

% Starting over game with the same players and same data
restartGame :-
    % (sudah_mulai(_) -> write('Permainan sudah dimulai!'), nl;
    % (retractall(pemain(_, _, _, _)),
    tampilkan_urutan_pemain,
    tampilkan_atribut_pemain,
    buat_papan,
    buat_jejak_dadu,
    nl, write('Permainan dimulai!'), nl,
    jalankan_giliran.

inisiasi_pemain(0) :- !.
inisiasi_pemain(N) :-
    write('Masukkan nama pemain '), write(N), write(': '),
    read(Nama),
    (pemain(Nama, _, _, _) ->
        (write('Pemain sudah ada! Silakan masukkan nama lain.'), nl,
        inisiasi_pemain(N));
        (asserta(pemain(Nama, 30, [merah, kuning, hijau, biru], 1)),
        N1 is N - 1,
        inisiasi_pemain(N1))).

tampilkan_urutan_pemain :-
    findall(Nama, pemain(Nama, _, _, _), DaftarPemain),
    write('Urutan pemain: '), write(DaftarPemain), nl.

tampilkan_atribut_pemain :-
    write('Setiap pemain mendapatkan 4 kartu, 30 poin dan 1 trap.'), nl,
    forall(pemain(Nama, Poin, Kartu, Jebakan), (
        write('Kartu '), write(Nama), write(': '), write(Kartu), nl,
        write('Poin '), write(Nama), write(': '), write(Poin), nl,
        write('Trap '), write(Nama), write(': '), write(Jebakan), nl
    )).

% Initialize game state
buat_papan :-
    retractall(posisi(_, _)),
    retractall(sudah_mulai(_)),
    retractall(unta_beres(_)),
    retractall(putaran_unta(_, _)),
    retractall(dadu_sudah_keluar(_)),
    asserta(sudah_mulai(true)),
    asserta(posisi(merah, 'S')),
    asserta(posisi(kuning, 'S')),
    asserta(posisi(hijau, 'S')),
    asserta(posisi(biru, 'S')),
    asserta(posisi(putih, 'S')),
    asserta(putaran_unta(merah, 0)),
    asserta(putaran_unta(kuning, 0)),
    asserta(putaran_unta(hijau, 0)),
    asserta(putaran_unta(biru, 0)),
    asserta(putaran_unta(putih, 0)).

buat_jejak_dadu :-
    retractall(jebakan(_, _)).

% Helper predicate to rotate list
rotate_list([], []).
rotate_list([H|T], Rotated) :- 
    append(T, [H], Rotated).

% Main game loop
jalankan_giliran :-
    (unta_beres(_) -> akhiri_permainan;
    (urutan_player_sekarang(DaftarPemain) ->
        true
    ;
        findall(Nama, pemain(Nama, _, _, _), InitialOrder),
        asserta(urutan_player_sekarang(InitialOrder)),
        DaftarPemain = InitialOrder
    ),
    jalankan_giliran_pemain(DaftarPemain),
    (semua_dadu_sudah_keluar -> 
        rotate_list(DaftarPemain, RotatedOrder),
        retractall(urutan_player_sekarang(_)),
        asserta(urutan_player_sekarang(RotatedOrder)),
        reset_dadu
    ; 
        true
    ),
    jalankan_giliran).

jalankan_giliran_pemain([]).
jalankan_giliran_pemain([Nama|T]) :-
    (unta_beres(_) -> true;
    giliran_pemain(Nama),
    jalankan_giliran_pemain(T)).


% Player's turn
% Giliran pemain
% Player's turn
giliran_pemain(Nama) :-
    giliran_pemain_loop(Nama).

giliran_pemain_loop(Nama) :-
    write('Giliran '), write(Nama), write('. Pilih aksi:'), nl,
    write('1. Jalankan unta'), nl,
    write('2. Pasang jebakan'), nl,
    write('3. Investasi'), nl,
    write('4. Cek peta'), nl,
    write('5. Akhiri giliran'), nl,
    read(Pilihan),
    (integer(Pilihan), between(1, 5, Pilihan) ->
        aksi(Nama, Pilihan),
        ((Pilihan = 1; Pilihan = 2; Pilihan = 5) ->
            true  % Actions that end the turn
        ;
            giliran_pemain_loop(Nama)  % Continue turn
        )
    ;
        write('Pilihan tidak valid!'), nl,
        giliran_pemain_loop(Nama)
    ).
% Action handlers
aksi(Nama, 1) :- jalankan_unta(Nama).
aksi(Nama, 2) :- pasang_jebakan(Nama).
aksi(Nama, 3) :- lakukan_investasi(Nama).
aksi(_, 4) :- tampilkan_peta.
aksi(_, 5) :- write('Giliran berakhir.'), nl.

% Camel movement
jalankan_unta(Nama) :-
    (kocok_dadu(Warna, Langkah) ->
        posisi(Warna, PosisiAwal),
        write('Unta '), write(Warna), write(' di posisi '), write(PosisiAwal),
        write(' bergerak '), write(Langkah), write(' langkah.'), nl,
        move_camel(PosisiAwal, Langkah, PosisiBaru, PassedS),
        % Update lap count if camel passed 'S'
        putaran_unta(Warna, LapCount),
        ( PassedS, LapCount = 0 ->
            LapCount1 is LapCount + 1,
            retract(putaran_unta(Warna, LapCount)),
            asserta(putaran_unta(Warna, LapCount1)),
            write('Unta '), write(Warna), write(' telah menyelesaikan 1 putaran.'), nl,
            asserta(unta_beres(Warna))
        ; % else
            true
        ),
        % If camel has finished and is beyond 'S', set position to 'S'
        ( unta_beres(Warna), PosisiBaru \= 'S', PassedS ->
            NewPosisiBaru = 'S'
        ;
            NewPosisiBaru = PosisiBaru
        ),
        retract(posisi(Warna, PosisiAwal)),
        asserta(posisi(Warna, NewPosisiBaru)),
        tambahkan_poin(Nama, 10),
        write('Unta '), write(Warna), write(' sekarang di posisi '), write(NewPosisiBaru), nl
    ;
        write('Tidak ada unta yang tersedia untuk dijalankan.'), nl
    ).

% Move camel and check if 'S' was passed
move_camel(CurrentPos, 0, CurrentPos, false).
move_camel(CurrentPos, Steps, FinalPos, PassedS) :-
    Steps > 0,
    next_pos(CurrentPos, NextPos),
    Steps1 is Steps - 1,
    move_camel(NextPos, Steps1, FinalPos, PassedSRest),
    (NextPos = 'S' -> PassedS = true; PassedS = PassedSRest).

% Dice roll mechanism ensuring no camel moves twice per round
kocok_dadu(Warna, Langkah) :-
    findall(W, (posisi(W, _), \+ unta_beres(W), \+ dadu_sudah_keluar(W)), DaftarWarna),
    (DaftarWarna \= [] ->
        randomPermutation(DaftarWarna, WarnaAcak),
        WarnaAcak = [Warna|_],
        asserta(dadu_sudah_keluar(Warna)),
        randomBetween(1, 7, Langkah) % Dice roll between 1-6
    ;
        % All camels have moved this round, reset dice and start over
        reset_dadu,
        kocok_dadu(Warna, Langkah)
    ).

randomPermutation([], []).
randomPermutation(List, [H|T]) :-
    randomSelect(H, List, Rest),
    randomPermutation(Rest, T).

randomSelect(X, [X|Xs], Xs).
randomSelect(R, [X|Xs], [X|Ys]) :-
    randomSelect(R, Xs, Ys).

randomBetween(L, U, R) :-
    random(L, U, R).


% Check if all camels have moved in the round
semua_dadu_sudah_keluar :-
    findall(Warna, posisi(Warna, _), SemuaWarna),
    findall(W, dadu_sudah_keluar(W), WarnaKeluar),
    subset_custom(SemuaWarna, WarnaKeluar).

% Custom subset predicate
subset_custom([], _).
subset_custom([H|T], Set) :-
    member(H, Set),
    subset_custom(T, Set).

% Reset the dice for a new round
reset_dadu :-
    retractall(dadu_sudah_keluar(_)),
    write('Semua dadu telah dilempar. Memulai ronde baru.'), nl.

% Update player's points
tambahkan_poin(Nama, Poin) :-
    pemain(Nama, PoinSekarang, Kartu, Jebakan),
    PoinBaru is PoinSekarang + Poin,
    retract(pemain(Nama, PoinSekarang, Kartu, Jebakan)),
    asserta(pemain(Nama, PoinBaru, Kartu, Jebakan)).

% Place trap
pasang_jebakan(Nama) :-
    pemain(Nama, _, _, Jebakan),
    Jebakan > 0 ->
        write('Masukkan lokasi jebakan: '),
        read(Lokasi),
        write('Jenis jebakan (maju/mundur): '),
        read(Jenis),
        asserta(jebakan(Lokasi, Jenis)),
        JebakanBaru is Jebakan - 1,
        retract(pemain(Nama, Poin, Kartu, Jebakan)),
        asserta(pemain(Nama, Poin, Kartu, JebakanBaru)),
        write('Jebakan berhasil dipasang.'), nl
    ;
        write('Anda tidak memiliki jebakan.'), nl.

% Make investment
lakukan_investasi(Nama) :-
    write('Pilih unta untuk investasi (merah, kuning, hijau, biru): '),
    read(Warna),
    (\+ investasi(Nama, Warna, _) ->
        findall(Urutan, investasi(_, Warna, Urutan), Daftar),
        length(Daftar, Panjang),
        Urutan is Panjang + 1,
        asserta(investasi(Nama, Warna, Urutan)),
        write('Investasi pada '), write(Warna), write(' berhasil!'), nl;
        write('Anda sudah berinvestasi pada unta ini.'), nl).

% Display the map
tampilkan_peta :-
    write('Posisi unta di papan 5x5:'), nl,
    write('          1                 2               3                4                 5'), nl,
    write('  +----------------+----------------+----------------+----------------+----------------+'), nl,
    tampilkan_baris(1),
    write('  +----------------+----------------+----------------+----------------+----------------+'), nl,
    tampilkan_baris(2),
    write('  +----------------+----------------+----------------+----------------+----------------+'), nl,
    tampilkan_baris(3),
    write('  +----------------+----------------+----------------+----------------+----------------+'), nl,
    tampilkan_baris(4),
    write('  +----------------+----------------+----------------+----------------+----------------+'), nl,
    tampilkan_baris(5),
    write('  +----------------+----------------+----------------+----------------+----------------+'), nl.

tampilkan_baris(Y) :-
    write(Y), write('  '),
    tampilkan_kolom(Y, 1),
    tampilkan_kolom(Y, 2),
    tampilkan_kolom(Y, 3),
    tampilkan_kolom(Y, 4),
    tampilkan_kolom(Y, 5),
    write(' '), nl.

tampilkan_kolom(Y, X) :-
    write(' '),
    (is_valid_position(X, Y) ->
        tampilkan_isi(X, Y);
        write('       ')),
    write(' ').

tampilkan_isi(X, Y) :-
    findall(Unta, (posisi(Unta, Pos), konversi_koordinat(Pos, (X,Y))), Untas),
    (Untas = [] -> 
        write('       ');
        format_untas_spaced(Untas)
    ),
    findall((Lok,Jenis), (jebakan(Lok,Jenis), konversi_koordinat(Lok,(X,Y))), Traps),
    (Traps = [] -> 
        write(' ');
        [(_, Jenis)|_] = Traps,
        (Jenis = maju -> write('(>)');
         Jenis = mundur -> write('(<)'))
    ).

% Modified format_untas to handle spacing
format_untas_spaced([]).
format_untas_spaced([Unta|T]) :-
    unta_symbol(Unta, Symbol),
    write('['), write(Symbol), write(']'),
    (T = [] -> 
        write('   ');  % 3 spaces after last camel
        write(' '))
    ,
    format_untas_spaced(T).

% Symbol representation for camels
unta_symbol(merah, 'UM').
unta_symbol(kuning, 'UK').
unta_symbol(hijau, 'UH').
unta_symbol(biru, 'UB').
unta_symbol(putih, 'UP').

is_valid_position(X, Y) :-
    (Y = 1; Y = 5; X = 1; X = 5).

% End the game
akhiri_permainan :-
    findall(Warna, posisi(Warna, _), SemuaWarna),
    findall(W, unta_beres(W), FinishedCamels),
    % If at least one camel has finished, end the game
    (FinishedCamels \= [] ->
        write('Permainan berakhir! Unta-unta berikut telah mencapai finish: '), write(FinishedCamels), nl,
        hitung_pemenang,
        retractall(sudah_mulai(_));
        true).

% Calculate the winner
hitung_pemenang :-
    findall(Poin-Nama, (
        pemain(Nama, PoinAwal, _, _),
        findall(20, (
            investasi(Nama, Unta, _),
            unta_beres(Unta)
        ), Bonuses),
        sumList(Bonuses, TotalBonus),
        Poin is PoinAwal + TotalBonus
    ), Pairs),
    keysort(Pairs, Sorted),
    reverse(Sorted, [PoinMax-Pemenang|_]),
    write('Pemenangnya adalah '), write(Pemenang),
    write(' dengan '), write(PoinMax), write(' poin!'), nl.

% Helper predicates
sumList([], 0).
sumList([H|T], Sum) :-
    sumList(T, Rest),
    Sum is H + Rest.