% Start the game
startGame :-
    write('Masukkan jumlah pemain (2-4): '),
    read(Jumlah),
    ((Jumlah < 2; Jumlah > 4) ->
        (write('Jumlah pemain tidak valid!'), nl, fail);
        true),
    retractall(pemain(_, _, _, _)), % Bersihkan pemain sebelumnya
    retractall(posisi(_, _)), % Bersihkan posisi unta sebelumnya
    retractall(dadu_sudah_keluar(_)), % Bersihkan status dadu
    inisiasi_pemain(Jumlah),
    tampilkan_urutan_pemain,
    tampilkan_atribut_pemain,
    buat_papan,
    nl, write('Permainan dimulai!'), nl,
    jalankan_putaran.

% Peta board permainan
peta([s, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]).

% Inisialisasi pemain
inisiasi_pemain(0) :- !.
inisiasi_pemain(N) :-
    write('Masukkan nama pemain '), write(N), write(': '),
    read(Nama),
    (   atom_chars(Nama, Chars), member(Char, Chars),
        char_code(Char, Code), Code >= 48, Code =< 57 % Cek kode ASCII
    ->  (write('Nama pemain tidak bisa mengandung angka! Silakan masukan nama lain.'), nl,
        inisiasi_pemain(N))
    ;   pemain(Nama, _, _, _) % Cek pemain duplikat
    ->  (write('Pemain sudah ada! Silakan masukkan nama lain.'), nl,
        inisiasi_pemain(N))
    ;   (asserta(pemain(Nama, 30, [merah, kuning, hijau, biru], 1)),
        N1 is N - 1,
        inisiasi_pemain(N1))).

% Tampilkan urutan pemain
tampilkan_urutan_pemain :-
    findall(Nama, pemain(Nama, _, _, _), DaftarPemain),
    write('Urutan pemain: '), write(DaftarPemain), nl.

% Tampilkan atribut pemain
tampilkan_atribut_pemain :-
    write('Setiap pemain mendapatkan 4 kartu, 30 poin, dan 1 trap.'), nl,
    forall(pemain(Nama, Poin, Kartu, Jebakan), (
        format('Kartu ~w: ~w~n', [Nama, Kartu]),
        format('Poin ~w: ~w~n', [Nama, Poin]),
        format('Trap ~w: ~w~n', [Nama, Jebakan])
    )).


% Buat papan permainan
buat_papan :-
    forall(member(Warna, [merah, kuning, hijau, biru, hitam, putih]), (
        asserta(posisi(Warna, 0))
    )).

% Jalankan putaran permainan
jalankan_putaran :-
    nl, write('--- Putaran Baru Dimulai ---'), nl,
    findall(Nama, pemain(Nama, _, _, _), DaftarPemain),
    rotate_list(DaftarPemain, DaftarPemainRotated),
    jalankan_giliran(DaftarPemainRotated),
    (   check_garis_finish -> true % Jika ada unta mencapai garis finish, permainan berhenti
    ;   jalankan_putaran      % Jika belum, lanjut ke putaran berikutnya
    ).

rotate_list(List, RotatedList) :-
    rotate_list_helper(List, [], RotatedList).

rotate_list_helper([], Acc, Acc).
rotate_list_helper([H|T], Acc, RotatedList) :-
    append(T, [H], NewList),
    rotate_list_helper(NewList, [H|Acc], RotatedList).

% Cek jika ada unta mencapai garis finish
check_garis_finish :-
    peta(Peta),
    length(Peta, Panjang),
    FinishIndex is Panjang - 1,
    (   (posisi(merah, FinishIndex), write('Unta merah melewati garis finish! Permainan berakhir.'), nl, !)
    ;   (posisi(kuning, FinishIndex), write('Unta kuning melewati garis finish! Permainan berakhir.'), nl, !)
    ;   (posisi(hijau, FinishIndex), write('Unta hijau melewati garis finish! Permainan berakhir.'), nl, !)
    ;   (posisi(biru, FinishIndex), write('Unta biru melewati garis finish! Permainan berakhir.'), nl, !)
    ).

% Jalankan giliran pemain
jalankan_giliran([]).
jalankan_giliran([Nama|Sisa]) :-
    format('Giliran pemain: ~w~n', [Nama]),
    write('Pilih aksi Anda: (1) Investasi, (2) Jalankan Unta, (3) Pasang Trap, (4) Tampilkan Peta, (5) Info Pemain, (6) Keluar: '),
    read(Aksi),
    (   pilih_aksi(Aksi, Nama)
    ->  (   (Aksi = 2; Aksi = 3) % Lanjutkan giliran hanya untuk jalankan_unta dan pasang_trap
        ->  tampilkan_posisi_unta, % Tampilkan posisi unta setelah setiap giliran
            jalankan_giliran(Sisa)
        ;   jalankan_giliran([Nama|Sisa]) % Tetap di giliran yang sama untuk aksi lainnya
        )
    ;   write('Aksi tidak valid! Silakan coba lagi.'), nl,
        jalankan_giliran([Nama])
    ).

% Pilih aksi pemain
pilih_aksi(1, Nama) :- investasi(Nama).
pilih_aksi(2, Nama) :- jalankan_unta(Nama).
pilih_aksi(3, Nama) :- pasang_trap(Nama).
pilih_aksi(4, _) :- tampilkan_peta.
pilih_aksi(5, Nama) :- tampilkan_atribut_pemain_sekarang(Nama).
pilih_aksi(6, _) :- write('Keluar  dari permainan.'), nl, halt.
pilih_aksi(_, _) :- fail.

tambahkan_poin(Nama, Poin) :-
    pemain(Nama, PoinSekarang, Kartu, Jebakan),
    PoinBaru is PoinSekarang + Poin,
    retract(pemain(Nama, PoinSekarang, Kartu, Jebakan)),
    asserta(pemain(Nama, PoinBaru, Kartu, Jebakan)).

% Aksi: jalankan unta
jalankan_unta(Nama) :-
    write('Mengocok dadu...'), nl,
    random(1, 7, AngkaMerah),
    random(1, 7, AngkaKuning),
    random(1, 7, AngkaHijau),
    random(1, 7, AngkaBiru),
    random(1, 7, AngkaHitamPutih),
    format('Dadu merah: ~w~n', [AngkaMerah]),
    format('Dadu kuning: ~w~n', [AngkaKuning]),
    format('Dadu hijau: ~w~n', [AngkaHijau]),
    format('Dadu biru: ~w~n', [AngkaBiru]),
    format('Dadu abu-abu: ~w~n', [AngkaHitamPutih]),
    update_posisi_unta(merah, AngkaMerah),
    update_posisi_unta(kuning, AngkaKuning),
    update_posisi_unta(hijau, AngkaHijau),
    update_posisi_unta(biru, AngkaBiru),
    update_posisi_hitam_putih(AngkaHitamPutih),
    tambahkan_poin(Nama,10).

% Update posisi unta
update_posisi_unta(Warna, Langkah) :-
    posisi(Warna, PosisiLama),
    NewPos is (PosisiLama + Langkah) mod 16,  % Ensure we stay within bounds
    retract(posisi(Warna, _)),
    asserta(posisi(Warna, NewPos)),
    format('Unta ~w bergerak ke posisi ~w~n', [Warna, NewPos]).

% Unta hitam-putih mundur
update_posisi_hitam_putih(Angka) :-
    peta(Peta),
    length(Peta, Panjang),
    (   Angka mod 2 =:= 1 -> % Hitam mundur jika ganjil
        posisi(hitam, PosisiLama),
        NewIndex is (PosisiLama - Angka + Panjang) mod Panjang,
        retract(posisi(hitam, _)),
        asserta(posisi(hitam, NewIndex)),
        format('Unta hitam mundur ~w langkah.~n', [Angka])
    ;   % Putih mundur jika genap
        posisi(putih, PosisiLama),
        NewIndex is (PosisiLama - Angka + Panjang) mod Panjang,
        retract(posisi(putih, _)),
        asserta(posisi(putih, NewIndex)),
        format('Unta putih mundur ~w langkah.~n', [Angka])
    ).

% Tampilkan posisi unta setelah setiap giliran
tampilkan_posisi_unta :-
    peta(Peta),
    write('Posisi unta saat ini: '), nl,
    forall(member(Warna, [merah, kuning, hijau, biru, hitam, putih]), (
        posisi(Warna, Posisi),
        nth0(Posisi, Peta, PosisiNama),
        format('Unta ~w berada di posisi ~w: ~w~n', [Warna, Posisi, PosisiNama])
    )).

% Aksi: pasang trap
pasang_trap(Nama) :-
    pemain(Nama, Poin, Kartu, Jebakan),
    (Jebakan > 0 ->
        write('Pasang trap di posisi mana? '),
        read(Lokasi),
        (get_koordinat(Lokasi, X, Y), posisi(_, pos), get_koordinat(pos, X, Y)
        ->  (write('Terdapat unta pada lokasi pemasangan'), nl,
            pasang_trap(Nama))
        ;   write('Jenis jebakan (maju/mundur): '),
            read(Jenis),
            asserta(jebakan(Lokasi, Jenis)),
            JebakanBaru is Jebakan - 1,
            retract(pemain(Nama, Poin, Kartu, Jebakan)),
            asserta(pemain(Nama, Poin, Kartu, JebakanBaru)),
            format('~w memasang trap di posisi ~w.~n', [Nama, Lokasi]), nl,
            write('Sisa trap '), write(Nama), write(': '), write(JebakanBaru), write('.'), nl
            )
    ;
        write('Anda tidak memiliki jebakan.'), nl,
        write('Sisa trap '), write(Nama), write(': '), write(Jebakan), write('.'), nl
    ).

% Aksi: investasi
investasi(Nama) :-
    write('Pilih unta untuk diinvestasikan (merah/kuning/hijau/biru): '),
    read(WarnaUnta),
    format('~w berinvestasi pada unta ~w.~n', [Nama, WarnaUnta]).

tampilkan_atribut_pemain_sekarang(Nama) :-
    pemain(Nama, Poin, Kartu, Jebakan),
    format('Kartu ~w: ~w~n', [Nama, Kartu]),
    format('Poin ~w: ~w~n', [Nama, Poin]),
    format('Trap ~w: ~w~n', [Nama, Jebakan]).

% Display the map
tampilkan_peta :-
    write('Posisi unta di papan 5x5:'), nl,
    write('              1                        2                       3                       4                       5'), nl,
    write('  +---------------------------+---------------------------+---------------------------+---------------------------+---------------------------+'), nl,
    tampilkan_baris(1),
    write('  +---------------------------+---------------------------+---------------------------+---------------------------+---------------------------+'), nl,
    tampilkan_baris(2),
    write('  +---------------------------+---------------------------+---------------------------+---------------------------+---------------------------+'), nl,
    tampilkan_baris(3),
    write('  +---------------------------+---------------------------+---------------------------+---------------------------+---------------------------+'), nl,
    tampilkan_baris(4),
    write('  +---------------------------+---------------------------+---------------------------+---------------------------+---------------------------+'), nl,
    tampilkan_baris(5),
    write('  +---------------------------+---------------------------+---------------------------+---------------------------+---------------------------+'), nl.

tampilkan_baris(Y) :-
    write(Y), write('  '),
    tampilkan_kolom(Y, 1),
    tampilkan_kolom(Y, 2),
    tampilkan_kolom(Y, 3),
    tampilkan_kolom(Y, 4),
    tampilkan_kolom(Y, 5),
    write(' '), nl.

% Modified tampilkan_kolom for consistent spacing
tampilkan_kolom(Y, X) :-
    write('|'),
    (is_valid_position(X, Y) ->
        tampilkan_isi(X, Y);
        write('                           ')),
    write('').

% Ensure only one definition of tampilkan_isi/2
tampilkan_isi(X, Y) :-
    findall(Symbol, (
        posisi(Unta, Pos),
        get_koordinat(Pos, X, Y),
        unta_symbol(Unta, Symbol)
    ), Symbols),
    (   Symbols \= []
    ->  format_untas_spaced(Symbols)
    ;   findall(Symbol, (
            jebakan(Lokasi, Jenis),
            get_koordinat(Lokasi, X, Y),
            trap_symbol(Jenis, Symbol)
        ), Symbols),
        (   Symbols \= []
        ->  format_traps_spaced(Symbols))
        ; write('                           ')
    ).

% Modified format_traps_spaced to write symbols separated by commas without using atomic_list_concat/3
format_traps_spaced(Symbols) :-
    write('       '),
    write_symbols(Symbols),
    write('        ').

% Modified format_untas_spaced to write symbols separated by commas without using atomic_list_concat/3
format_untas_spaced(Symbols) :-
    write('         '),
    write_symbols(Symbols),
    write('         ').

% Helper predicate to write symbols separated by commas
write_symbols([]).
write_symbols([Symbol]) :-
    write(Symbol).
write_symbols([Symbol|Rest]) :-
    write(Symbol),
    write(','),
    write_symbols(Rest).

% Correct mapping in get_koordinat/3
get_koordinat(Pos, X, Y) :-
    (Pos =:= 0 -> X = 1, Y = 1;      % Start position (s)
    Pos =:= 1 -> X = 2, Y = 1;       % Position a
    Pos =:= 2 -> X = 3, Y = 1;       % Position b
    Pos =:= 3 -> X = 4, Y = 1;       % Position c
    Pos =:= 4 -> X = 5, Y = 1;       % Position d
    Pos =:= 5 -> X = 5, Y = 2;       % Position e
    Pos =:= 6 -> X = 5, Y = 3;       % Position f
    Pos =:= 7 -> X = 5, Y = 4;       % Position g
    Pos =:= 8 -> X = 5, Y = 5;       % Position h
    Pos =:= 9 -> X = 4, Y = 5;       % Position i
    Pos =:=10 -> X = 3, Y = 5;       % Position j
    Pos =:=11 -> X = 2, Y = 5;       % Position k
    Pos =:=12 -> X = 1, Y = 5;       % Position l
    Pos =:=13 -> X = 1, Y = 4;       % Position m
    Pos =:=14 -> X = 1, Y = 3;       % Position n
    Pos =:=15 -> X = 1, Y = 2).      % Position o

% Fungsi untuk menulis spasi
repeat_space(0).
repeat_space(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    repeat_space(N1).

% Define camel symbols with full labels
unta_symbol(merah, '[UM]').
unta_symbol(kuning, '[UK]').
unta_symbol(hijau, '[UH]').
unta_symbol(biru, '[UB]').
unta_symbol(hitam, '[UI]').
unta_symbol(putih, '[UP]').

% Perbaiki fungsi is_valid_position untuk path unta
is_valid_position(X, Y) :-
    (Y = 1, X >= 1, X =< 5);    % Top row
    (X = 5, Y >= 1, Y =< 5);    % Right column
    (Y = 5, X >= 1, X =< 5);    % Bottom row
    (X = 1, Y >= 1, Y =< 5).    % Left column

% Debug helper - tambahkan ini untuk debugging
debug_posisi :-
    write('Debug Posisi Unta:'), nl,
    forall(posisi(Warna, Pos), (
        get_koordinat(Pos, X, Y),
        format('Unta ~w: Pos=~w, (X,Y)=(~w,~w)~n', [Warna, Pos, X, Y])
    )).

% Hapus definisi berikut karena sudah dipindahkan ke utils.pl
% konversi_koordinat/2 dan is_valid_position/2
