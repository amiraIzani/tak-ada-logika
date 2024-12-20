% :- include('fakta.pl').
% :- include('loop.pl').
% :- include('game.pl').
% :- include('utils.pl').

% Giliran pemain utama
main_giliran_pemain :-
    findall(Nama, pemain(Nama, _, _, _), DaftarPemain),
    jalankan_giliran_semua_pemain(DaftarPemain).

% Memastikan semua pemain mendapatkan giliran sebelum ronde baru
jalankan_giliran_semua_pemain([]) :-
    write('Semua pemain telah menyelesaikan giliran. Memulai ronde baru!'), nl,
    reset_dadu,
    main_giliran_pemain.
jalankan_giliran_semua_pemain([Nama|SisaPemain]) :-
    giliran_pemain(Nama),
    jalankan_giliran_semua_pemain(SisaPemain).

% Giliran pemain individu
giliran_pemain(Nama) :-
    write('Giliran '), write(Nama), write('. Pilih aksi:'), nl,
    write('1. Jalankan unta'), nl,
    write('2. Pasang jebakan'), nl,
    write('3. Investasi'), nl,
    write('4. Cek peta (bisa dilakukan tanpa batas)'), nl,
    write('5. Tampilkan pemain (bisa dilakukan tanpa batas)'), nl,
    read(Pilihan),
    (integer(Pilihan), between(1, 5, Pilihan) ->
        aksi(Nama, Pilihan)
    ;
        write('Pilihan tidak valid!'), nl,
        giliran_pemain(Nama)
    ).

% Aksi pemain
aksi(Nama, 1) :- jalankan_unta(Nama).
aksi(Nama, 2) :- pasang_jebakan(Nama).
aksi(Nama, 3) :- lakukan_investasi(Nama).
aksi(_, 4) :-
    tampilkan_peta,
    giliran_pemain(_).  % Tidak menghabiskan giliran
aksi(_, 5) :-
    tampilkan_atribut_pemain,
    giliran_pemain(_).  % Tidak menghabiskan giliran

% Camel movement
jalankan_unta(Nama) :-
    (kocok_dadu(Warna, Langkah) ->
        posisi(Warna, PosisiAwal),
        write('Unta '), write(Warna), write(' di posisi '), write(PosisiAwal),
        write(' bergerak '), write(Langkah), write(' langkah.'), nl,
        pindahkan_tumpukan(Warna, Langkah),
        tambahkan_poin(Nama, 10)
    ;
        write('Tidak ada unta yang tersedia untuk dijalankan.'), nl
    ).

% Pindahkan seluruh tumpukan unta jika diperlukan
pindahkan_tumpukan(Warna, Langkah) :-
    posisi(Warna, PosisiAwal),
    tumpukan(PosisiAwal, Tumpukan),
    pisahkan_tumpukan(Warna, Tumpukan, TumpukanDiAtas, TumpukanDiBawah),
    (TumpukanDiAtas = [] -> % Jika tidak ada unta di atas
        NewTumpukan = [Warna|TumpukanDiBawah]
    ;
        NewTumpukan = [Warna|TumpukanDiAtas] % Pindahkan unta bersama yang di atasnya
    ),
    (Warna = hitam ; Warna = putih -> % Logika khusus untuk unta hitam dan putih
        (Langkah > 0 -> % Jika maju, validasi blok tujuan
            PosisiBaruSementara is PosisiAwal + Langkah,
            (validasi_posisi(PosisiBaruSementara, PosisiBaru) ->
                update_posisi_tumpukan(NewTumpukan, PosisiAwal, PosisiBaru)
            ;
                write('Unta '), write(Warna), write(' tidak dapat bergerak ke S/F block.'), nl
            )
        ;
            LangkahNegatif is abs(Langkah), % Jika mundur
            PosisiBaruSementara is PosisiAwal - LangkahNegatif,
            (PosisiBaruSementara >= 0 ->
                update_posisi_tumpukan(NewTumpukan, PosisiAwal, PosisiBaruSementara)
            ;
                write('Unta '), write(Warna), write(' tidak dapat bergerak lebih mundur dari block A.'), nl
            )
        )
    ;
        PosisiBaruSementara is PosisiAwal + Langkah,
        validasi_posisi(PosisiBaruSementara, PosisiBaru),
        update_posisi_tumpukan(NewTumpukan, PosisiAwal, PosisiBaru)
    ).

% Pisahkan tumpukan menjadi unta di atas dan di bawah unta yang akan bergerak
pisahkan_tumpukan(Warna, [Warna|Ekor], [], Ekor) :- !.
pisahkan_tumpukan(Warna, [Unta|Ekor], [Unta|TumpukanDiAtas], TumpukanDiBawah) :-
    pisahkan_tumpukan(Warna, Ekor, TumpukanDiAtas, TumpukanDiBawah).

% Validasi posisi baru (unta tidak boleh berada di S atau F)
validasi_posisi(Posisi, Posisi) :-
    Posisi \= 'S', Posisi \= 'F', !.
validasi_posisi(_, _) :-
    fail.

% Update posisi tumpukan
update_posisi_tumpukan(Tumpukan, PosisiAwal, PosisiBaru) :-
    retract(tumpukan(PosisiAwal, Lama)),
    subtract(Lama, Tumpukan, Sisa),
    (Sisa = [] -> retract(posisi(_, PosisiAwal)) ; asserta(tumpukan(PosisiAwal, Sisa))),
    (tumpukan(PosisiBaru, TumpukanLama) ->
        append(Tumpukan, TumpukanLama, TumpukanBaru),
        retract(tumpukan(PosisiBaru, _)),
        asserta(tumpukan(PosisiBaru, TumpukanBaru))
    ;
        asserta(tumpukan(PosisiBaru, Tumpukan))),
    update_posisi_berurutan(Tumpukan, PosisiBaru).

% Update posisi tiap unta dalam tumpukan
update_posisi_berurutan([], _).
update_posisi_berurutan([Warna|Ekor], Posisi) :-
    retract(posisi(Warna, _)),
    asserta(posisi(Warna, Posisi)),
    update_posisi_berurutan(Ekor, Posisi).

% Tambahkan poin ke pemain
tambahkan_poin(Nama, PoinTambahan) :-
    pemain(Nama, Poin, Kartu, Jebakan),
    PoinBaru is Poin + PoinTambahan,
    retract(pemain(Nama, Poin, Kartu, Jebakan)),
    asserta(pemain(Nama, PoinBaru, Kartu, Jebakan)),
    write('Poin '), write(Nama), write(' bertambah menjadi '), write(PoinBaru), nl.


% Pasang jebakan
pasang_jebakan(Nama) :-
    pemain(Nama, Poin, Kartu, Jebakan),
    (Jebakan > 0 ->
        write('Masukkan lokasi jebakan: '),
        read(Lokasi),
        write('Jenis jebakan (maju/mundur): '),
        read(Jenis),
        asserta(jebakan(Lokasi, Jenis)),
        JebakanBaru is Jebakan - 1,
        retract(pemain(Nama, Poin, Kartu, Jebakan)),
        asserta(pemain(Nama, Poin, Kartu, JebakanBaru)),
        write('Jebakan berhasil dipasang.'), nl,
        write('Sisa trap '), write(Nama), write(': '), write(JebakanBaru), write('.'), nl
    ;
        write('Anda tidak memiliki jebakan.'), nl,
        write('Sisa trap '), write(Nama), write(': '), write(Jebakan), write('.'), nl
    ).

% Aksi: investasi
investasi(Nama) :-
    kartu(Nama, Kartu),
    (   Kartu == [] ->
        write('Anda tidak memiliki kartu. Tidak dapat berinvestasi.'), nl,
        jalankan_giliran([Nama])
    ;
        write('Kartu yang Anda miliki saat ini: '), write(Kartu), nl,
        write('Pilih unta untuk diinvestasikan: '), 
        read(WarnaUnta),
        (   member(WarnaUnta, Kartu) -> % Periksa apakah warna ada dalam kartu
            lakukan_investasi(Nama, WarnaUnta)
        ;
            write('Anda tidak memiliki kartu dengan warna tersebut.'), nl,
            jalankan_giliran([Nama])
        )
    ).

% Investasi
lakukan_investasi(Nama, Warna) :-
    (   \+ investasi(Nama, Warna, _) -> 
        findall(Urutan, investasi(_, Warna, Urutan), Daftar),
        length(Daftar, Panjang),
        Urutan is Panjang + 1,
        asserta(investasi(Nama, Warna, Urutan)),  
        hapus_kartu(Nama, Warna),                
        write('Investasi pada '), write(Warna), write(' berhasil!'), nl,
        tampilkan_investasi(Warna)              
    ).


% Hapus kartu yang sesuai dari pemain
hapus_kartu(Nama, Warna) :-
    kartu(Nama, Kartu),
    select(Warna, Kartu, KartuBaru), 
    retract(kartu(Nama, Kartu)),
    assertz(kartu(Nama, KartuBaru)).

% Rule untuk menampilkan investasi pada warna tertentu
tampilkan_investasi(Warna) :-
    findall((Urutan, Nama), investasi(Nama, Warna, Urutan), DaftarInvestasi),
    sort(DaftarInvestasi, DaftarInvestasiTerurut),  % Mengurutkan daftar berdasarkan Urutan
    format('Papan investasi pada unta ~w:~n', [Warna]),
    write('+---------------------+'), nl,
    write('|   INVESTASI UNTA    |'), nl,
    write('+---------------------+'), nl,
    (   DaftarInvestasiTerurut = [] ->
        write('Belum ada investasi'), nl
    ;
        forall(member((Urutan, Nama), DaftarInvestasiTerurut),
               format('~w. ~w~n', [Urutan, Nama]))
    ),
    nl.


% hitung pemenang
hitung_pemenang :-
    findall(PoinTotal-Nama, (
        pemain(Nama, PoinAwal, _),
        hitung_bonus(Nama, BonusTotal),
        PoinTotal is PoinAwal + BonusTotal
    ), DaftarPoin),
    keysort(DaftarPoin, DaftarPoinUrut),
    reverse(DaftarPoinUrut, [PoinMax-Pemenang|_]),
    write('Pemenangnya adalah '), write(Pemenang),
    write(' dengan '), write(PoinMax), write(' poin!'), nl.

hitung_bonus(Nama, BonusTotal) :-
    findall(Bonus, (
        investasi(Nama, Warna, UrutanInvestasi),
        unta_beres(Warna, UrutanFinish),
        poin_berdasarkan_urutan(UrutanFinish, PoinPerInvestasi),
        nth1(UrutanInvestasi, PoinPerInvestasi, Bonus)
    ), SemuaBonus),
    sum_list(SemuaBonus, BonusTotal).


% Reset dadu untuk ronde baru
reset_dadu :-
    retractall(dadu_sudah_keluar(_)),
    retractall(unta_beres(_)),
    write('Semua dadu telah direset untuk ronde baru.'), nl.

% Daftar warna unta
warna_unta([merah, kuning, biru, hijau]).

% Rule untuk menampilkan papan investasi untuk semua warna
papan_investasi :-
    warna_unta(WarnaList),
    forall(member(Warna, WarnaList),
           tampilkan_investasi(Warna)).
