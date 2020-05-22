len_row([], _).
len_row([Head | Tail], N) :-
    length(Head, N),
    len_row(Tail, N).

within_domain(Max, X):-
    fd_domain(X, 1, Max).

tower(N, T, C) :-
    length(T, N), len_row(T, N), 
    maplist(within_domain(N), T), 
    transpose(T, T_transposed),
    maplist(fd_all_different, T),
    maplist(fd_all_different, T_transposed),
    maplist(fd_labeling, T),
    check_count(T, C).

plain_tower(N, T, C) :-
    C = counts(Top,Bottom,Left,Right),
    length(T, N),
    findall(X, between(1, N, X), NumList),
    transpose(T, Transposed),
    length(Transposed, N),
    maplist(reverse, T, Reversed),
    maplist(reverse, Transposed, TransposedReversed),
    maplist(check(N, NumList), T, Left),
    maplist(check(N, NumList), Transposed, Top),
    maplist(check(N, NumList), Reversed, Right),
    maplist(check(N, NumList), TransposedReversed, Bottom).

check(N, NumList, Tower, Count) :-
    length(Tower, N),
    permutation(NumList, Tower),
    check_row(Tower, 0, Count).


check_left(Row, Count) :-
    check_row(Row, 0, Count).
check_row([], _, 0).
check_row([RowHead | RowTail], MaxHeight, Count):-
    RowHead < MaxHeight,
    check_row(RowTail, MaxHeight, Count).
check_row([RowHead | RowTail], MaxHeight, Count):-
    RowHead > MaxHeight,
    check_row(RowTail, RowHead, CountReturned),
    Count is (CountReturned+1).

reverse([],X,X).
reverse([Head|Tail],X,Result) :-
    reverse(Tail,X,[Head|Result]).

check_count(Towers, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(Left, N), length(Bottom, N),length(Top, N), length(Right, N),
    maplist(check_left, Towers, Left),
    transpose(Towers, TowersTransposed),
    maplist(check_left, TowersTransposed, Top),
    maplist(reverse, Towers, TowersReversed),
    maplist(check_left, TowersReversed, Right),
    maplist(reverse, TowersTransposed, TowersTransposedReversed),
    maplist(check_left, TowersTransposedReversed, Bottom).


tower_time(Time) :-
    statistics(cpu_time, [Start | _]),
    tower(4,_,counts([3,1,2,2],[2,3,1,3],[2,3,1,2],[3,1,2,2])),
    tower(4,[[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]],_),
    statistics(cpu_time, [End | _]), 
    Time is End - Start .

plain_tower_time(Time) :-
    statistics(cpu_time, [Start | _]),
    plain_tower(4,_,counts([3,1,2,2],[2,3,1,3],[2,3,1,2],[3,1,2,2])),
    plain_tower(4,[[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]],_),
    statistics(cpu_time, [End | _]),
    Time is End - Start .

speedup(Speedup) :-
    tower_time(Time),
    plain_tower_time(PlainTime),
    Speedup is PlainTime/Time .

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.

%   This is SWI-prolog's old implementation
%   https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%   Used helper code provided by TA Patricia Xiao: https://github.com/CS131-TA-team/UCLA_CS131_CodeHelp/blob/master/Prolog/sudoku_cell.pl