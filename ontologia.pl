% ontologia.pl
% Regras de herança sobre os fatos classe/1 e herda/2 definidos em entrada.txt.

% 1) Toda classe herda dela mesma (reflexiva)
herda_trans(C, C) :- classe(C).

% 2) Herança direta
herda_trans(Filho, Pai) :-
    herda(Filho, Pai).

% 3) Herança transitiva (avô, bisavô, ...)
herda_trans(Filho, Avo) :-
    herda(Filho, Pai),
    herda_trans(Pai, Avo).