% decisao.pl
% pontuacao/3 e decisao/2

:- use_module(library(lists)).  % sum_list/2
:- [sinais].
:- [politicas].

% PONTUACAO/3
% pontuacao(ID, Score, Evidencias)
pontuacao(ID, Score, Evid) :-
    sinais(ID, Evid),
    findall(P, member((_Label, P), Evid), Pesos),
    (   Pesos == []
    ->  Score = 0
    ;   sum_list(Pesos, Score)
    ).

% DECISAO/2
% Prioridade: hardstop -> recusar
% Depois: score vs limiares (definidos em entrada.txt)
decisao(ID, recusar) :-
    hardstop(ID, _),
    !.

decisao(ID, aprovar) :-
    pontuacao(ID, Score, _),
    limiar_revisao(Lrev),
    Score < Lrev,
    !.

decisao(ID, recusar) :-
    pontuacao(ID, Score, _),
    limiar_recusa(Lrec),
    Score >= Lrec,
    !.

decisao(ID, revisar) :-
    pontuacao(ID, Score, _),
    limiar_revisao(Lrev),
    limiar_recusa(Lrec),
    Score >= Lrev,
    Score < Lrec.
