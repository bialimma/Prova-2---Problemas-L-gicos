% politicas.pl
% Hard stops (regras eliminatórias)

:- [ontologia].
:- [metricas].

% HARDSTOP/2
% hardstop(ID, Motivo).

% 1) Idade mínima (< 18)
hardstop(ID, idade_minima) :-
    proposta(ID, Sol, _Prod, _Valor, _Prazo, _Taxa, _Gar),
    idade(Sol, Id),
    Id < 18.

% 2) Lista de sanções
hardstop(ID, sancao) :-
    proposta(ID, Sol, _Prod, _Valor, _Prazo, _Taxa, _Gar),
    em_lista_sancoes(Sol, sim).

% 3) LTV excedido (> 90%) em financiamento imobiliário
hardstop(ID, ltv_excedido) :-
    proposta(ID, _Sol, Prod, Valor, _Prazo, _Taxa, Gar),
    herda_trans(Prod, financiamento_imobiliario),
    ltv(Gar, Valor, LTV),
    LTV > 90.

% 4) Renda inválida (ausente ou <= 0)
hardstop(ID, renda_invalida) :-
    proposta(ID, Sol, _Prod, _Valor, _Prazo, _Taxa, _Gar),
    (
        \+ renda(Sol, _)
    ;
        renda(Sol, R), R =< 0
    ).
