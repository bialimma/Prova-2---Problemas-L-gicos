% metricas.pl
% Cálculo de parcela, DTI, LTV e empacotamento em metricas/4

:- [ontologia].

% --------------------------------------
% PARCELA/4 - fórmula simplificada dada no enunciado (obs: não bate com os dados da "saida_esperada", ver com professor Celso"
% Prest = Valor * (TaxaMes + 1/Prazo)
% --------------------------------------
parcela(Valor, TaxaMes, Prazo, Prest) :-
    Prazo > 0,
    Fator is TaxaMes + 1.0 / Prazo,
    Prest is Valor * Fator.

% --------------------------------------
% DTI/3
% DTI = (Despesas + Parcela) / Renda * 100 (
% --------------------------------------
dti(Solicitante, Parcela, DTI) :-
    renda(Solicitante, Renda),
    Renda > 0,
    despesa(Solicitante, Desp),
    Soma is Desp + Parcela,
    DTI is Soma / Renda * 100.

% --------------------------------------
% LTV/3
% LTV = ValorEmprestimo / ValorGarantia * 100
% sem_garantia -> 0
% --------------------------------------
ltv(sem_garantia, _Valor, 0.0) :- !.
ltv(Garantia, Valor, LTV) :-
    valor_garantia(Garantia, ValGar),
    ValGar > 0,
    LTV is Valor / ValGar * 100.

% --------------------------------------
% METRICAS/4
% metricas(ID, dti(DTI), ltv(LTV), parcela(Prest))
% --------------------------------------
metricas(ID, dti(DTI), ltv(LTV), parcela(Prest)) :-
    proposta(ID, Sol, Prod, Valor, Prazo, Taxa, Garantia),
    parcela(Valor, Taxa, Prazo, Prest),
    dti(Sol, Prest, DTI),
    (
        herda_trans(Prod, financiamento_imobiliario)
    ->
        ltv(Garantia, Valor, LTV)
    ;
        LTV = 0.0
    ).
