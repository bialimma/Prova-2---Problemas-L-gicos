% sinais.pl
% Classificação de DTI (lim/2) e sinais/3, sinais/2

:- [ontologia].
:- [metricas].

% -------------------------------
% LIM/2 - qualificação do DTI
% -------------------------------
lim(DTI, bom)  :- DTI =< 25.
lim(DTI, ok)   :- DTI > 25, DTI =< 35.
lim(DTI, alto) :- DTI > 35, DTI =< 45.
lim(DTI, ruim) :- DTI > 45.

% -------------------------------
% SINAL/3
% sinal(ID, Label, Peso)
% -------------------------------

% 1) Sinais de DTI
sinal(ID, dti_bom, -20) :-
    metricas(ID, dti(DTI), _LTV, _Parc),
    lim(DTI, bom).

sinal(ID, dti_ok, -10) :-
    metricas(ID, dti(DTI), _LTV, _Parc),
    lim(DTI, ok).

sinal(ID, dti_alto, 15) :-
    metricas(ID, dti(DTI), _LTV, _Parc),
    lim(DTI, alto).

sinal(ID, dti_ruim, 30) :-
    metricas(ID, dti(DTI), _LTV, _Parc),
    lim(DTI, ruim).

% 2) Sinais de LTV (somente financiamentos imobiliários)
sinal(ID, ltv_saude, -15) :-
    proposta(ID, _Sol, Prod, _V, _Pz, _Tx, _Gar),
    herda_trans(Prod, financiamento_imobiliario),
    metricas(ID, _DTI, ltv(LTV), _Parc),
    LTV =< 70.

sinal(ID, ltv_medio, 5) :-
    proposta(ID, _Sol, Prod, _V, _Pz, _Tx, _Gar),
    herda_trans(Prod, financiamento_imobiliario),
    metricas(ID, _DTI, ltv(LTV), _Parc),
    LTV > 70, LTV =< 85.

sinal(ID, ltv_limite, 15) :-
    proposta(ID, _Sol, Prod, _V, _Pz, _Tx, _Gar),
    herda_trans(Prod, financiamento_imobiliario),
    metricas(ID, _DTI, ltv(LTV), _Parc),
    LTV > 85, LTV =< 90.

% 3) Score de bureau
sinal(ID, bureau_excelente, -25) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    score_bureau(Sol, Score),
    Score >= 750.

sinal(ID, bureau_medio, 10) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    score_bureau(Sol, Score),
    Score >= 600, Score =< 749.

sinal(ID, bureau_baixo, 25) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    score_bureau(Sol, Score),
    Score < 600.

% 4) Comportamento (atrasos / consultas)
sinal(ID, atrasos_rec, 20) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    atrasos_12m(Sol, A),
    A >= 2.

sinal(ID, consultas_alta, 10) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    consultas_30d(Sol, C),
    C >= 3.

% 5) Emprego
sinal(ID, emprego_estavel, -10) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    tempo_emprego(Sol, _Emp, Meses),
    Meses >= 24.

sinal(ID, emprego_recente, 8) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    tempo_emprego(Sol, _Emp, Meses),
    Meses < 12.

% 6) Compostos

% Stress de parcela em crédito pessoal
sinal(ID, stress_parcela_pessoal, 15) :-
    proposta(ID, _Sol, Prod, _V, _Pz, _Tx, _Gar),
    herda_trans(Prod, credito_pessoal),
    metricas(ID, dti(DTI), _LTV, _Parc),
    DTI >= 35.

% Perfil premium: DTI muito baixo + score alto
sinal(ID, perfil_premium, -15) :-
    proposta(ID, Sol, _Prod, _V, _Pz, _Tx, _Gar),
    metricas(ID, dti(DTI), _LTV, _Parc),
    DTI =< 25,
    score_bureau(Sol, Score),
    Score >= 780.

% -------------------------------
% SINAIS/2 - coleta todos
% -------------------------------
sinais(ID, Lista) :-
    findall((Label, Peso), sinal(ID, Label, Peso), Lista).
