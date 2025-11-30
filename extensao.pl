% extensao.pl
% Extensão: sugestão de contraproposta para reduzir DTI
%
% IMPORTANTE: Este predicado aceita qualquer melhoria no DTI (DTINovo < DTIOrig),
% não exigindo que o DTI atinja necessariamente o limiar ideal de 35%.
%
% Estratégia de ajuste:
% 1. Primeiro tenta reduzir o valor do empréstimo em 20%
% 2. Se não houver melhoria, tenta aumentar o prazo em 50% (via backtracking)
%
% Ambas estratégias só são aplicadas se:
% - DTI original > 35% (só tenta se for alto)
% - DTI novo < DTI original (deve melhorar)

:- [metricas].

% contraproposta(ID, ValorNovo, PrazoNovo, DTINovo, Ajuste)
% Tenta primeiro reduzir valor em 20%; se não melhorar o DTI, tenta aumentar prazo.

% 1) Tentar reduzir valor em 20%
contraproposta(ID, ValorNovo, PrazoNovo, DTINovo, Ajuste) :-
    proposta(ID, Sol, _Prod, ValorOrig, PrazoOrig, Taxa, _Gar),
    metricas(ID, dti(DTIOrig), _LTV, _Parc),
    DTIOrig > 35,                       % só tenta se DTI original for alto
    ValorNovo is ValorOrig * 0.8,
    PrazoNovo = PrazoOrig,
    parcela(ValorNovo, Taxa, PrazoNovo, PrestNova),
    dti(Sol, PrestNova, DTINovo),
    DTINovo < DTIOrig,                  % MUDANÇA: aceita qualquer melhoria (não exige <= 35)
    Ajuste = "reduzir valor em 20%".

% 2) Se não deu, tentar aumentar prazo em 50%
contraproposta(ID, ValorNovo, PrazoNovo, DTINovo, Ajuste) :-
    proposta(ID, Sol, _Prod, ValorOrig, PrazoOrig, Taxa, _Gar),
    metricas(ID, dti(DTIOrig), _LTV, _Parc),
    DTIOrig > 35,
    ValorNovo = ValorOrig,
    PrazoNovo is round(PrazoOrig * 1.5),
    parcela(ValorNovo, Taxa, PrazoNovo, PrestNova),
    dti(Sol, PrestNova, DTINovo),
    DTINovo < DTIOrig,                  % MUDANÇA: aceita qualquer melhoria (não exige <= 35)
    Ajuste = "aumentar prazo em 50%".