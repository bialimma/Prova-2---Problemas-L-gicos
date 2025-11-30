% principal.pl
% Carrega entrada.txt, módulos e gera saida.txt com análise das propostas

:- consult('entrada.txt').
:- [ontologia, metricas, politicas, sinais, decisao, explicacao, extensao].

% -------------------------
% Formatação auxiliar
% -------------------------

imprime_cabecalho :-
    writeln("=== SISTEMA DE CONCESSÃO DE CRÉDITO ==="),
    nl.

imprime_cabecalho_proposta(ID) :-
    upcase_atom(ID, IDUpper),
    format("=== PROPOSTA ~w ===~n", [IDUpper]).

imprime_linha_solicitante(Sol) :-
    idade(Sol, Id),
    format("Solicitante: ~w (~d anos)~n", [Sol, Id]).

imprime_linha_produto(Prod) :-
    format("Produto: ~w~n", [Prod]).

imprime_linha_valor(Valor) :-
    format("Valor: R$ ~2f~n", [Valor]).

imprime_linha_prazo(Prazo) :-
    format("Prazo: ~d meses~n", [Prazo]).

imprime_linha_taxa(Taxa) :-
    TxPerc is Taxa * 100,
    format("Taxa: ~1f%% a.m.~n", [TxPerc]).

imprime_linha_garantia(sem_garantia) :-
    writeln("Garantia: sem garantia").
imprime_linha_garantia(Gar) :-
    Gar \= sem_garantia,
    valor_garantia(Gar, VGar),
    format("Garantia: ~w (R$ ~2f)~n", [Gar, VGar]).

imprime_metricas(ID) :-
    metricas(ID, dti(DTI), ltv(LTV), parcela(Prest)),
    writeln(""),
    writeln("MÉTRICAS CALCULADAS:"),
    format("  Parcela estimada: R$ ~2f~n", [Prest]),
    format("  DTI (Debt-to-Income): ~1f%%~n", [DTI]),
    format("  LTV (Loan-to-Value): ~1f%%~n", [LTV]).

imprime_sinais(ID) :-
    sinais(ID, Lista),
    nl,
    writeln("SINAIS DETECTADOS:"),
    (   Lista = []
    ->  writeln("  (nenhum sinal)")
    ;   forall(
            member((Label, Peso), Lista),
            (   (Peso >= 0 -> Sinal = "+" ; Sinal = ""),
                format("  [~w~d] ~w~n", [Sinal, Peso, Label])
            )
        )
    ).

imprime_pontuacao_decisao(ID) :-
    pontuacao(ID, Score, _),
    decisao(ID, Dec),
    nl,
    format("PONTUAÇÃO TOTAL: ~d~n", [Score]),
    upcase_atom(Dec, DecUpper),
    format("DECISÃO: ~w~n", [DecUpper]).

imprime_motivos(ID) :-
    motivos(ID, Motivos),
    writeln("MOTIVOS:"),
    (   Motivos = []
    ->  writeln("  (sem motivos registrados)")
    ;   forall(
            member(M, Motivos),
            format("  - ~s~n", [M])
        )
    ).

% Caso com hard stop: versão simplificada
imprimir_proposta_com_hardstop(ID, Sol, Prod, Valor, Prazo, Taxa, Gar) :-
    imprime_cabecalho_proposta(ID),
    imprime_linha_solicitante(Sol),
    imprime_linha_produto(Prod),
    imprime_linha_valor(Valor),
    imprime_linha_prazo(Prazo),
    imprime_linha_taxa(Taxa),
    imprime_linha_garantia(Gar),
    nl,
    findall(H, hardstop(ID, H), [HPrimeiro|_]),
    rotulo_hard(HPrimeiro, Msg),
    format("HARD STOP DETECTADO: ~w~n", [HPrimeiro]),
    writeln("DECISÃO: RECUSAR"),
    format("MOTIVO: ~s~n~n", [Msg]).

% Caso normal: métricas + sinais + decisão
imprimir_proposta_normal(ID, Sol, Prod, Valor, Prazo, Taxa, Gar) :-
    imprime_cabecalho_proposta(ID),
    imprime_linha_solicitante(Sol),
    imprime_linha_produto(Prod),
    imprime_linha_valor(Valor),
    imprime_linha_prazo(Prazo),
    imprime_linha_taxa(Taxa),
    imprime_linha_garantia(Gar),
    imprime_metricas(ID),
    imprime_sinais(ID),
    imprime_pontuacao_decisao(ID),
    imprime_motivos(ID),
    nl.

% Impressão de uma proposta (decide se tem hardstop)
imprimir_analise_proposta(ID) :-
    proposta(ID, Sol, Prod, Valor, Prazo, Taxa, Gar),
    (   hardstop(ID, _)
    ->  imprimir_proposta_com_hardstop(ID, Sol, Prod, Valor, Prazo, Taxa, Gar)
    ;   imprimir_proposta_normal(ID, Sol, Prod, Valor, Prazo, Taxa, Gar)
    ).

% Resumo geral
imprime_resumo :-
    findall(ID, proposta(ID, _Sol, _Prod, _V, _P, _T, _G), IDs),
    findall(D, (member(ID, IDs), decisao(ID, D)), Decisoes),
    include(==(aprovar), Decisoes, As), length(As, NA),
    include(==(revisar), Decisoes, Rs), length(Rs, NR),
    include(==(recusar), Decisoes, Cs), length(Cs, NC),
    % Hard stops (IDs únicos)
    (   setof(IDHS, M^hardstop(IDHS, M), IDsHS)
    ->  length(IDsHS, NHS)
    ;   NHS = 0
    ),
    length(IDs, Total),
    writeln("=== RESUMO ==="),
    format("Total de propostas: ~d~n", [Total]),
    format("Aprovadas: ~d~n", [NA]),
    format("Revisadas: ~d~n", [NR]),
    format("Recusadas: ~d~n", [NC]),
    format("Hard stops: ~d~n~n", [NHS]),
    writeln("=== FIM ===").

% Ponto de entrada principal
main :-
    open('saida.txt', write, Out),
    set_output(Out),
    imprime_cabecalho,
    forall(proposta(ID, _Sol, _Prod, _Val, _Pz, _Tx, _Gar),
           imprimir_analise_proposta(ID)),
    imprime_resumo,
    close(Out),
    set_output(user_output),
    writeln("Arquivo saida.txt gerado com sucesso!").

% Executa automaticamente ao carregar
:- initialization(main).