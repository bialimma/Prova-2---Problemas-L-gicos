% explicacao.pl
% Rótulos legíveis, motivos/2 e predicados explicativos

:- [politicas].
:- [sinais].
:- [decisao].

% -------------------------
% ROTULO/2 - sinais
% -------------------------
rotulo(dti_bom, "DTI muito saudável").
rotulo(dti_ok, "DTI adequado").
rotulo(dti_alto, "DTI elevado").
rotulo(dti_ruim, "DTI muito elevado").

rotulo(ltv_saude, "LTV baixo (garantia forte)").
rotulo(ltv_medio, "LTV moderado").
rotulo(ltv_limite, "LTV muito alto (próximo do limite)").

rotulo(bureau_excelente, "score de crédito excelente").
rotulo(bureau_medio, "score de crédito mediano").
rotulo(bureau_baixo, "score de crédito baixo").

rotulo(atrasos_rec, "atrasos recentes em pagamentos").
rotulo(consultas_alta, "muitas consultas recentes").

rotulo(emprego_estavel, "emprego estável (>=24m)").
rotulo(emprego_recente, "emprego recente (<12m)").

rotulo(stress_parcela_pessoal, "parcela alta para crédito pessoal").
rotulo(perfil_premium, "perfil premium (baixo DTI e alto score)").

% -------------------------
% ROTULO_HARD/2
% -------------------------
rotulo_hard(idade_minima, "idade abaixo do mínimo legal").
rotulo_hard(sancao, "solicitante em lista de sanções restritivas").
rotulo_hard(ltv_excedido, "LTV acima do limite permitido").
rotulo_hard(renda_invalida, "renda não informada ou inválida").

% -------------------------
% MOTIVOS/2
% -------------------------
motivos(ID, Motivos) :-
    % Primeiro tenta hard stops
    findall(H, hardstop(ID, H), Hs),
    (   Hs \= []
    ->  findall(Msg,
                (member(H, Hs), rotulo_hard(H, Msg)),
                Motivos)
    ;   % Senão, usa sinais
        sinais(ID, Sinais),
        findall(Msg,
                ( member((Label,_Peso), Sinais),
                  rotulo(Label, Msg)
                ),
                Motivos)
    ).

% -------------------------
% EXPLICACAO_SOLUCAO/3
% explicacao_solucao(ID, Decisao, Motivos)
% -------------------------
explicacao_solucao(ID, Decisao, Motivos) :-
    decisao(ID, Decisao),
    motivos(ID, Motivos).

% -------------------------
% MOTIVO_FALHA/2
% motivo_falha(ID, Motivos) para recusas
% -------------------------
motivo_falha(ID, Motivos) :-
    decisao(ID, recusar),
    motivos(ID, Motivos).
