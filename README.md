# Prova-2---Problemas-L-gicos
Desenvolvimento de um Sistema Lógico Especialista em Prolog capaz de raciocinar sobre um domínio definido

# Sobre o Projeto
Este sistema especialista implementa um motor de decisão para análise de propostas de crédito, utilizando:

- Ontologia hierárquica de produtos financeiros e entidades
- Sistema de pontuação baseado em sinais (positivos e negativos)
- Hard stops (regras eliminatórias automáticas)
- Métricas financeiras (DTI, LTV, capacidade de pagamento)
- Explicabilidade completa de todas as decisões
- Geração automática de contrapropostas para casos com DTI elevado


#  Instalação
- Pré-requisitos
  - SWI-Prolog 8.0 ou superior
- Linux (Ubuntu / Debian)
  - sudo apt-get install swi-prolog
- macOS
  - brew install swi-prolog
- Windows
  - Baixe o instalador em: [swi-prolog.org](https://www.swi-prolog.org)](https://www.swi-prolog.org)
      
-Clonar o Repositório
  - https://github.com/bialimma/Prova-2---Problemas-L-gicos.git

# Execução no prolog
- No Prolog
  - cd('C:/Users/seu_usuario/Documents/credito'). --> Alterar conforme o caminho de onde está o arquivo (observação, há ponto final no fim dos comandos)
  - Se estiver tudo ok, aparecerá "true"
  - Executar: [principal].
  - Isso vai:
    - Ler a base de fatos de entrada.txt --> analisar todas as propostas (loan1, loan2, …) --> gerar um relatório completo em saida.txt (decisões + métricas + motivos + resumo final).
    - Gerará alguns "Warning's", que não são erros, pode prosseguir.
  - Abrir o "saida.txt" que será gerado na mesma pasta do projeto.
    <img width="475" height="254" alt="image" src="https://github.com/user-attachments/assets/ba9f1f03-ce18-42df-a8ca-7499cdcff08d" />


 # Entrada e saída de dados 
- entrada.txt
  - Contém apenas fatos (ontologia, solicitantes, garantias, propostas).
  - É lido automaticamente por principal.pl via consult('entrada.txt').

- saida.txt
  - É gerado/sobrescrito toda vez que você chama main..
  - Contém: Análise de cada proposta, dados básicos (solicitante, produto, valor, prazo, taxa, garantia), métricas calculadas (parcela, DTI, LTV), sinais de risco/benefício, pontuação final, decisão (APROVAR / REVISAR / RECUSAR), motivos em texto legível.
  - Um resumo final com total de propostas, quantas aprovadas, revisadas, recusadas e com hard stop.
 
# Exemplos de Resultados Esperados
  Execução do comando & resultado a se esperar
- explicacao_solucao(loan6, Decisao, Motivos).
- Decisao = aprovar,
Motivos = ["DTI elevado", "score de crédito excelente", 
           "emprego estável (>=24m)", "parcela alta para crédito pessoal"].
  
  



