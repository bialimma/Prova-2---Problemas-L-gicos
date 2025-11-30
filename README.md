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
- 1) explicacao_solucao/3
  <img width="643" height="105" alt="image" src="https://github.com/user-attachments/assets/5bce6a6f-6fe6-4dc7-b4e4-ebc6f7a20df9" />
- 2) motivo_falha/2
  <img width="408" height="61" alt="image" src="https://github.com/user-attachments/assets/c09c6d1b-985f-4f94-ae4c-544849045d86" />
- 3) motivos/2
  <img width="681" height="99" alt="image" src="https://github.com/user-attachments/assets/a1770419-fff0-4efe-9c15-aef3093f2ffc" />
- 4) Gerar Contraproposta
  <img width="393" height="126" alt="image" src="https://github.com/user-attachments/assets/9cc277bf-6456-4dfd-a5a4-51ddaebaeb7a" />
- 5) Listar todas as recusas
  <img width="443" height="50" alt="image" src="https://github.com/user-attachments/assets/66926971-f89c-410f-ac63-260271571ae5" />




