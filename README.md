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

# Execução (terminal ou prolog)
- No CMD
- cd C:\Users\seu_usuario\Documents\credito (alterar conforme o caminho de onde está o arquivo)
- Dentro da pasta do projeto, rodar:
    - swipl -s principal.pl 
- Dentro do prompt do Prolog que abrir, rode:
    - main.
- Isso vai:
  - Ler a base de fatos de entrada.txt --> analisar todas as propostas (loan1, loan2, …) --> gerar um relatório completo em saida.txt (decisões + métricas + motivos + resumo final).
  - Abrir o "saida.txt" que será gerado na mesma pasta do projeto.
**************************************************
- No Prolog
  - cd('C:/Users/seu_usuario/Documents/credito'). ( (alterar conforme o caminho de onde está o arquivo)
  - Se estiver tudo ok, aparecerá "true"
  - Executar: [principal].
  - Isso vai:
    - Ler a base de fatos de entrada.txt --> analisar todas as propostas (loan1, loan2, …) --> gerar um relatório completo em saida.txt (decisões + métricas + motivos + resumo final).
  - Abrir o "saida.txt" que será gerado na mesma pasta do projeto.






