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

- Windows
    - Baixe o instalador em: [swi-prolog.org](https://www.swi-prolog.org)](https://www.swi-prolog.org)
      
-Clonar o Repositório
    - https://github.com/bialimma/Prova-2---Problemas-L-gicos.git

#Execução (cmd ou prolog)
- CMD
- Dentro da pasta do projeto
    - swipl -s principal.pl
- Dentro do prompt do Prolog que abrir, rode:
    - main.
- Isso vai:
  - Ler a base de fatos de entrada.txt --> analisar todas as propostas (loan1, loan2, …) --> gerar um relatório completo em saida.txt (decisões + métricas + motivos + resumo final).
  - Abrir o "saida.txt" que será gerado dentro da pasta clonada 







