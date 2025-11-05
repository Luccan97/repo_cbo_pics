# 🌀 Análise da Oferta de Práticas Integrativas no SUS

Este repositório contém um pipeline de análise e visualização de dados sobre as **Práticas Integrativas e Complementares em Saúde (PICS)** no Sistema Único de Saúde (SUS). A proposta é consolidar informações de diversas planilhas, processar e transformar os dados, gerar tabelas analíticas e gráficos informativos que auxiliem na compreensão da distribuição dessas práticas ao longo do tempo e entre diferentes ocupações profissionais (CBOs).

O objetivo é oferecer uma base sólida e reutilizável para análises descritivas, visuais e comparativas, com potencial de apoio à gestão pública, estudos acadêmicos e projetos de avaliação em saúde.

---

## 📁 Estrutura do Projeto

### `dados_brutos/`
Contém os arquivos originais em formato `.xlsx`, organizados por região e ano. Cada planilha apresenta a quantidade de procedimentos realizados por tipo de PICS e por CBO.

---

### `dados_clean/`
Armazena objetos processados (ex: `df.RDS`) com os dados prontos para análise. Essa pasta é alimentada pelo script `00_import.R`.

---

### `script/`

#### `00_import/`
Importa, trata e transforma os dados brutos:
- Valida os registros com base em uma lista de PICS permitidas.
- Faz limpeza e padronização de variáveis.
- Converte dados para formato longo (`long`).
- Salva o `data frame` consolidado como `df.RDS`.

#### `01_criaTabela1/`
Gera uma tabela com:
- Número de procedimentos por CBO (de 2015 a 2024).
- Variação percentual ano a ano.
- Estilização com `gt` e exportação em HTML (`t2.html`).

#### `02_criaTabela2/`
Gera uma tabela  com:
- Número de procedimentos por tipo de PICS (de 2015 a 2024).
- Variação percentual ano a ano.
- Estilização com `gt` e exportação em HTML (`t2.html`).

#### `03_criaGrafico1/`
Cria um **gráfico de linhas** com o número total de procedimentos por ano

#### `04_criaGrafico2/`
Cria um **heatmap categorizado** com os principais CBOs e as 10 práticas mais realizadas:
- Células coloridas por faixas de volume de procedimentos.
- Etiquetas com valores absolutos formatados.
- Exporta o gráfico em `.png` para uso em relatórios e apresentações.

---

## 🛠️ Requisitos

Este projeto requer R (>= 4.0) e os seguintes pacotes:

- `tidyverse`
- `janitor`
- `scales`
- `gt`
- `gtExtras`
- `readxl`
- `forcats`
- `purrr`

Instalação rápida via `pacman`:

```r
pacman::p_load(tidyverse, janitor, scales, gt, gtExtras, readxl, forcats, purrr)
