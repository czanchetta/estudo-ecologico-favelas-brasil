# Estudo Ecológico — População em Aglomerados Subnormais no Brasil

Análise epidemiológica ecológica da proporção de população residente em favelas e comunidades urbanas (aglomerados subnormais) por Unidade da Federação, com base no **Censo Demográfico IBGE 2022**.

## Objetivos

- Calcular taxas brutas e padronizadas por idade e sexo de residência em aglomerados subnormais para as 27 UFs brasileiras
- Explorar a correlação entre renda per capita estadual e taxa padronizada
- Produzir mapa coroplético, gráficos comparativos e relatório reproduzível em RMarkdown

## Fonte dos dados

| Tabela SIDRA | Conteúdo |
|---|---|
| **9884** | Pop. em Favelas e Comunidades Urbanas por sexo e grupo de idade (Censo 2022) |
| **9514** | Pop. total por sexo e idade (Censo 2022) |
| **10295** | Rendimento nominal médio mensal domiciliar per capita (Censo 2022) |

Acesso via [API SIDRA v3](https://servicodados.ibge.gov.br/api/docs/agregados).

## Pré-requisitos

- **R >= 4.2**
- Pacotes gerenciados via `renv` — lista completa em `renv.lock`
- Conexão à internet para download dos dados na primeira execução

## Reprodução

```r
# 1. Clonar o repositório
# git clone <URL>
# cd ibge-favelas-estudo

# 2. Restaurar ambiente de pacotes
renv::restore()

# 3. Executar pipeline em sequência
source("R/01_download.R")   # download dos dados via API SIDRA
source("R/02_clean.R")      # limpeza e validação
source("R/03_rates.R")      # taxas brutas e padronizadas
source("R/04_analysis.R")   # visualizações e tabela Excel

# 4. Gerar relatório HTML
rmarkdown::render("relatorio/estudo_ecologico.Rmd", output_format = "html_document")
```

## Variáveis principais

| Variável | Descrição |
|---|---|
| `state_code` | Código numérico IBGE da UF |
| `state_name` | Nome da UF |
| `pop_total` | População total residente (Censo 2022) |
| `pop_subnormal` | Pop. em aglomerados subnormais/favelas |
| `income_pc` | Rendimento domiciliar per capita médio (R$) |
| `crude_rate` | Taxa bruta (%) |
| `standardized_rate` | Taxa padronizada por idade e sexo (%) |
| `standardized_rate_male` | Taxa padronizada — Homens (%) |
| `standardized_rate_female` | Taxa padronizada — Mulheres (%) |

## Estrutura do repositório

```
ibge-favelas-estudo/
├── data/
│   ├── raw/            # dados brutos da API (não versionados)
│   └── processed/      # dados limpos e taxas calculadas
├── R/
│   ├── 00_setup.R      # instalação de pacotes via renv
│   ├── 01_download.R   # download via API SIDRA
│   ├── 02_clean.R      # limpeza e validação dos dados
│   ├── 03_rates.R      # cálculo de taxas brutas e padronizadas
│   └── 04_analysis.R   # visualizações e exportação Excel
├── relatorio/
│   └── estudo_ecologico.Rmd
├── outputs/
│   ├── figures/        # mapas e gráficos (PNG, 300 dpi)
│   └── tables/         # resultados em Excel
├── .gitignore
├── README.md
└── renv.lock
```

## Limitações do estudo ecológico

- **Falácia ecológica:** associações no nível estadual não são extrapoláveis ao nível individual
- **n = 27 UFs:** poder estatístico limitado para análises de correlação
- **Proxy:** o conceito de aglomerado subnormal não captura a totalidade das situações de precariedade habitacional
- **Renda per capita estadual:** derivada do questionário amostral do Censo (~10% dos domicílios), com maior incerteza em estados pequenos

## Padronização direta — fórmula

$$\text{Taxa padronizada}_{UF} = \sum_{i,j} \left( \frac{\text{Pop. sub}_{ij,UF}}{\text{Pop. total}_{ij,UF}} \times \frac{\text{Pop. padrão}_{ij}}{\text{Pop. padrão total}} \right) \times 100$$

Onde $i$ = grupo etário quinquenal (21 grupos: 0–4 a 100+) e $j$ = sexo (Homens, Mulheres).
**População padrão:** população brasileira total do Censo 2022 (~203 milhões).
