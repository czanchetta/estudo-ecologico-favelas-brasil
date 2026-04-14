library(httr)
library(jsonlite)
library(tidyverse)

SIDRA_BASE <- "https://servicodados.ibge.gov.br/api/v3/agregados"

# List all tables for Censo 2022 (pesquisa code CD2022)
url_list <- paste0(SIDRA_BASE, "?pesquisas=CD2022")
message("Fetching table list: ", url_list)
resp <- GET(url_list, timeout(60))
message("Status: ", status_code(resp))

tables_cd2022 <- content(resp, as = "text", encoding = "UTF-8") |>
  fromJSON(flatten = TRUE)

message("Total tables for CD2022: ", nrow(tables_cd2022))
print(tables_cd2022 |> select(id, nome) |> as_tibble(), n = 100)

# Search for tables with "subnormal" or "aglomerado" in name
cat("\n--- Tables with 'subnormal' or 'aglomerado' ---\n")
tables_cd2022 |>
  filter(str_detect(tolower(nome), "subnormal|aglomerado")) |>
  select(id, nome) |>
  print()

# Search for tables with 'rendimento' or 'renda'
cat("\n--- Tables with 'rendimento' or 'renda' ---\n")
tables_cd2022 |>
  filter(str_detect(tolower(nome), "rendimento|renda")) |>
  select(id, nome) |>
  print()

# Search for population tables (populacao, pessoas)
cat("\n--- Tables with 'populacao' or 'pessoas' ---\n")
tables_cd2022 |>
  filter(str_detect(tolower(nome), "populaç|popula|pessoas")) |>
  select(id, nome) |>
  print()
