library(httr)
library(jsonlite)
library(tidyverse)

SIDRA_BASE <- "https://servicodados.ibge.gov.br/api/v3/agregados"

# List tables within Censo Demografico (pesquisa CD)
url_cd <- paste0(SIDRA_BASE, "?pesquisas=CD")
message("Fetching CD tables: ", url_cd)
resp <- GET(url_cd, timeout(60))
message("Status: ", status_code(resp))
tables_cd <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
message("Total CD tables: ", nrow(tables_cd))

# Show all tables
print(tables_cd |> select(id, nome) |> as_tibble(), n = 200)

# Filter by keywords
cat("\n--- 'subnormal' or 'aglomerado' ---\n")
print(tables_cd |> filter(str_detect(tolower(nome), "subnormal|aglomerado")) |> select(id, nome))

cat("\n--- 'rendimento' or 'renda' ---\n")
print(tables_cd |> filter(str_detect(tolower(nome), "rendimento|renda")) |> select(id, nome))

cat("\n--- 'sexo' and ('idade' or 'grupo') ---\n")
print(tables_cd |> filter(str_detect(tolower(nome), "sexo") & str_detect(tolower(nome), "idade|grupo")) |> select(id, nome))
