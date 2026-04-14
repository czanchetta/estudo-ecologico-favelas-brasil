library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

# All agregados — nested by pesquisa
resp <- GET(paste0(BASE, "/agregados"), timeout(30))
message("Status: ", status_code(resp))
raw_list <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)

# raw_list is a list of pesquisas, each with $id, $nome, $agregados
# Unnest to get individual tables
all_tables <- map_dfr(raw_list, function(pesquisa) {
  agr <- pesquisa$agregados
  if (is.null(agr) || length(agr) == 0) return(NULL)
  map_dfr(agr, function(a) {
    tibble(
      pesquisa_id   = pesquisa$id   %||% NA_character_,
      pesquisa_nome = pesquisa$nome %||% NA_character_,
      tabela_id     = a$id          %||% NA_character_,
      tabela_nome   = a$nome        %||% NA_character_
    )
  })
})

cat("Total tables: ", nrow(all_tables), "\n")

# Filter Censo Demografico
cd <- all_tables |> filter(pesquisa_id == "CD")
cat("Censo Demografico tables: ", nrow(cd), "\n\n")
print(cd |> select(tabela_id, tabela_nome), n = 200)

# Key topics
cat("\n--- subnormal/aglomerado ---\n")
print(cd |> filter(str_detect(tolower(tabela_nome), "subnormal|aglomerado")) |> select(tabela_id, tabela_nome))

cat("\n--- rendimento/renda ---\n")
print(cd |> filter(str_detect(tolower(tabela_nome), "rendimento|renda")) |> select(tabela_id, tabela_nome))

cat("\n--- sexo + (idade|grupo) ---\n")
print(cd |> filter(str_detect(tolower(tabela_nome), "sexo") & str_detect(tolower(tabela_nome), "idade|grupo")) |> select(tabela_id, tabela_nome))

cat("\n--- populac|pessoas ---\n")
print(cd |> filter(str_detect(tolower(tabela_nome), "populaç|popula|pessoas")) |> select(tabela_id, tabela_nome))
