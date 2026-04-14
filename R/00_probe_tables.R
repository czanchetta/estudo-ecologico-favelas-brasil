library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

probe_table <- function(id) {
  url <- paste0(BASE, "/agregados/", id)
  resp <- GET(url, timeout(15))
  if (status_code(resp) != 200) {
    cat(sprintf("  Table %s: HTTP %d\n", id, status_code(resp)))
    return(NULL)
  }
  meta <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
  periodos <- map_chr(meta$periodos %||% list(), ~.x$id %||% NA_character_)
  variaveis <- map_chr(meta$variaveis %||% list(), ~paste0(.x$id, ":", .x$nome))
  cat(sprintf("\n=== Table %s: %s ===\n", id, meta$nome))
  cat(sprintf("  Periods: %s\n", paste(periodos, collapse = ", ")))
  cat(sprintf("  Variables:\n"))
  walk(variaveis, ~cat("    ", .x, "\n"))
}

# Subnormal/aglomerado tables — check if any have 2022
cat("### Subnormal candidate tables ###\n")
walk(c("3379", "3380", "3381", "1425"), probe_table)

# Also check full CD table list for 2022 subnormal tables
# Re-load all CD tables
all_resp  <- GET(paste0(BASE, "/agregados"), timeout(30))
all_list  <- content(all_resp, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
all_tables <- map_dfr(all_list, function(p) {
  map_dfr(p$agregados %||% list(), ~tibble(
    pesquisa_id = p$id, tabela_id = .x$id, tabela_nome = .x$nome
  ))
})

cd <- all_tables |> filter(pesquisa_id == "CD")

# All subnormal tables
subnormal_all <- cd |> filter(str_detect(tolower(tabela_nome), "subnormal|aglomerado"))
cat("\n### All subnormal tables (full names) ###\n")
print(subnormal_all, n = 50)

# Population by sex+age (look for tables 9000+ or 10000+)
pop_sexo_idade <- cd |>
  filter(str_detect(tolower(tabela_nome), "sexo") &
         str_detect(tolower(tabela_nome), "grupo|idade") &
         as.integer(tabela_id) >= 9000)
cat("\n### Population sex+age tables (ID >= 9000) ###\n")
print(pop_sexo_idade, n = 50)

# Per capita income
renda_pc <- cd |>
  filter(str_detect(tolower(tabela_nome), "per capita|rendimento médio|renda média") &
         as.integer(tabela_id) >= 9000)
cat("\n### Income per capita tables (ID >= 9000) ###\n")
print(renda_pc, n = 30)

# Also check for rendimento nominal
rend_nominal <- cd |>
  filter(str_detect(tolower(tabela_nome), "rendimento") &
         as.integer(tabela_id) >= 9000)
cat("\n### Rendimento tables (ID >= 9000) ###\n")
print(rend_nominal |> select(tabela_id, tabela_nome), n = 30)
