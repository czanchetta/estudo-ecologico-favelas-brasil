library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

# Reload CD tables
all_list <- GET(paste0(BASE, "/agregados"), timeout(30)) |>
  content(as = "text", encoding = "UTF-8") |>
  fromJSON(simplifyVector = FALSE)

cd_tables <- map_dfr(all_list, function(p) {
  map_dfr(p$agregados %||% list(), ~tibble(pid = p$id, tid = .x$id, nome = .x$nome))
}) |> filter(pid == "CD", as.integer(tid) >= 9000)

# Look for tables mentioning "situacao" or "localizacao" do domicilio
cat("=== situacao/localizacao domicilio (ID>=9000) ===\n")
cd_tables |>
  filter(str_detect(tolower(nome), "situa|localiza")) |>
  select(tid, nome) |>
  print(n = 50)

# Probe table 10211 metadata (populacao residente situacao domicilio)
probe <- function(id) {
  url <- paste0(BASE, "/agregados/", id)
  resp <- GET(url, timeout(15))
  cat(sprintf("\n=== Table %s (HTTP %d) ===\n", id, status_code(resp)))
  if (status_code(resp) != 200) return(invisible(NULL))
  meta <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
  cat(sprintf("Nome: %s\n", meta$nome))
  periodos <- map_chr(meta$periodos %||% list(), ~.x$id)
  cat(sprintf("Periodos: %s\n", paste(periodos, collapse=", ")))
  cat("Variaveis:\n")
  walk(meta$variaveis %||% list(), ~cat(sprintf("  %s: %s\n", .x$id, .x$nome)))
  cat("Classificacoes:\n")
  walk(meta$classificacoes %||% list(), function(cl) {
    cat(sprintf("  [%s] %s\n", cl$id, cl$nome))
    cats <- cl$categorias %||% list()
    walk(head(cats, 10), ~cat(sprintf("    %s: %s\n", .x$id, .x$nome)))
  })
}

probe("10211")  # populacao por situacao domicilio

# Try table 3379 with 2022 period directly
url_test <- paste0(BASE, "/agregados/3379/periodos/2022/variaveis/allxp?localidades=N3[all]")
cat("\n\nTest 3379 with 2022:\n", url_test, "\n")
resp_test <- GET(url_test, timeout(15))
cat("Status:", status_code(resp_test), "\n")
if (status_code(resp_test) == 200) {
  print(content(resp_test, as = "text", encoding = "UTF-8") |> fromJSON() |> head(3))
}

# Also probe table 9514 (common pop by sex/age census table)
probe("9514")
