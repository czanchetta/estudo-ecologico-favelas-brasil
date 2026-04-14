library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

# Fetch data with all classifications and show unique category values
fetch_classif <- function(table_id, var_id, classif_str) {
  url <- paste0(BASE, "/agregados/", table_id, "/periodos/2022/variaveis/", var_id,
                "?localidades=N3[11]",          # just Rondonia for speed
                "&classificacao=", classif_str)
  cat(sprintf("\n=== Table %s var %s classif %s ===\nURL: %s\n", table_id, var_id, classif_str, url))
  resp <- GET(url, timeout(30))
  cat("Status:", status_code(resp), "\n")
  if (status_code(resp) != 200) return(invisible(NULL))

  raw  <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
  # raw is a list of variables; each has $resultados = list of {classificacoes, series}
  walk(raw, function(var) {
    cat(sprintf("Var: %s\n", var$variavel))
    walk(var$resultados, function(res) {
      # classificacoes: list of {id, nome, categoria: {id, nome}}
      walk(res$classificacoes, function(cl) {
        cat(sprintf("  Classif [%s] %s = cat [%s] %s\n",
                    cl$id, cl$nome, cl$categoria$id, cl$categoria$nome))
      })
    })
  })
}

# Table 9884: favelas — classif 2 (Sexo) x 58 (Grupo de idade)
# Get all categories of classif 58
url_58 <- paste0(BASE, "/agregados/9884/periodos/2022/variaveis/9612",
                 "?localidades=N3[11]&classificacao=2[0]|58[all]")
cat("Fetching age groups in table 9884:\n", url_58, "\n")
r <- GET(url_58, timeout(30))
cat("Status:", status_code(r), "\n")
if (status_code(r) == 200) {
  d <- content(r, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
  # Extract unique age group categories
  cats <- map(d[[1]]$resultados, ~.x$classificacoes) |>
    unlist(recursive = FALSE) |>
    keep(~.x$id == "58") |>
    map(~tibble(cat_id = .x$categoria$id, cat_nome = .x$categoria$nome)) |>
    bind_rows() |> distinct()
  cat("Age group categories (classif 58):\n")
  print(cats, n = 50)
}

# Table 9514: populacao — classif 287 (Idade/grupos)
url_287 <- paste0(BASE, "/agregados/9514/periodos/2022/variaveis/93",
                  "?localidades=N3[11]&classificacao=2[0]|287[all]")
cat("\n\nFetching age categories in table 9514 (classif 287):\n", url_287, "\n")
r2 <- GET(url_287, timeout(30))
cat("Status:", status_code(r2), "\n")
if (status_code(r2) == 200) {
  d2 <- content(r2, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
  cats2 <- map(d2[[1]]$resultados, ~.x$classificacoes) |>
    unlist(recursive = FALSE) |>
    keep(~.x$id == "287") |>
    map(~tibble(cat_id = .x$categoria$id, cat_nome = .x$categoria$nome)) |>
    bind_rows() |> distinct()
  cat("Age categories in table 9514 (classif 287):\n")
  print(cats2, n = 200)
}

# Table 10315: rendimento medio/mediano domiciliar per capita
url_10315 <- paste0(BASE, "/agregados/10315/periodos/2022/variaveis/allxp",
                    "?localidades=N3[11]")
cat("\n\nTesting table 10315 (rendimento medio):\n", url_10315, "\n")
r3 <- GET(url_10315, timeout(30))
cat("Status:", status_code(r3), "\n")
if (status_code(r3) == 200) {
  d3 <- content(r3, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
  cat("Dim:", dim(d3), "\n")
  print(d3[, c("id", "variavel", "unidade")])
}
