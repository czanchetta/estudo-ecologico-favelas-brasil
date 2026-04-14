library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

probe <- function(id) {
  url <- paste0(BASE, "/agregados/", id)
  resp <- GET(url, timeout(15))
  cat(sprintf("\n========== Table %s (HTTP %d) ==========\n", id, status_code(resp)))
  if (status_code(resp) != 200) return(invisible(NULL))
  meta <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
  cat(sprintf("Nome: %s\n", meta$nome))
  periodos <- map_chr(meta$periodos %||% list(), ~.x$id)
  cat(sprintf("Periodos: %s\n", paste(periodos, collapse=", ")))
  cat("Variaveis:\n")
  walk(meta$variaveis %||% list(), ~cat(sprintf("  [%s] %s\n", .x$id, .x$nome)))
  cat("Classificacoes:\n")
  walk(meta$classificacoes %||% list(), function(cl) {
    cat(sprintf("  Classif [%s]: %s\n", cl$id, cl$nome))
    walk(cl$categorias %||% list(), ~cat(sprintf("    cat [%s]: %s\n", .x$id, .x$nome)))
  })
}

# Tables to probe
probe("9884")   # favelas/comunidades urbanas
probe("9514")   # populacao por sexo e idade
probe("10296")  # rendimento domiciliar per capita

# Quick data test for each
test_data <- function(id, vars, classif = "") {
  url <- paste0(BASE, "/agregados/", id, "/periodos/2022/variaveis/", vars,
                "?localidades=N3[all]", if (classif != "") paste0("&classificacao=", classif))
  cat(sprintf("\n--- Data test table %s ---\nURL: %s\n", id, url))
  resp <- GET(url, timeout(30))
  cat("Status:", status_code(resp), "\n")
  if (status_code(resp) == 200) {
    d <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
    cat("Class:", class(d), "\n")
    if (is.data.frame(d)) {
      cat("Dim:", dim(d), "\n")
      print(head(d, 2))
    } else {
      cat("Length:", length(d), "\n")
      str(d[[1]], max.level = 2)
    }
  }
}

test_data("9884", "allxp")
test_data("9514", "allxp")
test_data("10296", "allxp")
