library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

# Get table 9884 with classif 2[all]|58[all] — inspect first resultado
url1 <- paste0(BASE, "/agregados/9884/periodos/2022/variaveis/9612",
               "?localidades=N3[11]&classificacao=2[all]|58[all]")
r1 <- GET(url1, timeout(30))
cat("Status:", status_code(r1), "\n")
parsed1 <- content(r1, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
res1 <- parsed1[[1]]$resultados

cat("Number of resultado items:", length(res1), "\n")
cat("\nFirst resultado - raw str:\n")
str(res1[[1]], max.level = 4)
cat("\nSecond resultado:\n")
str(res1[[2]], max.level = 4)

# Now extract all sex × age combos with their series values
cat("\n\n=== All sex x age combos (table 9884, UF 11) ===\n")
map_dfr(res1, function(res) {
  classifs <- res$classificacoes
  sex_cat  <- Filter(function(x) x$id == "2", classifs)[[1]]$categoria
  age_cat  <- Filter(function(x) x$id == "58", classifs)[[1]]$categoria
  val <- res$series[[1]]$serie[["2022"]]
  tibble(
    sex_id   = sex_cat$id   %||% NA,
    sex_nome = sex_cat$nome %||% NA,
    age_id   = age_cat$id   %||% NA,
    age_nome = age_cat$nome %||% NA,
    value    = val
  )
}) |> print(n = 100)

# Table 9514: inspect structure — classif 2[all]|287[all]
url2 <- paste0(BASE, "/agregados/9514/periodos/2022/variaveis/93",
               "?localidades=N3[11]&classificacao=2[all]|287[all]")
r2 <- GET(url2, timeout(30))
parsed2 <- content(r2, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
res2 <- parsed2[[1]]$resultados
cat("\n\nTable 9514 — number of resultados:", length(res2), "\n")
cat("First resultado raw:\n")
str(res2[[1]], max.level = 4)

cat("\n=== All sex x age combos (table 9514, UF 11) ===\n")
map_dfr(res2, function(res) {
  classifs <- res$classificacoes
  sex_cat <- Filter(function(x) x$id == "2", classifs)[[1]]$categoria
  age_cat <- Filter(function(x) x$id == "287", classifs)[[1]]$categoria
  val <- res$series[[1]]$serie[["2022"]]
  tibble(sex_id=sex_cat$id%||%NA, sex_nome=sex_cat$nome%||%NA,
         age_id=age_cat$id%||%NA, age_nome=age_cat$nome%||%NA, value=val)
}) |> print(n = 200)

# Table 10295: income — query mean (var 13431) for all UFs
url3 <- paste0(BASE, "/agregados/10295/periodos/2022/variaveis/13431",
               "?localidades=N3[all]")
cat("\n\nIncome mean (table 10295 var 13431) all UFs:\n", url3, "\n")
r3 <- GET(url3, timeout(30))
cat("Status:", status_code(r3), "\n")
parsed3 <- content(r3, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
cat("Number of resultados:", length(parsed3[[1]]$resultados), "\n")
str(parsed3[[1]]$resultados[[1]], max.level = 4)
