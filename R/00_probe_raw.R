library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

# 1. Table 9884: favelas — inspect raw structure with classif 58 (age groups)
url1 <- paste0(BASE, "/agregados/9884/periodos/2022/variaveis/9612",
               "?localidades=N3[11]&classificacao=58[all]")
cat("URL:", url1, "\n")
r1 <- GET(url1, timeout(30))
cat("Status:", status_code(r1), "\n")
raw1 <- content(r1, as = "text", encoding = "UTF-8")
parsed1 <- fromJSON(raw1, simplifyVector = FALSE)
cat("Length:", length(parsed1), "\n")
# parsed1[[1]] should be the variable object
# parsed1[[1]]$resultados is a list of result objects
res1 <- parsed1[[1]]$resultados
cat("Num resultados:", length(res1), "\n")
cat("Keys in resultados[[1]]:", paste(names(res1[[1]]), collapse=", "), "\n")
# classificacoes in each resultado
cl1 <- res1[[1]]$classificacoes
cat("Num classificacoes:", length(cl1), "\n")
cat("classif names:", paste(sapply(cl1, function(x) x$id), collapse=", "), "\n")
# show all categories from classif 58
classif58_cats <- unique(sapply(res1, function(r) {
  cl <- r$classificacoes
  c58 <- keep(cl, ~.x$id == "58")
  if (length(c58) == 0) return(NA_character_)
  paste0(c58[[1]]$categoria$id, ":", c58[[1]]$categoria$nome)
}))
cat("Age group categories (classif 58):\n")
cat(paste(sort(na.omit(classif58_cats)), collapse="\n"), "\n")

# 2. Table 9514: population — inspect classif 287
url2 <- paste0(BASE, "/agregados/9514/periodos/2022/variaveis/93",
               "?localidades=N3[11]&classificacao=287[all]")
cat("\n\nURL:", url2, "\n")
r2 <- GET(url2, timeout(30))
cat("Status:", status_code(r2), "\n")
parsed2 <- content(r2, as = "text", encoding = "UTF-8") |> fromJSON(simplifyVector = FALSE)
res2 <- parsed2[[1]]$resultados
classif287_cats <- unique(sapply(res2, function(r) {
  cl <- r$classificacoes
  c287 <- keep(cl, ~.x$id == "287")
  if (length(c287) == 0) return(NA_character_)
  paste0(c287[[1]]$categoria$id, ":", c287[[1]]$categoria$nome)
}))
cat("Age categories (classif 287) — first 30:\n")
cat(paste(head(sort(na.omit(classif287_cats)), 60), collapse="\n"), "\n")

# 3. Test table 10293 for income (distribuicao massa rendimento per capita)
url3 <- paste0(BASE, "/agregados/10293/periodos/2022/variaveis/allxp",
               "?localidades=N3[11]")
cat("\n\nURL:", url3, "\n")
r3 <- GET(url3, timeout(30))
cat("Status:", status_code(r3), "\n")
if (status_code(r3) == 200) {
  d3 <- content(r3, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
  cat("Dim:", dim(d3), "\n")
  print(d3[, c("id", "variavel", "unidade")])
}

# 4. Test table 10295 for income
url4 <- paste0(BASE, "/agregados/10295/periodos/2022/variaveis/allxp",
               "?localidades=N3[11]")
cat("\n\nURL:", url4, "\n")
r4 <- GET(url4, timeout(30))
cat("Status:", status_code(r4), "\n")
if (status_code(r4) == 200) {
  d4 <- content(r4, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
  cat("Dim:", dim(d4), "\n")
  print(d4[, c("id", "variavel", "unidade")])
}
