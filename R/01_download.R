library(httr)
library(jsonlite)
library(tidyverse)

SIDRA_BASE <- "https://servicodados.ibge.gov.br/api/v3/agregados"

# ---------------------------------------------------------------------------
# Parser: converts nested SIDRA v3 JSON into a flat tibble
# Structure: list[variable] -> resultados[classif x UF combos] -> series[UF]
#
# categoria field in SIDRA v3 is a named list: list("id_key" = "name_value")
# We flatten each classification into cid_{N} / cname_{N} column pairs.
# ---------------------------------------------------------------------------
parse_sidra <- function(parsed_list) {
  map_dfr(parsed_list, function(var_obj) {
    var_id   <- var_obj$id
    var_nome <- var_obj$variavel

    map_dfr(var_obj$resultados, function(res) {
      # Build one row of classification columns per resultado
      classif_row <- list()
      for (cl in res$classificacoes) {
        cat <- cl$categoria
        cid <- paste0("cid_",   cl$id)
        cnm <- paste0("cname_", cl$id)
        classif_row[[cid]] <- names(cat)[1]
        classif_row[[cnm]] <- unlist(cat, use.names = FALSE)[1]
      }
      classif_df <- as_tibble(classif_row)

      # Extract series (one row per UF × year)
      map_dfr(res$series, function(s) {
        uf_id   <- s$localidade$id
        uf_nome <- s$localidade$nome
        map_dfr(names(s$serie), function(yr) {
          bind_cols(
            tibble(var_id, var_nome, uf_id, uf_nome, year = yr, value = s$serie[[yr]]),
            classif_df
          )
        })
      })
    })
  })
}

# Helper: fetch SIDRA URL, check status, return parsed tibble or NULL
fetch_sidra <- function(url) {
  message("Calling: ", url)
  resp <- tryCatch(
    GET(url, timeout(60)),
    error = function(e) stop("HTTP request failed: ", conditionMessage(e))
  )
  status <- status_code(resp)
  message("HTTP status: ", status)
  if (status != 200) stop("Non-200 response (", status, ") for: ", url)
  raw  <- content(resp, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(raw, simplifyVector = FALSE)
  parse_sidra(parsed)
}

# ---------------------------------------------------------------------------
# 1. Population in favelas/comunidades urbanas — Censo 2022
#    Table 9884 | Variable 9612
#    Classif 2[all]=Sexo, 58[all]=Grupo de idade, 86[95251]=Total cor/raca
# ---------------------------------------------------------------------------
url_subnormal <- paste0(
  SIDRA_BASE,
  "/9884/periodos/2022/variaveis/9612",
  "?localidades=N3[all]",
  "&classificacao=2[all]|58[all]|86[95251]"
)

raw_subnormal <- tryCatch(
  fetch_sidra(url_subnormal),
  error = function(e) { message("ERROR [favelas]: ", conditionMessage(e)); NULL }
)
if (!is.null(raw_subnormal)) {
  saveRDS(raw_subnormal, "data/raw/raw_subnormal.rds")
  message("Saved: data/raw/raw_subnormal.rds")
}

# ---------------------------------------------------------------------------
# 2. Total population by UF, sex and individual age — Censo 2022
#    Table 9514 | Variable 93
#    Classif 2[all]=Sexo, 287[all]=Idade, 286[113635]=Total forma declaracao
# ---------------------------------------------------------------------------
url_pop_age_sex <- paste0(
  SIDRA_BASE,
  "/9514/periodos/2022/variaveis/93",
  "?localidades=N3[all]",
  "&classificacao=2[all]|287[all]|286[113635]"
)

raw_pop_age_sex <- tryCatch(
  fetch_sidra(url_pop_age_sex),
  error = function(e) { message("ERROR [pop age/sex]: ", conditionMessage(e)); NULL }
)
if (!is.null(raw_pop_age_sex)) {
  saveRDS(raw_pop_age_sex, "data/raw/raw_pop_age_sex.rds")
  message("Saved: data/raw/raw_pop_age_sex.rds")
}

# ---------------------------------------------------------------------------
# 3. Mean household income per capita by UF — Censo 2022
#    Table 10295 | Variable 13431 (valor do rendimento nominal médio mensal)
# ---------------------------------------------------------------------------
url_income <- paste0(
  SIDRA_BASE,
  "/10295/periodos/2022/variaveis/13431",
  "?localidades=N3[all]"
)

raw_income <- tryCatch(
  fetch_sidra(url_income),
  error = function(e) { message("ERROR [income]: ", conditionMessage(e)); NULL }
)
if (!is.null(raw_income)) {
  saveRDS(raw_income, "data/raw/raw_income.rds")
  message("Saved: data/raw/raw_income.rds")
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
message("\n=== Download summary ===")
datasets <- list(subnormal = raw_subnormal, pop_age_sex = raw_pop_age_sex, income = raw_income)
for (nm in names(datasets)) {
  obj <- datasets[[nm]]
  if (is.null(obj)) {
    message(nm, ": FAILED")
  } else {
    message(nm, ": OK | dim = ", paste(dim(obj), collapse = " x "))
    print(head(obj, 3))
  }
}
