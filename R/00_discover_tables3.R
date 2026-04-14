library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

safe_get <- function(url) {
  message("GET ", url)
  resp <- GET(url, timeout(30))
  message("  -> ", status_code(resp))
  if (status_code(resp) != 200) return(NULL)
  content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
}

# Step 1: list all pesquisas and find Censo Demografico code
pesquisas <- safe_get(paste0(BASE, "/pesquisas"))
cat("\n=== Pesquisas with 'Censo' ===\n")
pesquisas |>
  filter(str_detect(tolower(nome), "censo")) |>
  select(id, nome) |>
  print()

# Step 2: list all agregados (tables) — no filter, get full list
all_tables <- safe_get(paste0(BASE, "/agregados"))
cat("\n=== Total tables in SIDRA: ", nrow(all_tables), "===\n")

# Step 3: find censo demografico tables (look for "Censo Demográfico" in nome
# or periodos containing 2022)
cd_tables <- all_tables |>
  filter(str_detect(tolower(nome), "aglomerado|subnormal|rendimento|renda|domicil|moradores"))
cat("\n=== Tables matching key topics ===\n")
print(cd_tables |> select(id, nome), n = 50)

# Step 4: Probe known/plausible Census 2022 table numbers
# Check if metadata endpoint returns period 2022
candidate_tables <- c("9935", "9900", "9901", "9898", "9899",
                       "9514", "9515", "9516", "9606", "9607",
                       "9608", "9609", "9610", "9611", "9612",
                       "9920", "9921", "9922", "9923", "9924",
                       "9930", "9931", "9932", "9933", "9934")

cat("\n=== Probing candidate table numbers ===\n")
results <- map_dfr(candidate_tables, function(tbl) {
  url <- paste0(BASE, "/agregados/", tbl)
  resp <- GET(url, timeout(15))
  if (status_code(resp) != 200) {
    return(tibble(id = tbl, status = status_code(resp), nome = NA_character_, periodos = NA_character_))
  }
  meta <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON()
  periodos_str <- if (!is.null(meta$periodos)) paste(unlist(meta$periodos), collapse = ",") else NA_character_
  tibble(id = tbl, status = status_code(resp), nome = meta$nome %||% NA_character_, periodos = periodos_str)
})
print(results |> filter(status == 200), n = 50)
