library(httr)
library(jsonlite)
library(tidyverse)

BASE <- "https://servicodados.ibge.gov.br/api/v3"

# Get all agregados and inspect structure
message("Fetching all agregados...")
resp <- GET(paste0(BASE, "/agregados"), timeout(30))
message("Status: ", status_code(resp))

raw <- content(resp, as = "text", encoding = "UTF-8")
parsed <- fromJSON(raw, flatten = TRUE)

# Show structure and first rows
cat("\nColumn names: ", paste(names(parsed), collapse = ", "), "\n")
print(head(parsed, 3))

# Check if there's a pesquisa column (may be nested)
cat("\nClass: ", class(parsed), "\n")
cat("Dim: ", dim(parsed), "\n")

# If it's a data frame with pesquisa.id column, filter for CD
if ("pesquisa.id" %in% names(parsed)) {
  cd_tables <- parsed |> filter(pesquisa.id == "CD")
  cat("\n=== Censo Demografico tables ===\n")
  print(cd_tables |> select(id, nome, periodos) |> as_tibble(), n = 200)
} else {
  cat("\nAll columns:\n")
  str(parsed, max.level = 2)
}
