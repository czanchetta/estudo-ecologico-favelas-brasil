library(tidyverse)

# Load cleaned data
data     <- readRDS("data/processed/census_clean.rds")
uf_data  <- data$uf       # one row per UF: state_code, state_name, pop_total, pop_subnormal, income_pc
detail   <- data$detail   # one row per UF × sex × age_group: pop_subnormal, pop_total

cat("UF table dim:", dim(uf_data), "\n")
cat("Detail table dim:", dim(detail), "\n")

# ---------------------------------------------------------------------------
# Crude rate per UF
# Formula: crude_rate(UF) = (pop_subnormal_UF / pop_total_UF) * 100
# ---------------------------------------------------------------------------
rates <- uf_data |>
  mutate(crude_rate = (pop_subnormal / pop_total) * 100) |>
  select(state_code, state_name, pop_total, pop_subnormal, income_pc, crude_rate)

# ---------------------------------------------------------------------------
# Direct standardization by age and sex
#
# Standard population: total Brazilian population (sum of all UFs, Censo 2022)
#
# For each UF:
#   1. Cell-specific rate (age group × sex):
#      rate_ij = pop_subnormal_ij / pop_total_ij
#
#   2. Weight of each cell in the standard population:
#      weight_ij = std_pop_ij / std_pop_total
#
#   3. Standardized rate:
#      standardized_rate = sum(rate_ij * weight_ij) * 100
# ---------------------------------------------------------------------------

# Build standard population: national totals by sex × age_group
std_pop <- detail |>
  group_by(sex, age_group) |>
  summarise(std_pop = sum(pop_total, na.rm = TRUE), .groups = "drop")

std_pop_total <- sum(std_pop$std_pop)
cat("\nStandard population total:", format(std_pop_total, big.mark=","), "\n")
cat("Standard pop cells:", nrow(std_pop), "(should be 2 sex × 21 age groups =", 2*21, ")\n")

# Add weights
std_pop <- std_pop |>
  mutate(weight = std_pop / std_pop_total)

cat("Sum of weights:", sum(std_pop$weight), "(should be 1.0)\n")

# Merge detail with standard population weights
detail_with_std <- detail |>
  left_join(std_pop |> select(sex, age_group, weight), by = c("sex", "age_group")) |>
  mutate(
    rate_ij         = pop_subnormal / pop_total,          # cell-specific rate
    weighted_rate   = rate_ij * weight                    # contribution to standardized rate
  )

# Calculate standardized rate per UF (all sexes combined)
std_rates <- detail_with_std |>
  group_by(state_code) |>
  summarise(
    standardized_rate = sum(weighted_rate, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Calculate standardized rate by sex
std_rates_sex <- detail_with_std |>
  group_by(state_code, sex) |>
  summarise(
    std_rate_sex = sum(weighted_rate, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  pivot_wider(names_from = sex, values_from = std_rate_sex,
              names_prefix = "standardized_rate_") |>
  rename_with(~str_replace_all(tolower(.), " ", "_"), starts_with("standardized_rate_"))

# Rename sex columns to English
# The sex categories from the API are in Portuguese: Homens, Mulheres
names(std_rates_sex) <- names(std_rates_sex) |>
  str_replace("standardized_rate_homens",   "standardized_rate_male") |>
  str_replace("standardized_rate_mulheres", "standardized_rate_female")

cat("\nStd rates sex columns:", names(std_rates_sex), "\n")

# Merge all rates into final table
rates_final <- rates |>
  left_join(std_rates,     by = "state_code") |>
  left_join(std_rates_sex, by = "state_code") |>
  select(state_code, state_name, pop_total, pop_subnormal, income_pc,
         crude_rate, standardized_rate, standardized_rate_male, standardized_rate_female)

# ---------------------------------------------------------------------------
# Internal validation
# The national standardized rate (all UFs aggregated using their own population
# as the standard) must equal the national crude rate.
# Tolerance: 0.1 percentage point
# ---------------------------------------------------------------------------
national_crude <- (sum(uf_data$pop_subnormal) / sum(uf_data$pop_total)) * 100

# National standardized rate: aggregate each cell across all UFs, then apply weights.
# Because the standard pop IS the national pop, this collapses to the crude rate.
national_std_check <- detail_with_std |>
  group_by(sex, age_group) |>
  summarise(
    cell_sub  = sum(pop_subnormal, na.rm = TRUE),
    cell_pop  = sum(pop_total,     na.rm = TRUE),
    w         = first(weight),
    .groups   = "drop"
  ) |>
  mutate(contrib = (cell_sub / cell_pop) * w) |>
  summarise(std_rate = sum(contrib) * 100) |>
  pull(std_rate)

cat(sprintf("\nValidation:\n  National crude rate:        %.4f%%\n", national_crude))
cat(sprintf("  National standardized rate: %.4f%%\n", national_std_check))
cat(sprintf("  Difference:                 %.6f pp\n", abs(national_crude - national_std_check)))

stopifnot(
  "National standardized rate deviates more than 0.1 pp from crude rate" =
    abs(national_crude - national_std_check) < 0.1
)
cat("OK: validation passed\n")

# ---------------------------------------------------------------------------
# Print comparative table ordered by crude_rate descending
# ---------------------------------------------------------------------------
cat("\n=== Crude rate vs Standardized rate — all 27 UFs ===\n")
rates_final |>
  arrange(desc(crude_rate)) |>
  select(state_name, crude_rate, standardized_rate, standardized_rate_male, standardized_rate_female) |>
  mutate(across(where(is.numeric), ~round(.x, 2))) |>
  print(n = 27)

# Save
saveRDS(rates_final, "data/processed/rates.rds")
message("Saved: data/processed/rates.rds")
