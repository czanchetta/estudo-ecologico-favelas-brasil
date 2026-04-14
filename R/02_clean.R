library(tidyverse)

# ---------------------------------------------------------------------------
# Load raw data
# ---------------------------------------------------------------------------
raw_sub    <- readRDS("data/raw/raw_subnormal.rds")
raw_pop    <- readRDS("data/raw/raw_pop_age_sex.rds")
raw_income <- readRDS("data/raw/raw_income.rds")

# ---------------------------------------------------------------------------
# Inspect category IDs (printed for transparency)
# ---------------------------------------------------------------------------
cat("=== Sex categories (classif 2) ===\n")
raw_sub |> distinct(cid_2, cname_2) |> print()

cat("\n=== Age group categories in favelas table (classif 58) ===\n")
raw_sub |> distinct(cid_58, cname_58) |> arrange(as.integer(cid_58)) |> print(n = 30)

cat("\n=== Age categories in population table (classif 287) ===\n")
raw_pop |> distinct(cid_287, cname_287) |> print(n = 200)

# ---------------------------------------------------------------------------
# 1. Subnormal population table
#    - Remove Total rows (keep only sex x age_group combinations)
#    - Identify male/female IDs from non-Total sex categories
# ---------------------------------------------------------------------------
sex_ids      <- raw_sub |> distinct(cid_2, cname_2)
sex_total_id <- sex_ids |> filter(cname_2 == "Total") |> pull(cid_2)
age_total_id <- raw_sub |> filter(cname_58 == "Total") |> distinct(cid_58) |> pull(cid_58)

cat("\n\nSex total ID:", sex_total_id, "\n")
cat("Age total ID:", age_total_id, "\n")

# Subnormal: all sex × age combinations (including Totals for crude rate)
sub_clean <- raw_sub |>
  rename(
    state_code   = uf_id,
    state_name   = uf_nome,
    sex_id       = cid_2,
    sex          = cname_2,
    age_group_id = cid_58,
    age_group    = cname_58,
    pop_subnormal = value
  ) |>
  mutate(
    state_code    = as.integer(state_code),
    pop_subnormal = as.numeric(pop_subnormal)
  ) |>
  select(state_code, state_name, sex_id, sex, age_group_id, age_group, pop_subnormal)

cat("\nSubnormal clean dim:", dim(sub_clean), "\n")
print(head(sub_clean, 5))

# ---------------------------------------------------------------------------
# 2. Total population table (table 9514 — individual ages + grouped ages)
#    Goal: match the age groups used in the subnormal table (classif 58)
#
#    Strategy: check if classif 287 includes 5-year groups that match classif 58
#    If so, use them directly. Otherwise, aggregate individual year ages.
# ---------------------------------------------------------------------------

# Get the distinct age names from pop table and from subnormal table
age_groups_sub <- raw_sub |> distinct(cname_58) |> filter(cname_58 != "Total") |> pull()
age_cats_pop   <- raw_pop |> distinct(cid_287, cname_287)

cat("\n\nAge groups from favelas table (classif 58):\n")
print(sort(age_groups_sub))

cat("\nAge categories from population table — matching subnormal groups:\n")
matching <- age_cats_pop |> filter(cname_287 %in% age_groups_sub)
print(matching)

if (nrow(matching) >= 10) {
  cat("\nFound", nrow(matching), "matching age groups in population table — using directly.\n")
  pop_clean <- raw_pop |>
    filter(cname_287 %in% age_groups_sub) |>
    rename(
      state_code   = uf_id,
      state_name   = uf_nome,
      sex_id       = cid_2,
      sex          = cname_2,
      age_group_id = cid_287,
      age_group    = cname_287,
      pop_total    = value
    ) |>
    mutate(
      state_code = as.integer(state_code),
      pop_total  = as.numeric(pop_total)
    ) |>
    select(state_code, state_name, sex_id, sex, age_group, pop_total)
} else {
  cat("\nFewer than 10 matching groups — aggregating individual ages.\n")
  # Build a mapping: individual age -> 5-year group label
  # Age group labels use pattern "X a Y anos" or "100 anos ou mais"
  assign_group <- function(age_nome) {
    # Try to parse "X anos" or "X a Y anos" or "100 anos ou mais"
    if (grepl("^\\d+ a \\d+", age_nome)) return(age_nome)  # already a group
    if (grepl("100 anos ou mais", age_nome)) return("100 anos ou mais")
    age_num <- as.integer(str_extract(age_nome, "^\\d+"))
    if (is.na(age_num)) return(NA_character_)
    if (age_num >= 100) return("100 anos ou mais")
    lower <- (age_num %/% 5) * 5
    upper <- lower + 4
    paste0(lower, " a ", upper, " anos")
  }

  pop_clean <- raw_pop |>
    mutate(
      age_single_num = as.integer(str_extract(cname_287, "^\\d+")),
      age_group      = map_chr(cname_287, assign_group)
    ) |>
    filter(!is.na(age_group), age_group != "Total") |>
    group_by(uf_id, uf_nome, cid_2, cname_2, age_group) |>
    summarise(pop_total = sum(as.numeric(value), na.rm = TRUE), .groups = "drop") |>
    rename(
      state_code = uf_id,
      state_name = uf_nome,
      sex_id     = cid_2,
      sex        = cname_2
    ) |>
    mutate(state_code = as.integer(state_code)) |>
    select(state_code, state_name, sex_id, sex, age_group, pop_total)
}

cat("\nPopulation clean dim:", dim(pop_clean), "\n")
print(head(pop_clean, 5))

# ---------------------------------------------------------------------------
# 3. Income table — simple: one row per UF
# ---------------------------------------------------------------------------
income_clean <- raw_income |>
  rename(
    state_code = uf_id,
    state_name = uf_nome,
    income_pc  = value
  ) |>
  mutate(
    state_code = as.integer(state_code),
    income_pc  = as.numeric(income_pc)
  ) |>
  select(state_code, state_name, income_pc)

cat("\nIncome clean dim:", dim(income_clean), "\n")
print(income_clean)

# ---------------------------------------------------------------------------
# 4. Merge into census_clean
#    Merge always by state_code (numeric IBGE code), never by name
# ---------------------------------------------------------------------------

# Get total pop_subnormal per UF (Total sex, Total age) for crude rate
sub_total <- sub_clean |>
  filter(sex == "Total", age_group == "Total") |>
  select(state_code, pop_subnormal)

# Get total pop per UF from population table (sex=Total, then sum age groups)
pop_total_uf <- raw_pop |>
  filter(cname_2 == "Total", cname_287 == "Total") |>
  rename(state_code = uf_id, state_name = uf_nome, pop_total = value) |>
  mutate(state_code = as.integer(state_code), pop_total = as.numeric(pop_total)) |>
  select(state_code, state_name, pop_total)

# Detailed table: sex × age_group (non-Total) for standardization
# Use the matched/aggregated pop_clean and filter sub_clean to matching groups
sex_nontotal_ids <- sex_ids |> filter(cname_2 != "Total") |> pull(cid_2)

sub_detail <- sub_clean |>
  filter(sex_id %in% sex_nontotal_ids, age_group != "Total") |>
  select(state_code, sex, age_group, pop_subnormal)

pop_detail <- pop_clean |>
  filter(sex != "Total") |>
  select(state_code, sex, age_group, pop_total)

cat("\n=== Merging detail tables ===\n")
cat("sub_detail unique age groups:", sort(unique(sub_detail$age_group)), "\n")
cat("pop_detail unique age groups:", sort(unique(pop_detail$age_group)), "\n")
cat("sub_detail unique sex:", unique(sub_detail$sex), "\n")
cat("pop_detail unique sex:", unique(pop_detail$sex), "\n")

# Check age group alignment
common_ages   <- intersect(unique(sub_detail$age_group), unique(pop_detail$age_group))
only_in_sub   <- setdiff(unique(sub_detail$age_group), unique(pop_detail$age_group))
only_in_pop   <- setdiff(unique(pop_detail$age_group), unique(sub_detail$age_group))
cat("Common age groups:", length(common_ages), "\n")
if (length(only_in_sub) > 0) cat("Only in sub:", only_in_sub, "\n")
if (length(only_in_pop) > 0) cat("Only in pop:", only_in_pop, "\n")

# Check sex label alignment
# Sub table from table 9884 uses Portuguese sex names; pop from 9514 may differ
# If they don't match, harmonize
sex_sub <- sort(unique(sub_detail$sex))
sex_pop <- sort(unique(pop_detail$sex))
cat("Sex labels in sub:", sex_sub, "\n")
cat("Sex labels in pop:", sex_pop, "\n")

if (!all(sex_sub %in% sex_pop)) {
  cat("WARNING: Sex labels don't match. Checking by sex category IDs...\n")
  # Align by matching sex_id from both tables
  sex_mapping_sub <- sub_clean |> distinct(sex_id, sex) |> filter(sex != "Total")
  sex_mapping_pop <- raw_pop |> distinct(cid_2, cname_2) |> filter(cname_2 != "Total") |>
    rename(sex_id = cid_2, sex_pop = cname_2)
  cat("Sub sex mapping:\n"); print(sex_mapping_sub)
  cat("Pop sex mapping:\n"); print(sex_mapping_pop)
}

# Merge detail (sex x age_group)
detail_merged <- inner_join(sub_detail, pop_detail, by = c("state_code", "sex", "age_group"))
cat("\nDetail merged dim:", dim(detail_merged), "\n")
cat("Expected (2 sexes × n_age_groups × 27 UFs):", 2 * length(common_ages) * 27, "\n")

# Build top-level UF table (state_code, state_name, pop_total, pop_subnormal, income_pc)
census_clean <- pop_total_uf |>
  inner_join(sub_total,    by = "state_code") |>
  inner_join(income_clean, by = "state_code") |>
  rename(state_name = state_name.x) |>
  select(-any_of("state_name.y"))

# Attach detail (sex × age) for standardization
census_detail <- detail_merged

# ---------------------------------------------------------------------------
# 5. Validations
# ---------------------------------------------------------------------------
cat("\n\n=== Validations ===\n")

# 5.1 Population total between 190M and 215M
total_pop <- sum(census_clean$pop_total)
cat(sprintf("Total population: %s\n", format(total_pop, big.mark=",")))
stopifnot("pop_total sum not in expected range [190M, 215M]" =
            total_pop >= 190e6 & total_pop <= 215e6)
cat("OK: population total in valid range\n")

# 5.2 No negative numeric values
for (col in c("pop_total", "pop_subnormal", "income_pc")) {
  stopifnot("Negative values found" = all(census_clean[[col]] >= 0, na.rm = TRUE))
}
cat("OK: no negative values\n")

# 5.3 pop_subnormal <= pop_total per UF
stopifnot("pop_subnormal > pop_total in some UF" =
            all(census_clean$pop_subnormal <= census_clean$pop_total))
cat("OK: pop_subnormal <= pop_total\n")

# 5.4 Exactly 27 UFs
stopifnot("Expected 27 UFs" = nrow(census_clean) == 27)
cat("OK: 27 UFs present\n")

# 5.5 No duplicate state_code
stopifnot("Duplicate state_code found" =
            !any(duplicated(census_clean$state_code)))
cat("OK: no duplicate state_code\n")

# 5.6 Missing values check
cat("\nMissing values per column (census_clean):\n")
miss <- map_int(census_clean, ~sum(is.na(.x)))
print(miss)
if (any(miss > 0)) {
  stop("Missing values found in census_clean. Columns: ",
       paste(names(miss[miss > 0]), collapse=", "),
       ". Stopping — check data before proceeding.")
}
cat("OK: no missing values\n")

cat("\nMissing values per column (census_detail):\n")
miss2 <- map_int(census_detail, ~sum(is.na(.x)))
print(miss2)
if (any(miss2 > 0)) {
  cat("\n=== Rows with NA pop_subnormal — replacing with 0 (authorized) ===\n")
  print(census_detail |> filter(is.na(pop_subnormal)), n = 20)
  # These cells are suppressed ("-") by IBGE for statistical confidentiality.
  # All 7 NAs are in very old age groups (95-99, 100+) in small states where
  # favela population is 0 or near-0. Authorized substitution with 0.
  census_detail <- census_detail |>
    mutate(pop_subnormal = replace_na(pop_subnormal, 0))
  cat("Replaced", sum(is.na(census_detail$pop_subnormal)), "remaining NAs after replacement.\n")
}

# ---------------------------------------------------------------------------
# 6. Summary output
# ---------------------------------------------------------------------------
cat("\n=== census_clean summary ===\n")
cat("dim:", dim(census_clean), "\n")
print(summary(census_clean))

cat("\n=== Top 5 UFs by % favelas ===\n")
census_clean |>
  mutate(pct_favela = (pop_subnormal / pop_total) * 100) |>
  arrange(desc(pct_favela)) |>
  select(state_name, pop_subnormal, pop_total, pct_favela) |>
  head(5) |>
  print()

cat("\n=== census_detail summary ===\n")
cat("dim:", dim(census_detail), "\n")
print(head(census_detail, 5))

# ---------------------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------------------
saveRDS(list(uf = census_clean, detail = census_detail),
        "data/processed/census_clean.rds")
message("Saved: data/processed/census_clean.rds")
