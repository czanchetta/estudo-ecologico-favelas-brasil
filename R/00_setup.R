# Project environment setup
# Run once to initialize renv and install dependencies

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::init(bare = TRUE)

packages <- c(
  "tidyverse",
  "httr",
  "jsonlite",
  "sf",
  "geobr",
  "ggplot2",
  "ggrepel",
  "readxl",
  "writexl",
  "knitr",
  "rmarkdown",
  "kableExtra",
  "scales",
  "broom",
  "patchwork",
  "RColorBrewer",
  "renv"
)

# sidrar availability check
sidrar_available <- tryCatch({
  avail <- available.packages()
  "sidrar" %in% rownames(avail)
}, error = function(e) FALSE)

if (sidrar_available) {
  packages <- c(packages, "sidrar")
  message("sidrar found on CRAN — will be installed.")
} else {
  message("sidrar NOT found on CRAN — httr will be used directly for SIDRA API calls.")
}

install.packages(packages)
renv::snapshot()

message("Setup complete. Packages installed and renv.lock written.")
