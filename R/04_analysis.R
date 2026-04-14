library(tidyverse)
library(ggplot2)
library(ggrepel)
library(geobr)
library(sf)
library(broom)
library(patchwork)
library(RColorBrewer)
library(scales)
library(writexl)

rates <- readRDS("data/processed/rates.rds")

# State abbreviations (ordered by IBGE code)
siglas <- tibble(
  state_code = c(11,12,13,14,15,16,17,
                 21,22,23,24,25,26,27,28,29,
                 31,32,33,35,
                 41,42,43,
                 50,51,52,53),
  sigla = c("RO","AC","AM","RR","PA","AP","TO",
            "MA","PI","CE","RN","PB","PE","AL","SE","BA",
            "MG","ES","RJ","SP",
            "PR","SC","RS",
            "MS","MT","GO","DF")
)
rates <- rates |> left_join(siglas, by = "state_code")

# Common theme
theme_estudo <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    plot.caption  = element_text(size = 8, color = "grey50"),
    legend.position = "bottom"
  )

# ---------------------------------------------------------------------------
# 4.1 Choropleth map — standardized rate by UF
# ---------------------------------------------------------------------------
message("Loading geobr geometries...")
estados <- read_state(year = 2020, showProgress = FALSE)

# Merge by IBGE code (code_state in geobr, state_code in our data)
map_data <- estados |>
  left_join(rates, by = c("code_state" = "state_code"))

p_mapa <- ggplot(map_data) +
  geom_sf(aes(fill = standardized_rate), color = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette  = "YlOrRd",
    direction = 1,
    name     = "Taxa padronizada (%)",
    labels   = label_number(suffix = "%", accuracy = 0.1)
  ) +
  labs(
    title   = "Taxa Padronizada de População em Aglomerados Subnormais por UF — Brasil, Censo 2022",
    caption = "Fonte: IBGE, Censo 2022 | Tabela 9884 (Favelas e Comunidades Urbanas)"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title   = element_text(face = "bold", size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 1),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm")
  )

ggsave("outputs/figures/mapa_taxa_padronizada.png", p_mapa,
       width = 10, height = 8, dpi = 300, bg = "white")
message("Saved: outputs/figures/mapa_taxa_padronizada.png")

# ---------------------------------------------------------------------------
# 4.2 Scatter: income per capita × standardized rate
# ---------------------------------------------------------------------------
model <- lm(standardized_rate ~ income_pc, data = rates)
tidy_model <- tidy(model)
r2     <- summary(model)$r.squared
pearson <- cor(rates$income_pc, rates$standardized_rate)
pval   <- tidy_model |> filter(term == "income_pc") |> pull(p.value)

annotation_text <- sprintf(
  "R² = %.3f\nr de Pearson = %.3f\np-valor = %s",
  r2, pearson,
  ifelse(pval < 0.001, "< 0,001", sprintf("%.3f", pval))
)

p_scatter <- ggplot(rates, aes(x = income_pc, y = standardized_rate)) +
  geom_smooth(method = "lm", se = TRUE, color = "#2166ac", fill = "#abd9e9",
              alpha = 0.3, linewidth = 0.8) +
  geom_point(color = "#d73027", size = 2.5, alpha = 0.8) +
  geom_label_repel(
    aes(label = sigla),
    size = 2.8, max.overlaps = 27,
    box.padding = 0.3, point.padding = 0.2,
    segment.color = "grey60", segment.size = 0.3
  ) +
  annotate("label", x = Inf, y = Inf, hjust = 1.05, vjust = 1.3,
           label = annotation_text, size = 3, color = "grey20",
           fill = "grey95", label.size = 0.3) +
  scale_x_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1)) +
  labs(
    title    = "Renda Per Capita vs Taxa de Aglomerados Subnormais por UF — Brasil, Censo 2022",
    x        = "Rendimento domiciliar per capita médio (R$)",
    y        = "Taxa padronizada (%)",
    caption  = "Fonte: IBGE, Censo 2022 | Regressão OLS com IC 95%"
  ) +
  theme_estudo

ggsave("outputs/figures/scatter_renda_favela.png", p_scatter,
       width = 10, height = 7, dpi = 300, bg = "white")
message("Saved: outputs/figures/scatter_renda_favela.png")

# ---------------------------------------------------------------------------
# 4.3 Grouped bar chart: crude_rate vs standardized_rate
#     Split into two panels (14 + 13 UFs) using patchwork
# ---------------------------------------------------------------------------
rates_ordered <- rates |>
  arrange(desc(crude_rate)) |>
  mutate(state_label = factor(sigla, levels = sigla))

rates_long <- rates_ordered |>
  select(state_label, crude_rate, standardized_rate) |>
  pivot_longer(cols = c(crude_rate, standardized_rate),
               names_to = "tipo", values_to = "taxa") |>
  mutate(tipo = recode(tipo,
                       crude_rate         = "Taxa Bruta",
                       standardized_rate  = "Taxa Padronizada"))

# Split into two groups for readability
group1 <- levels(rates_long$state_label)[1:14]
group2 <- levels(rates_long$state_label)[15:27]

make_bar <- function(states, show_legend = FALSE) {
  ggplot(rates_long |> filter(state_label %in% states),
         aes(x = state_label, y = taxa, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(
      values = c("Taxa Bruta" = "#4393c3", "Taxa Padronizada" = "#d6604d"),
      name   = ""
    ) +
    scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1),
                       limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Taxa (%)") +
    theme_estudo +
    theme(legend.position = if (show_legend) "bottom" else "none",
          axis.text.x = element_text(size = 9))
}

p_bar1 <- make_bar(group1, show_legend = FALSE)
p_bar2 <- make_bar(group2, show_legend = TRUE)

p_barras <- (p_bar1 / p_bar2) +
  plot_annotation(
    title   = "Taxa Bruta vs Taxa Padronizada de Aglomerados Subnormais por UF — Brasil, Censo 2022",
    caption = "Fonte: IBGE, Censo 2022 | Ordenado por taxa bruta decrescente",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 11),
      plot.caption = element_text(size = 8, color = "grey50")
    )
  )

ggsave("outputs/figures/barras_crude_vs_padronizada.png", p_barras,
       width = 12, height = 10, dpi = 300, bg = "white")
message("Saved: outputs/figures/barras_crude_vs_padronizada.png")

# ---------------------------------------------------------------------------
# 4.4 Excel output
# ---------------------------------------------------------------------------
resultados_df <- rates |>
  arrange(desc(crude_rate)) |>
  select(
    `Código UF`                  = state_code,
    `UF`                         = state_name,
    `Sigla`                      = sigla,
    `Pop. Total`                  = pop_total,
    `Pop. em Aglomerados Subnormais` = pop_subnormal,
    `Renda Per Capita (R$)`       = income_pc,
    `Taxa Bruta (%)`              = crude_rate,
    `Taxa Padronizada (%)`        = standardized_rate,
    `Taxa Padronizada — Homens (%)` = standardized_rate_male,
    `Taxa Padronizada — Mulheres (%)` = standardized_rate_female
  ) |>
  mutate(across(where(is.numeric) & !c(`Código UF`, `Pop. Total`, `Pop. em Aglomerados Subnormais`),
                ~round(.x, 4)))

metodologia_df <- tibble(
  Item = c(
    "Estudo",
    "Fonte de dados",
    "Variável de desfecho",
    "Nível de agregação",
    "Tabela SIDRA — Favelas",
    "Tabela SIDRA — População total",
    "Tabela SIDRA — Renda",
    "Taxa bruta",
    "Padronização direta",
    "Fórmula taxa bruta",
    "Fórmula taxa padronizada",
    "População padrão",
    "Grupos etários",
    "Estratificação por sexo",
    "Células suprimidas (NA → 0)"
  ),
  Descrição = c(
    "Estudo ecológico — percentual de população em aglomerados subnormais por UF",
    "IBGE, Censo Demográfico 2022, via API SIDRA v3",
    "Percentual de população residente em favelas e comunidades urbanas (aglomerados subnormais)",
    "Unidade da Federação (27 UFs: 26 estados + DF)",
    "Tabela 9884, Variável 9612 (Classificação 2: Sexo; 58: Grupo de idade)",
    "Tabela 9514, Variável 93 (Classificação 2: Sexo; 287: Idade)",
    "Tabela 10295, Variável 13431 (Valor do rendimento nominal médio mensal domiciliar per capita)",
    "taxa_bruta(UF) = (pop_subnormal_UF / pop_total_UF) × 100",
    "Método de padronização direta por idade e sexo",
    "taxa_bruta = (pop_subnormal / pop_total) × 100",
    "taxa_padronizada = SUM(taxa_ij × peso_ij) × 100, onde taxa_ij = pop_subnormal_ij / pop_total_ij e peso_ij = pop_padrao_ij / pop_padrao_total",
    "Soma de todos os 27 UFs do Censo 2022 (população brasileira total ≈ 203 milhões)",
    "21 grupos quinquenais: 0-4, 5-9, ..., 95-99, 100+ anos",
    "Homens e Mulheres separados (Classificação 2, categorias 4 e 5)",
    "7 células suprimidas pelo IBGE (grupos 95-99 e 100+ em RR, MS, GO) substituídas por 0 — impacto negligível na taxa padronizada"
  )
)

write_xlsx(
  list(Resultados = resultados_df, Metodologia = metodologia_df),
  "outputs/tables/resultados_completos.xlsx"
)
message("Saved: outputs/tables/resultados_completos.xlsx")

# Print model summary to console
cat("\n=== Regressão OLS: renda per capita ~ taxa padronizada ===\n")
cat(sprintf("R²: %.4f\nPearson r: %.4f\np-valor (income_pc): %s\n",
            r2, pearson,
            ifelse(pval < 0.001, "< 0.001", sprintf("%.4f", pval))))
print(tidy_model)

# ===========================================================================
# 4.5  Pirâmides etárias — Brasil e regiões
# ===========================================================================
library(openxlsx)

data_clean <- readRDS("data/processed/census_clean.rds")
detail     <- data_clean$detail   # state_code, sex, age_group, pop_subnormal, pop_total

# Ordem numérica dos grupos etários
age_order <- detail |>
  distinct(age_group) |>
  mutate(n = as.integer(str_extract(age_group, "^\\d+"))) |>
  arrange(n) |>
  pull(age_group)

# Agregado nacional por sexo × grupo etário
pir <- detail |>
  group_by(sex, age_group) |>
  summarise(
    pop_total_ij = sum(pop_total,     na.rm = TRUE),
    pop_sub_ij   = sum(pop_subnormal, na.rm = TRUE),
    .groups = "drop"
  )

total_geral  <- sum(pir$pop_total_ij)
total_favela <- sum(pir$pop_sub_ij)

pir <- pir |>
  mutate(
    prop_geral  = (pop_total_ij / total_geral)  * 100,
    prop_favela = (pop_sub_ij   / total_favela) * 100,
    age_fct     = factor(age_group, levels = age_order),
    geral_sig   = if_else(sex == "Homens", -prop_geral,  prop_geral),
    favela_sig  = if_else(sex == "Homens", -prop_favela, prop_favela)
  )

y_lim     <- max(abs(c(pir$geral_sig, pir$favela_sig))) * 1.08
cores_sexo <- c("Homens" = "#3182bd", "Mulheres" = "#de2d26")

theme_pir <- theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 10),
    plot.caption       = element_text(size = 8, color = "grey50"),
    axis.text.y        = element_text(size = 7.5),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank()
  )

# ── 4.5.1  Pirâmide dupla (lado a lado) ──────────────────────────────────────
make_half <- function(data, ycol, title, show_y = TRUE) {
  ggplot(data, aes(x = age_fct, y = !!sym(ycol), fill = sex)) +
    geom_col(width = 0.85) +
    coord_flip() +
    scale_fill_manual(values = cores_sexo, name = NULL) +
    scale_y_continuous(
      labels  = function(x) paste0(abs(round(x, 1)), "%"),
      limits  = c(-y_lim, y_lim),
      expand  = expansion(0)
    ) +
    labs(title = title, x = NULL, y = "Proporção (%)") +
    theme_pir +
    theme(
      axis.text.y  = if (show_y) element_text(size = 7.5) else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank()
    )
}

p_geral  <- make_half(pir, "geral_sig",  "Distribuição — Pop. Geral",          show_y = TRUE)
p_favela <- make_half(pir, "favela_sig", "Distribuição — Pop. em Agl. Subn.",  show_y = FALSE)

p_dupla <- (p_geral | p_favela) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title   = "Estrutura Etária e por Sexo: Pop. Geral vs. Pop. em Aglomerados Subnormais — Brasil, Censo 2022",
    caption = "Fonte: IBGE, Censo 2022 | Proporção relativa (%) dentro de cada população",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.caption = element_text(size = 8, color = "grey50")
    )
  ) & theme(legend.position = "bottom")

ggsave("outputs/figures/piramide_brasil.png", p_dupla,
       width = 12, height = 8, dpi = 300, bg = "white")
message("Saved: outputs/figures/piramide_brasil.png")

# ── 4.5.2  Pirâmide sobreposta ────────────────────────────────────────────────
pir_long <- pir |>
  select(age_fct, sex, geral_sig, favela_sig) |>
  pivot_longer(
    cols      = c(geral_sig, favela_sig),
    names_to  = "pop_type",
    values_to = "prop"
  ) |>
  mutate(
    pop_type = recode(pop_type,
                      geral_sig  = "Pop. Geral",
                      favela_sig = "Pop. em Aglomerados Subnormais"),
    pop_type = factor(pop_type, levels = c("Pop. Geral", "Pop. em Aglomerados Subnormais"))
  ) |>
  arrange(pop_type)   # Pop. Geral desenhada primeiro (fundo), favelas por cima

p_sobreposta <- ggplot(pir_long,
                       aes(x = age_fct, y = prop, fill = pop_type, alpha = pop_type)) +
  geom_col(position = "identity", width = 0.85) +
  coord_flip() +
  scale_fill_manual(
    values = c("Pop. Geral" = "grey60",
               "Pop. em Aglomerados Subnormais" = "#e6550d"),
    name = NULL
  ) +
  scale_alpha_manual(
    values = c("Pop. Geral" = 0.60, "Pop. em Aglomerados Subnormais" = 0.75),
    guide  = "none"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(abs(round(x, 1)), "%"),
    limits = c(-y_lim, y_lim),
    expand = expansion(0)
  ) +
  labs(
    title    = "Pirâmide Etária Sobreposta: Pop. Geral vs. Aglomerados Subnormais — Brasil, Censo 2022",
    subtitle = "Homens à esquerda (valores negativos) | Mulheres à direita",
    x        = NULL,
    y        = "Proporção relativa (%)",
    caption  = "Fonte: IBGE, Censo 2022 | Cinza = Pop. Geral; Laranja = Pop. em Aglomerados Subnormais"
  ) +
  theme_pir

ggsave("outputs/figures/piramide_sobreposta.png", p_sobreposta,
       width = 10, height = 9, dpi = 300, bg = "white")
message("Saved: outputs/figures/piramide_sobreposta.png")

# ── 4.5.3  Pirâmides por região ───────────────────────────────────────────────
regioes_map <- tibble(
  state_code = c(11,12,13,14,15,16,17,
                 21,22,23,24,25,26,27,28,29,
                 31,32,33,35,
                 41,42,43,
                 50,51,52,53),
  regiao = c(rep("Norte",        7),
             rep("Nordeste",     9),
             rep("Sudeste",      4),
             rep("Sul",          3),
             rep("Centro-Oeste", 4))
)

pir_reg <- detail |>
  left_join(regioes_map, by = "state_code") |>
  group_by(regiao, sex, age_group) |>
  summarise(
    pop_total_ij = sum(pop_total,     na.rm = TRUE),
    pop_sub_ij   = sum(pop_subnormal, na.rm = TRUE),
    .groups      = "drop"
  ) |>
  group_by(regiao) |>
  mutate(
    prop_geral  = (pop_total_ij / sum(pop_total_ij))     * 100,
    prop_favela = (pop_sub_ij   / sum(pop_sub_ij, na.rm = TRUE)) * 100,
    age_fct     = factor(age_group, levels = age_order),
    geral_sig   = if_else(sex == "Homens", -prop_geral,  prop_geral),
    favela_sig  = if_else(sex == "Homens", -prop_favela, prop_favela)
  ) |>
  ungroup() |>
  mutate(across(c(geral_sig, favela_sig), ~if_else(is.nan(.x) | is.infinite(.x), 0, .x)))

pir_reg_long <- pir_reg |>
  select(regiao, age_fct, sex, geral_sig, favela_sig) |>
  pivot_longer(
    cols      = c(geral_sig, favela_sig),
    names_to  = "pop_type",
    values_to = "prop"
  ) |>
  mutate(
    pop_type = recode(pop_type,
                      geral_sig  = "Pop. Geral",
                      favela_sig = "Pop. em Agl. Subn."),
    pop_type = factor(pop_type, levels = c("Pop. Geral", "Pop. em Agl. Subn.")),
    regiao   = factor(regiao, levels = c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
  ) |>
  arrange(regiao, pop_type)

y_lim_reg <- max(abs(pir_reg_long$prop), na.rm = TRUE) * 1.08

p_regiao <- ggplot(pir_reg_long,
                   aes(x = age_fct, y = prop, fill = pop_type, alpha = pop_type)) +
  geom_col(position = "identity", width = 0.85) +
  coord_flip() +
  facet_wrap(~ regiao, ncol = 3) +
  scale_fill_manual(
    values = c("Pop. Geral" = "grey60", "Pop. em Agl. Subn." = "#e6550d"),
    name   = NULL
  ) +
  scale_alpha_manual(
    values = c("Pop. Geral" = 0.60, "Pop. em Agl. Subn." = 0.75),
    guide  = "none"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(abs(round(x, 1)), "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title   = "Estrutura Etária por Região: Pop. Geral vs. Aglomerados Subnormais — Brasil, Censo 2022",
    x = NULL, y = "Proporção relativa (%)",
    caption = "Fonte: IBGE, Censo 2022 | Cinza = Pop. Geral; Laranja = Pop. em Aglomerados Subnormais"
  ) +
  theme_pir +
  theme(
    axis.text.y     = element_text(size = 5.5),
    strip.text      = element_text(face = "bold", size = 9),
    legend.position = "bottom"
  )

ggsave("outputs/figures/piramide_por_regiao.png", p_regiao,
       width = 14, height = 10, dpi = 300, bg = "white")
message("Saved: outputs/figures/piramide_por_regiao.png")

# ── 4.5.4  Tabela Excel de apoio ──────────────────────────────────────────────
pir_excel <- pir |>
  mutate(
    razao_prop = prop_favela / prop_geral,
    age_fct    = factor(age_group, levels = age_order)
  ) |>
  arrange(age_fct, sex) |>
  select(
    grupo_etario            = age_group,
    sexo                    = sex,
    pop_total_brasil        = pop_total_ij,
    `prop_geral (%)`        = prop_geral,
    pop_subnormal_brasil    = pop_sub_ij,
    `prop_favela (%)`       = prop_favela,
    razao_prop
  )

wb_pir <- createWorkbook()
addWorksheet(wb_pir, "Piramide_dados")

s_hdr_pir <- createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#4472C4", halign = "CENTER", textDecoration = "Bold",
  border = "Bottom", borderColour = "#2F5496"
)
writeData(wb_pir, "Piramide_dados", pir_excel, headerStyle = s_hdr_pir)
setColWidths(wb_pir, "Piramide_dados",
             cols = 1:7, widths = c(22, 12, 18, 14, 20, 14, 14))
freezePane(wb_pir, "Piramide_dados", firstRow = TRUE)

addStyle(wb_pir, "Piramide_dados",
         createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(pir_excel)+1), cols = c(3, 5),
         gridExpand = TRUE, stack = TRUE)
addStyle(wb_pir, "Piramide_dados",
         createStyle(numFmt = "0.0000"),
         rows = 2:(nrow(pir_excel)+1), cols = c(4, 6, 7),
         gridExpand = TRUE, stack = TRUE)

over_rows  <- which(pir_excel$razao_prop > 1.10) + 1
under_rows <- which(pir_excel$razao_prop < 0.90) + 1

if (length(over_rows) > 0)
  addStyle(wb_pir, "Piramide_dados", createStyle(fgFill = "#FFFF99"),
           rows = over_rows, cols = 1:7, gridExpand = TRUE, stack = TRUE)
if (length(under_rows) > 0)
  addStyle(wb_pir, "Piramide_dados", createStyle(fgFill = "#CCE5FF"),
           rows = under_rows, cols = 1:7, gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb_pir, "outputs/tables/piramide_dados.xlsx", overwrite = TRUE)
message("Saved: outputs/tables/piramide_dados.xlsx")

cat(sprintf("\nrazao_prop > 1.10: %d celulas (sobre-representados em favelas)\n",
            length(over_rows)))
cat(sprintf("razao_prop < 0.90: %d celulas (sub-representados em favelas)\n",
            length(under_rows)))
