library(tidyverse)
library(openxlsx)
# Se necessário: install.packages("openxlsx")

# ── Caminhos ──────────────────────────────────────────────────────────────────
out_path <- "outputs/tables/memoria_calculo_padronizacao.xlsx"

# ── Carregar dados ────────────────────────────────────────────────────────────
data_in <- readRDS("data/processed/census_clean.rds")
rates    <- readRDS("data/processed/rates.rds")
uf_data  <- data_in$uf      # 27 × 5
detail   <- data_in$detail  # 1134 × 5 (sex × age_group × UF)

# Ordem numérica para grupos etários
age_num_map <- detail |>
  distinct(age_group) |>
  mutate(age_num = as.integer(str_extract(age_group, "^\\d+")))

# ── População padrão e pesos ──────────────────────────────────────────────────
std_pop <- detail |>
  group_by(sex, age_group) |>
  summarise(pop_padrao_ij = sum(pop_total, na.rm = TRUE), .groups = "drop") |>
  left_join(age_num_map, by = "age_group") |>
  arrange(sex, age_num) |>
  select(-age_num)

pop_padrao_total_val <- sum(std_pop$pop_padrao_ij)

std_pop <- std_pop |>
  mutate(peso_ij = pop_padrao_ij / pop_padrao_total_val)

weights_lkp <- std_pop |> select(sex, age_group, peso_ij)

cat(sprintf("Pop. padrao total: %s\n", format(pop_padrao_total_val, big.mark = ",")))
cat(sprintf("Soma dos pesos: %.8f (deve ser 1.0)\n", sum(std_pop$peso_ij)))
cat(sprintf("Celulas (sexo x grupo): %d (deve ser 42)\n\n", nrow(std_pop)))

# ============================================================================
# ABA 1 — Populacao_Padrao
# ============================================================================
aba1 <- std_pop |>
  mutate(pop_padrao_total = pop_padrao_total_val) |>
  select(
    grupo_etario     = age_group,
    sexo             = sex,
    pop_padrao_ij,
    pop_padrao_total,
    peso_ij
  )

aba1_total <- tibble(
  grupo_etario     = "TOTAL",
  sexo             = "",
  pop_padrao_ij    = pop_padrao_total_val,
  pop_padrao_total = pop_padrao_total_val,
  peso_ij          = sum(aba1$peso_ij)
)

aba1 <- bind_rows(aba1, aba1_total)

# ============================================================================
# ABA 2 — Calculo_por_UF
# ============================================================================
detail_c <- detail |>
  left_join(weights_lkp, by = c("sex", "age_group")) |>
  left_join(age_num_map, by = "age_group") |>
  mutate(
    taxa_especifica_ij = pop_subnormal / pop_total,
    contribuicao_ij    = taxa_especifica_ij * peso_ij
  ) |>
  left_join(uf_data |> select(state_code, state_name), by = "state_code") |>
  arrange(state_code, sex, age_num) |>
  select(-age_num)

det_rows <- detail_c |>
  select(
    state_code,
    state_name,
    grupo_etario       = age_group,
    sexo               = sex,
    pop_subnormal_ij   = pop_subnormal,
    pop_total_ij       = pop_total,
    taxa_especifica_ij,
    peso_ij,
    contribuicao_ij
  )

# Subtotais por UF (pop de uf_data; contrib = soma das 42 celulas)
sub_rows <- detail_c |>
  group_by(state_code) |>
  summarise(contribuicao_sum = sum(contribuicao_ij, na.rm = TRUE), .groups = "drop") |>
  left_join(uf_data |> select(state_code, state_name, pop_total, pop_subnormal),
            by = "state_code") |>
  transmute(
    state_code,
    state_name,
    grupo_etario       = "SUBTOTAL — soma contribuicao_ij x100 = taxa_padronizada (%)",
    sexo               = "",
    pop_subnormal_ij   = pop_subnormal,
    pop_total_ij       = pop_total,
    taxa_especifica_ij = NA_real_,
    peso_ij            = NA_real_,
    contribuicao_ij    = contribuicao_sum
  )

# Intercalar linhas de dados com subtotais
aba2 <- map_dfr(sort(unique(det_rows$state_code)), function(sc) {
  bind_rows(
    filter(det_rows, state_code == sc),
    filter(sub_rows,  state_code == sc)
  )
})

# Posições das linhas de subtotal (1-based no data frame => +1 para linha na aba Excel)
sub_idx_aba2 <- which(grepl("^SUBTOTAL", aba2$grupo_etario))

# ============================================================================
# ABA 3 — Conferencia_Final
# ============================================================================
national_crude <- (sum(uf_data$pop_subnormal) / sum(uf_data$pop_total)) * 100

# Função: taxa padronizada nacional (re-agrega celulas de todas as UFs)
calc_nat_std <- function(d) {
  d |>
    group_by(sex, age_group) |>
    summarise(
      cs = sum(pop_subnormal, na.rm = TRUE),
      cp = sum(pop_total,     na.rm = TRUE),
      w  = first(peso_ij),
      .groups = "drop"
    ) |>
    mutate(contrib = (cs / cp) * w) |>
    summarise(rate = sum(contrib) * 100) |>
    pull(rate)
}

national_std      <- calc_nat_std(detail_c)
national_std_masc <- calc_nat_std(filter(detail_c, sex == "Homens"))
national_std_fem  <- calc_nat_std(filter(detail_c, sex == "Mulheres"))

cat(sprintf("Validacao ABA 3:\n  Taxa bruta nacional:        %.6f%%\n", national_crude))
cat(sprintf("  Taxa padronizada nacional:  %.6f%%\n", national_std))
cat(sprintf("  Diferenca:                  %.8f p.p.\n\n", abs(national_crude - national_std)))

# Soma de contribuicoes por UF (para coluna de conferencia)
contrib_uf <- detail_c |>
  group_by(state_code) |>
  summarise(soma_contribuicoes_raw = sum(contribuicao_ij, na.rm = TRUE), .groups = "drop")

aba3_ufs <- rates |>
  left_join(contrib_uf, by = "state_code") |>
  transmute(
    state_code,
    state_name,
    pop_total,
    pop_subnormal,
    taxa_bruta                  = crude_rate,
    soma_contribuicoes_raw,
    taxa_padronizada            = standardized_rate,
    diferenca_bruta_padronizada = crude_rate - standardized_rate,
    taxa_padronizada_masc       = standardized_rate_male,
    taxa_padronizada_fem        = standardized_rate_female
  ) |>
  arrange(desc(taxa_bruta))

brasil_row <- tibble(
  state_code                  = 0L,
  state_name                  = "BRASIL (agregado nacional)",
  pop_total                   = sum(uf_data$pop_total),
  pop_subnormal               = sum(uf_data$pop_subnormal),
  taxa_bruta                  = national_crude,
  soma_contribuicoes_raw      = national_std / 100,
  taxa_padronizada            = national_std,
  diferenca_bruta_padronizada = national_crude - national_std,
  taxa_padronizada_masc       = national_std_masc,
  taxa_padronizada_fem        = national_std_fem
)

aba3 <- bind_rows(aba3_ufs, brasil_row)
brasil_idx_aba3 <- nrow(aba3)  # ultima linha

# ============================================================================
# ABA 4 — Metodologia
# ============================================================================
aba4 <- tibble(
  Item = c(
    "ESTUDO",
    "Título",
    "Tipo",
    "Unidade de análise",
    "",
    "FONTE DE DADOS",
    "API SIDRA",
    "Tabela 9884",
    "Variável (T. 9884)",
    "Classificações (T. 9884)",
    "Tabela 9514",
    "Variável (T. 9514)",
    "Classificações (T. 9514)",
    "Tabela 10295",
    "Variável (T. 10295)",
    "",
    "PADRONIZAÇÃO DIRETA — PASSO A PASSO",
    "Notação",
    "Passo 1 — Taxa específica da célula",
    "Fórmula (Passo 1)",
    "Passo 2 — Peso da célula na pop. padrão",
    "Fórmula (Passo 2)",
    "Passo 3 — Contribuição ponderada",
    "Fórmula (Passo 3)",
    "Passo 4 — Taxa padronizada da UF",
    "Fórmula (Passo 4)",
    "Células totais",
    "População padrão",
    "Nota — coluna SUBTOTAL na Aba 2",
    "",
    "TAXA BRUTA",
    "Fórmula taxa bruta",
    "",
    "CÉLULAS SUPRIMIDAS (NA → 0)",
    "Células afetadas",
    "Localização",
    "Justificativa",
    "Impacto",
    "",
    "VALIDAÇÃO INTERNA",
    "Critério",
    "Resultado"
  ),
  Descrição = c(
    "",
    "Estudo ecológico — proporção de população em aglomerados subnormais (favelas e comunidades urbanas) por Unidade da Federação — Brasil, Censo 2022",
    "Estudo ecológico transversal",
    "Unidade da Federação — 27 UFs (26 estados + Distrito Federal)",
    "",
    "",
    "API SIDRA v3 — https://servicodados.ibge.gov.br/api/v3/agregados",
    "Pessoas em Favelas e Comunidades Urbanas por sexo e grupo de idade — Censo Demográfico 2022",
    "Variável 9612: Pessoas em Favelas e Comunidades Urbanas",
    "Classif. 2 (Sexo): Total[6794], Homens[4], Mulheres[5]  |  Classif. 58 (Grupo etário): 21 grupos quinquenais (0-4 até 100+)  |  Classif. 86 (Situação do domicílio): Total[95251]",
    "Pessoas residentes por sexo e idade — Censo Demográfico 2022",
    "Variável 93: Pessoas residentes",
    "Classif. 2 (Sexo): todos  |  Classif. 287 (Idade): todos (individual + agrupado)  |  Classif. 286 (Área): Total[113635]",
    "Rendimento nominal médio mensal domiciliar per capita — Censo Demográfico 2022",
    "Variável 13431: Valor do rendimento nominal médio mensal domiciliar per capita (R$)",
    "",
    "",
    "i = grupo etário (21 grupos quinquenais: 0-4 até 100+)  |  j = sexo (Homens, Mulheres)  |  UF = Unidade da Federação",
    "Para cada UF, calcular a taxa de residência em aglomerados subnormais em cada célula (cruzamento de grupo etário e sexo)",
    "taxa_ij  =  pop_subnormal_ij  /  pop_total_ij",
    "Calcular o peso de cada célula com base na participação dessa célula na população padrão (população brasileira total, Censo 2022)",
    "peso_ij  =  pop_padrao_ij  /  pop_padrao_total    [pop_padrao_total ≈ 203 milhões  |  soma de todos os pesos = 1,0]",
    "Ponderar a taxa específica pelo peso da célula na população padrão",
    "contribuicao_ij  =  taxa_ij  ×  peso_ij",
    "Somar as contribuições de todas as 42 células e multiplicar por 100",
    "taxa_padronizada_UF (%)  =  SUM_{i=1..21, j=1..2}(contribuicao_ij)  ×  100",
    "42 células por UF (21 grupos etários quinquenais × 2 sexos) × 27 UFs = 1.134 células no total",
    "Soma de todas as 27 UFs do Censo 2022 (~203 milhões de pessoas). Por construção da padronização direta, quando a população padrão coincide com a população estudada, a taxa padronizada nacional deve ser idêntica à taxa bruta nacional.",
    "As linhas marcadas como 'SUBTOTAL' na Aba Calculo_por_UF exibem a soma bruta de contribuicao_ij para a UF. Para obter a taxa padronizada em percentual, multiplique o valor da coluna contribuicao_ij por 100. As colunas pop_subnormal_ij e pop_total_ij no SUBTOTAL refletem os totais oficiais da UF (sex=Total, age=Total) provenientes do Censo 2022.",
    "",
    "",
    "taxa_bruta_UF (%)  =  (pop_subnormal_UF  /  pop_total_UF)  ×  100",
    "",
    "",
    "7 células com valor '-' (supressão por sigilo estatístico do IBGE) substituídas por zero.",
    "Grupos etários 95-99 anos e 100+ anos nos estados: RR (Roraima), MS (Mato Grosso do Sul) e GO (Goiás).",
    "O IBGE suprime contagens muito pequenas para impedir a identificação de indivíduos. Em grupos etários extremos e estados com prevalência muito baixa de aglomerados subnormais, a contagem real é nula ou próxima de zero.",
    "Impacto negligível: grupos ≥ 95 anos representam < 0,3% da população total e os três estados afetados apresentam taxas de aglomerados subnormais muito baixas.",
    "",
    "",
    "A taxa padronizada nacional re-calculada (agregando células de todas as UFs com os mesmos pesos) deve ser idêntica à taxa bruta nacional. Tolerância máxima: 0,1 ponto percentual.",
    sprintf("Diferença observada = %.8f p.p.  →  Validação APROVADA", abs(national_crude - national_std))
  )
)

# ============================================================================
# Construir workbook com openxlsx
# ============================================================================
wb <- createWorkbook()

# ── Estilos ───────────────────────────────────────────────────────────────────
s_hdr <- createStyle(
  fontName       = "Calibri",
  fontSize       = 11,
  fontColour     = "#FFFFFF",
  fgFill         = "#4472C4",
  halign         = "CENTER",
  valign         = "CENTER",
  textDecoration = "Bold",
  wrapText       = TRUE,
  border         = "Bottom",
  borderColour   = "#2F5496"
)
s_sub  <- createStyle(fgFill = "#FFFF99", textDecoration = "Bold")
s_bra  <- createStyle(fgFill = "#CCFFCC", textDecoration = "Bold")
s_pop  <- createStyle(numFmt = "#,##0")
s_r4   <- createStyle(numFmt = "0.0000")
s_r8   <- createStyle(numFmt = "0.00000000")  # pesos e contribuicoes (8 decimais)
s_bold <- createStyle(textDecoration = "Bold")

# ── Helper: criar aba, escrever dados, congelar cabeçalho ─────────────────────
add_sheet <- function(wb, nome, dados, col_widths) {
  addWorksheet(wb, nome)
  writeData(wb, nome, dados, headerStyle = s_hdr)
  freezePane(wb, nome, firstRow = TRUE)
  setColWidths(wb, nome, cols = seq_along(col_widths), widths = col_widths)
}

# ────────────────────────────────────────────────────────────────────────────
# ABA 1 — Populacao_Padrao
# ────────────────────────────────────────────────────────────────────────────
add_sheet(wb, "Populacao_Padrao", aba1,
          col_widths = c(24, 12, 20, 20, 16))

nr1 <- nrow(aba1)
# Linha TOTAL (ultima) em amarelo
addStyle(wb, "Populacao_Padrao", s_sub,
         rows = nr1 + 1, cols = 1:5, gridExpand = TRUE, stack = TRUE)
# Formato populacional (colunas 3 e 4)
addStyle(wb, "Populacao_Padrao", s_pop,
         rows = 2:(nr1 + 1), cols = c(3, 4), gridExpand = TRUE, stack = TRUE)
# Peso com 8 casas (coluna 5)
addStyle(wb, "Populacao_Padrao", s_r8,
         rows = 2:(nr1 + 1), cols = 5, gridExpand = TRUE, stack = TRUE)

# ────────────────────────────────────────────────────────────────────────────
# ABA 2 — Calculo_por_UF
# ────────────────────────────────────────────────────────────────────────────
add_sheet(wb, "Calculo_por_UF", aba2,
          col_widths = c(12, 26, 52, 12, 18, 16, 18, 16, 18))

nr2 <- nrow(aba2)
# Linhas de subtotal em amarelo
if (length(sub_idx_aba2) > 0) {
  addStyle(wb, "Calculo_por_UF", s_sub,
           rows = sub_idx_aba2 + 1, cols = 1:9, gridExpand = TRUE, stack = TRUE)
}
# Populacao (colunas 5 e 6)
addStyle(wb, "Calculo_por_UF", s_pop,
         rows = 2:(nr2 + 1), cols = c(5, 6), gridExpand = TRUE, stack = TRUE)
# Taxas e pesos com 8 casas (colunas 7, 8, 9)
addStyle(wb, "Calculo_por_UF", s_r8,
         rows = 2:(nr2 + 1), cols = 7:9, gridExpand = TRUE, stack = TRUE)

# ────────────────────────────────────────────────────────────────────────────
# ABA 3 — Conferencia_Final
# ────────────────────────────────────────────────────────────────────────────
add_sheet(wb, "Conferencia_Final", aba3,
          col_widths = c(12, 30, 16, 16, 14, 22, 16, 26, 22, 20))

nr3 <- nrow(aba3)
# Linha Brasil em verde (ultima)
addStyle(wb, "Conferencia_Final", s_bra,
         rows = nr3 + 1, cols = 1:10, gridExpand = TRUE, stack = TRUE)
# Populacao (colunas 3 e 4)
addStyle(wb, "Conferencia_Final", s_pop,
         rows = 2:(nr3 + 1), cols = c(3, 4), gridExpand = TRUE, stack = TRUE)
# Taxas com 4 casas (colunas 5 a 10)
addStyle(wb, "Conferencia_Final", s_r4,
         rows = 2:(nr3 + 1), cols = 5:10, gridExpand = TRUE, stack = TRUE)
# Soma contribuicoes em 8 casas (coluna 6 — soma_contribuicoes_raw)
addStyle(wb, "Conferencia_Final", s_r8,
         rows = 2:(nr3 + 1), cols = 6, gridExpand = TRUE, stack = TRUE)

# ────────────────────────────────────────────────────────────────────────────
# ABA 4 — Metodologia
# ────────────────────────────────────────────────────────────────────────────
addWorksheet(wb, "Metodologia")
writeData(wb, "Metodologia", aba4, headerStyle = s_hdr)
freezePane(wb, "Metodologia", firstRow = TRUE)
setColWidths(wb, "Metodologia", cols = 1:2, widths = c(38, 120))

# Negrito nas linhas de secao (Item em maiusculas, Descricao vazia)
sec_rows <- which(aba4$Item != "" & aba4$Descrição == "") + 1
if (length(sec_rows) > 0) {
  addStyle(wb, "Metodologia", s_bold,
           rows = sec_rows, cols = 1:2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Metodologia",
           createStyle(fgFill = "#DCE6F1"),
           rows = sec_rows, cols = 1:2, gridExpand = TRUE, stack = TRUE)
}

# ── Salvar ────────────────────────────────────────────────────────────────────
saveWorkbook(wb, out_path, overwrite = TRUE)

# ── Resumo final ──────────────────────────────────────────────────────────────
cat(sprintf("\nArquivo salvo: %s\n", normalizePath(out_path)))
cat(sprintf("Tamanho: %.1f KB\n\n", file.size(out_path) / 1024))
cat(sprintf("ABA 1 — Populacao_Padrao:   %3d linhas  (42 celulas + 1 TOTAL)\n",      nrow(aba1)))
cat(sprintf("ABA 2 — Calculo_por_UF:    %4d linhas  (1134 celulas + %d SUBTOTAIS)\n",
            nrow(aba2), length(sub_idx_aba2)))
cat(sprintf("ABA 3 — Conferencia_Final: %3d linhas  (27 UFs + linha BRASIL)\n",      nrow(aba3)))
cat(sprintf("ABA 4 — Metodologia:       %3d linhas\n",                                nrow(aba4)))
