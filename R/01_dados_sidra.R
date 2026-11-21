# 01_dados_sidra.R — séries trimestrais do SIDRA (PNAD Contínua)
source("R/00_pacotes.R")

# Tabela 4099: desocupação (4099) e subutilização (4118), com CVs (4103, 4119)
# n1 = Brasil, n2 = Grandes Regiões, n3 = UFs; p/all = todos os trimestres
tab_4099 <- get_sidra(api = "/t/4099/n1/all/n2/all/n3/all/v/4099,4103,4118,4119/p/all")
tab_4099 <- clean_names(tab_4099)

# --- Organização: uma linha por local x trimestre ---
desoc <- tab_4099 |>
  transmute(
    nivel  = nivel_territorial,
    cod    = brasil_grande_regiao_e_unidade_da_federacao_codigo,
    local  = brasil_grande_regiao_e_unidade_da_federacao,
    ano    = as.integer(substr(trimestre_codigo, 1, 4)),
    tri    = as.integer(substr(trimestre_codigo, 5, 6)),
    data   = as.Date(paste0(ano, "-", tri * 3 - 2, "-01")),   # 1º mês do trimestre
    indicador = recode(variavel_codigo,
                       "4099" = "desocupacao",   "4103" = "desocupacao_cv",
                       "4118" = "subutilizacao", "4119" = "subutilizacao_cv"),
    valor
  ) |>
  pivot_wider(names_from = indicador, values_from = valor) |>
  mutate(regiao_cod = if_else(nivel == "Unidade da Federação", substr(cod, 1, 1), NA_character_))

glimpse(desoc)

# Primeira espiada: 5 UFs com maior desocupação no último trimestre
desoc |>
  filter(ano == 2026, tri == 3, nivel == "Unidade da Federação") |>
  arrange(desc(desocupacao)) |>
  select(local, desocupacao, desocupacao_cv) |>
  head(5)
