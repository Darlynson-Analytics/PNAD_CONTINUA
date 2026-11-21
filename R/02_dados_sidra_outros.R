# 02_dados_sidra_outros.R — nível de ocupação, informalidade e rendimento (SIDRA)
source("R/00_pacotes.R")

# Tabelas (PNAD Contínua trimestral; n1 = Brasil, n2 = Grandes Regiões, n3 = UFs):
#   6466: nível da ocupação (4097) e CV (4101)                 — desde 2012T1
#   8529: taxa de informalidade (12466) e CV (12467)           — desde 2015T4
#   6472: rendimento médio real habitual, todos os trabalhos (5933) e CV (5941)
consultas <- c(
  "/t/6466/n1/all/n2/all/n3/all/v/4097,4101/p/all",
  "/t/8529/n1/all/n2/all/n3/all/v/12466,12467/p/all",
  "/t/6472/n1/all/n2/all/n3/all/v/5933,5941/p/all"
)

# Baixa as três tabelas e empilha num único data frame
brutos <- map(consultas, \(api) get_sidra(api = api) |> clean_names()) |> list_rbind()
count(brutos, variavel_codigo)

# Mesma organização do script 01: uma linha por local x trimestre
outros <- brutos |>
  transmute(
    nivel = nivel_territorial,   # necessário: Brasil e a Região Norte têm ambos código "1"
    cod  = brasil_grande_regiao_e_unidade_da_federacao_codigo,
    ano  = as.integer(substr(trimestre_codigo, 1, 4)),
    tri  = as.integer(substr(trimestre_codigo, 5, 6)),
    indicador = recode(variavel_codigo,
                       "4097"  = "ocupacao",      "4101"  = "ocupacao_cv",
                       "12466" = "informalidade", "12467" = "informalidade_cv",
                       "5933"  = "rendimento",    "5941"  = "rendimento_cv"),
    valor
  ) |>
  pivot_wider(names_from = indicador, values_from = valor)
glimpse(outros)   # deve ter 1.947 linhas e colunas <dbl> (não <list>)

# Junta com a base de desocupação/subutilização do script 01
desoc <- readRDS("dados/tratados/desocupacao_subutilizacao.rds")
painel <- desoc |> left_join(outros, by = c("nivel", "cod", "ano", "tri"))
glimpse(painel)

# Espiada: as 5 UFs de maior e menor rendimento no último trimestre
painel |>
  filter(ano == 2026, tri == 3, nivel == "Unidade da Federação") |>
  arrange(desc(rendimento)) |>
  select(local, rendimento, informalidade, ocupacao) |>
  slice(c(1:5, 23:27))

# Salvar a base consolidada do painel
saveRDS(painel, "dados/tratados/base_painel.rds")
write_csv(painel, "dados/tratados/base_painel.csv")
