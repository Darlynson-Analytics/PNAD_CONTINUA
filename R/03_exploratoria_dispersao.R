# 03_exploratoria_dispersao.R — as discrepâncias entre UFs estão diminuindo ou aumentando?
source("R/00_pacotes.R")

painel <- readRDS("dados/tratados/base_painel.rds")
indicadores <- c("desocupacao", "subutilizacao", "ocupacao", "informalidade", "rendimento")

# 1) Só as 27 UFs, em formato longo: uma linha por UF x trimestre x indicador
ufs_longo <- painel |>
  filter(nivel == "Unidade da Federação") |>
  select(local, ano, tri, data, all_of(indicadores)) |>
  pivot_longer(all_of(indicadores), names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor))

# 2) Medidas de dispersão entre UFs, a cada trimestre
#    (cada UF pesa igual: medimos a distância entre territórios, não entre pessoas)
dispersao <- ufs_longo |>
  group_by(indicador, data, ano, tri) |>
  summarise(
    media  = mean(valor),
    dp     = sd(valor),
    cv     = 100 * dp / media,          # coeficiente de variação entre UFs (%)
    minimo = min(valor),
    maximo = max(valor),
    razao  = maximo / minimo,           # quantas vezes o maior é o menor
    uf_min = local[which.min(valor)],
    uf_max = local[which.max(valor)],
    .groups = "drop"
  )
glimpse(dispersao)

# 3) Início x fim, sempre no 3º trimestre (evita efeito sazonal)
#    informalidade começa em 2015T4, então o "início" dela é 2016T3
dispersao |>
  filter(tri == 3) |>
  group_by(indicador) |>
  filter(ano == min(ano) | ano == max(ano)) |>
  select(indicador, ano, cv, razao, uf_min, uf_max) |>
  mutate(across(c(cv, razao), \(x) round(x, 2))) |>
  print(n = Inf)

# 4) Gráfico: evolução do CV entre UFs
nomes <- c(desocupacao = "Taxa de desocupação", subutilizacao = "Taxa de subutilização",
           ocupacao = "Nível da ocupação", informalidade = "Taxa de informalidade",
           rendimento = "Rendimento médio real")

g_cv <- ggplot(dispersao, aes(data, cv)) +
  geom_line(colour = "#1f5f8b", linewidth = 0.8) +
  facet_wrap(~ indicador, scales = "free_y", ncol = 2, labeller = as_labeller(nomes)) +
  labs(title = "Dispersão entre as 27 UFs ao longo do tempo",
       subtitle = "Coeficiente de variação entre UFs (%), PNAD Contínua trimestral",
       x = NULL, y = "CV entre UFs (%)", caption = "Fonte: IBGE, SIDRA. Elaboração própria.") +
  theme_minimal(base_size = 11)
g_cv

ggsave("saidas/cv_entre_ufs.png", g_cv, width = 9, height = 7, dpi = 150, bg = "white")
saveRDS(dispersao, "dados/tratados/dispersao_ufs.rds")
