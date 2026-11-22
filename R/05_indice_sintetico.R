# 05_indice_sintetico.R — índice sintético do mercado de trabalho por UF (4T2015–3T2026)
# Modelo principal: mín-máx com limites fixos, 3 dimensões, pesos iguais, média geométrica
source("R/00_pacotes.R")

# ---------------------------------------------------------------------------
# PASSO 1 — Baixar o desalento do SIDRA
#   Tabela 6813: percentual de desalentados na força de trabalho ampliada (9869) e CV (9870)
#   Atenção: esta tabela só tem Brasil (n1) e UFs (n3), não tem Grandes Regiões
# ---------------------------------------------------------------------------
tab_6813 <- get_sidra(api = "/t/6813/n1/all/n3/all/v/9869,9870/p/all") |> clean_names()
names(tab_6813)          # conferir o nome da coluna de código do território

col_cod <- grep("^brasil.*_codigo$", names(tab_6813), value = TRUE)   # nome muda sem o n2

desalento <- tab_6813 |>
  transmute(
    nivel = nivel_territorial,
    cod   = .data[[col_cod]],
    ano   = as.integer(substr(trimestre_codigo, 1, 4)),
    tri   = as.integer(substr(trimestre_codigo, 5, 6)),
    indicador = recode(variavel_codigo, "9869" = "desalento", "9870" = "desalento_cv"),
    valor
  ) |>
  pivot_wider(names_from = indicador, values_from = valor)
glimpse(desalento)       # esperado: 28 locais x 59 trimestres = 1.652 linhas

# ---------------------------------------------------------------------------
# PASSO 2 — Juntar com a base do painel (scripts 01 e 02) e ficar só com Brasil + UFs
#   O join usa nivel + cod porque Brasil e Região Norte têm ambos código "1"
# ---------------------------------------------------------------------------
painel <- readRDS("dados/tratados/base_painel.rds") |>
  filter(nivel %in% c("Brasil", "Unidade da Federação")) |>
  left_join(desalento, by = c("nivel", "cod", "ano", "tri"))

siglas <- c("11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
            "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL","28"="SE","29"="BA",
            "31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR","42"="SC","43"="RS",
            "50"="MS","51"="MT","52"="GO","53"="DF")
painel <- painel |>
  mutate(sigla = if_else(nivel == "Brasil", "BR", unname(siglas[as.character(cod)])))
count(painel, nivel)     # esperado: Brasil 59, UF 27 x 59 = 1.593
sum(is.na(painel$desalento))   # esperado: 0

# ---------------------------------------------------------------------------
# PASSO 3 — Limites fixos (mín e máx entre todas as UFs, todos os trimestres)
#   sentido = -1 quando "menor é melhor"; +1 quando "maior é melhor"
#   informalidade só existe desde 2015T4 (na.rm = TRUE cuida disso)
# ---------------------------------------------------------------------------
sentido <- c(desocupacao = -1, subutilizacao = -1, desalento = -1,
             informalidade = -1, rendimento = 1, ocupacao = 1)

limites <- painel |>
  filter(nivel == "Unidade da Federação") |>
  summarise(across(all_of(names(sentido)),
                   list(min = \(x) min(x, na.rm = TRUE), max = \(x) max(x, na.rm = TRUE)))) |>
  pivot_longer(everything(), names_to = c("indicador", ".value"), names_pattern = "(.*)_(min|max)")
limites   # conferir: desocupação 2,1 a 21,8; rendimento 1.663 a 6.881 etc.

# ---------------------------------------------------------------------------
# PASSO 4 — Normalizar (0 a 1), montar as dimensões e o índice
#   Acesso   = média de desocupação, subutilização e desalento
#   Qualidade = média de informalidade e rendimento
#   Inserção = nível da ocupação
#   Índice   = média geométrica das três dimensões
# ---------------------------------------------------------------------------
normaliza <- function(x, ind) {
  lo <- limites$min[limites$indicador == ind]
  hi <- limites$max[limites$indicador == ind]
  if (sentido[[ind]] > 0) (x - lo) / (hi - lo) else (hi - x) / (hi - lo)
}

indice <- painel |>
  mutate(across(all_of(names(sentido)), \(x) normaliza(x, cur_column()), .names = "n_{.col}")) |>
  mutate(
    acesso    = (n_desocupacao + n_subutilizacao + n_desalento) / 3,
    qualidade = (n_informalidade + n_rendimento) / 2,
    insercao  = n_ocupacao,
    indice    = (acesso * qualidade * insercao)^(1/3),
    periodo   = paste0(ano, "T", tri)
  ) |>
  filter(!is.na(indice)) |>                      # série começa em 2015T4 (informalidade)
  group_by(ano, tri) |>
  mutate(ranking = if_else(sigla == "BR", NA_integer_,
                           min_rank(desc(if_else(sigla == "BR", NA_real_, indice))))) |>
  ungroup()

# ---------------------------------------------------------------------------
# PASSO 5 — Validar contra o que já calculamos
#   3T2026: SC 0,897 (1º) | DF 0,888 (2º) | Brasil 0,695 | AL em 2020T3 = 0
# ---------------------------------------------------------------------------
indice |>
  filter((ano == 2026 & tri == 3 & sigla %in% c("SC", "DF", "BR")) |
         (periodo == "2020T3" & sigla == "AL")) |>
  select(periodo, sigla, acesso, qualidade, insercao, indice, ranking) |>
  mutate(across(where(is.double), \(x) round(x, 3)))

range(indice$periodo); n_distinct(indice$periodo)   # esperado: 2015T4 a 2026T3, 44 trimestres

# ---------------------------------------------------------------------------
# PASSO 6 — Gráfico: uma curva por UF, com o Brasil como referência
# ---------------------------------------------------------------------------
ordem <- indice |> filter(ano == 2026, tri == 3) |>
  arrange(sigla != "BR", ranking) |>
  mutate(rotulo = if_else(sigla == "BR", "Brasil", paste0(ranking, "º ", sigla)))

serie <- indice |>
  left_join(select(ordem, sigla, rotulo), by = "sigla") |>
  mutate(rotulo = factor(rotulo, levels = ordem$rotulo),
         t = ano + (tri - 1) / 4)
ref_br <- serie |> filter(sigla == "BR") |> select(t, indice)

g_indice <- ggplot(serie, aes(t, indice)) +
  annotate("rect", xmin = 2020.25, xmax = 2021.75, ymin = -Inf, ymax = Inf, fill = "#eef3f8") +
  geom_line(data = ref_br, colour = "#6baed6", linewidth = 0.6) +
  geom_line(colour = "#08519c", linewidth = 0.8) +
  geom_point(data = \(d) filter(d, ano == 2026, tri == 3), colour = "#08519c", size = 1.4) +
  facet_wrap(~ rotulo, ncol = 7) +
  scale_y_continuous(limits = c(0, 1), labels = \(x) format(x, decimal.mark = ",")) +
  scale_x_continuous(breaks = c(2016, 2019, 2022, 2025)) +
  labs(title = "Índice sintético do mercado de trabalho por UF, 4T2015 a 3T2026",
       subtitle = "Azul escuro: UF. Azul claro: Brasil. Faixa: 2T2020 a 4T2021 (pandemia). Painéis na ordem do ranking do 3T2026.",
       x = NULL, y = "Índice (0 a 1)",
       caption = "Mín-máx com limites fixos (UFs, 2012T1–2026T3); índice = média geométrica de Acesso, Qualidade e Inserção.\nFonte: PNAD Contínua Trimestral. Elaboração própria.") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", hjust = 0))
g_indice

# ---------------------------------------------------------------------------
# PASSO 7 — Salvar (nomes novos: não sobrescreve os arquivos já existentes em saidas/)
# ---------------------------------------------------------------------------
indice_out <- indice |>
  select(nivel, cod, sigla, local, ano, tri, periodo, starts_with("n_"),
         acesso, qualidade, insercao, indice, ranking)

saveRDS(indice_out, "dados/tratados/indice_sintetico.rds")
write_csv(indice_out, "dados/tratados/indice_sintetico.csv")
saveRDS(limites, "dados/tratados/indice_sintetico_limites.rds")
ggsave("saidas/indice_sintetico_serie_R.png", g_indice, width = 14, height = 9, dpi = 150, bg = "white")
