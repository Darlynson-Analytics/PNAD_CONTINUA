# 04_composicao_populacao.R — composição da população por condição na força de trabalho
source("R/00_pacotes.R")

# 5917: população total (606), em mil pessoas
# 4093: pessoas 14+ (1641), ocupadas (4090), desocupadas (4092), fora da força (4094)
# c2/6794 = sexo Total; n1 = Brasil, n2 = Grandes Regiões, n3 = UFs
consultas <- c(
  "/t/5917/n1/all/n2/all/n3/all/v/606/p/all/c2/6794",
  "/t/4093/n1/all/n2/all/n3/all/v/1641,4090,4092,4094/p/all/c2/6794"
)

brutos_pop <- map(consultas, \(api) get_sidra(api = api) |> clean_names()) |> list_rbind()
count(brutos_pop, variavel_codigo)
