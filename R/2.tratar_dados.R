#### Packages -----------
rm(list = ls())
needs::needs(tidyverse, vroom)


#### DADOS -------------

discentes <- read_rds("dados/tidy/discentes.rds")
producao_artigos_periodicos <- read_rds("dados/tidy/producao_artigos_periodicos.rds")
teses_dissertacoes <- read_rds("dados/tidy/teses_dissertacoes.rds")



discentes_pb <- discentes |> filter(SG_UF_PROGRAMA == "PB")
discentes_pb |> write_rds("dados/tidy/discentes_pb.rds")

teses_dissertacoes_pb <- teses_dissertacoes |> filter(SG_UF_IES == "PB")
teses_dissertacoes_pb |> write_rds("dados/tidy/teses_dissertacoes_pb.rds")

# 23931 sem tese/dissertação; 25119 com tese/dissertação
discentes_pb$ID_PESSOA %in% teses_dissertacoes_pb$ID_PESSOA_DISCENTE |> table()


