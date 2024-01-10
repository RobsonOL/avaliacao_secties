#### Packages -----------
rm(list = ls())
needs::needs(tidyverse, vroom)

#### DADOS -------------

# Quantidade de bolsas por programa de pós-graduação ---------------------------
quantidade_bolsas <- list.files(path = "dados/brutos/concessao/", pattern = "*.zip", full.names = TRUE) |> 
  map_df(read_csv, locale = locale(encoding = 'latin5'))

quantidade_bolsas |> write_rds("dados/brutos/quantidade_bolsas.rds")


# Informações dos discentes de pós-graduação -----------------------------------
# https://dadosabertos.capes.gov.br/dataset/discentes-da-pos-graduacao-stricto-sensu-do-brasil-2017-a-2019

discentes <- list.files(path = "dados/brutos/discentes/", pattern = "*.zip", full.names = TRUE) |>  
  map_df(vroom, delim = ";", locale = locale(encoding = 'latin5'), col_types = cols(.default = "c")) 

discentes |> write_rds("dados/brutos/discentes.rds")


# Produção de artigos em periódicos --------------------------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-producao-intelectual-de-pos-graduacao-stricto-sensu-no-brasil
producao_artigos_periodicos <- list.files(path = "dados/brutos/producao/",
                                          pattern = "artpe",
                                          full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

producao_artigos_periodicos |> write_rds("dados/brutos/producao_artigos_periodicos.rds")


# Teses e dissertações de discentes de pós-graduação ---------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-2020-catalogo-de-teses-e-dissertacoes-da-capes
teses_dissertacoes <- list.files(path = "dados/brutos/teses_dissertacoes/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

teses_dissertacoes |> write_rds("dados/brutos/teses_dissertacoes.rds")
