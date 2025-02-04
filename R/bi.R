pacman::p_load(tidyverse, vroom, readxl, genderBR)
rm(list = ls())

editais_fapesq <- readr::read_rds("dados/tidy/editais_fapesq.rds")
discentes_pb <- readr::read_rds("dados/tidy/discentes_pb.rds")
bolsistas_capes <- readr::read_rds("dados/tidy/bolsas_pb.rds")
bolsistas_cnpq <- readr::read_rds("dados/tidy/bolsas_cnpq_pb.rds")

bolsistas_fapesq <- editais_fapesq |> 
  mutate(INICIO_BOLSA = as.Date(INICIO_BOLSA), FIM_BOLSA = as.Date(FIM_BOLSA)) |> 
  mutate(INICIO_BOLSA = lubridate::year(INICIO_BOLSA), FIM_BOLSA = lubridate::year(FIM_BOLSA)) |> 
  filter(!is.na(INICIO_BOLSA) & !is.na(FIM_BOLSA)) |>
  transmute(NM_DISCENTE, DS_GRAU_ACADEMICO_DISCENTE, NR_DOCUMENTO_DISCENTE, EDITAL, SG_ENTIDADE_ENSINO, GENERO, NM_PROGRAMA_IES, VL_BOLSA_MES, ANO = map2(INICIO_BOLSA, FIM_BOLSA, seq)) |>
  unnest(cols = c(ANO))  


bolsistas_fapesq |> 
  dplyr::left_join(discentes_pb, by = c("NM_DISCENTE", "SG_ENTIDADE_ENSINO", "DS_GRAU_ACADEMICO_DISCENTE", "ANO", "NR_DOCUMENTO_DISCENTE")) |> View()

discentes_pb |> count(SG_ENTIDADE_ENSINO)
