pacman::p_load(tidyverse, vroom, readxl, genderBR)
rm(list = ls())

editais_fapesq <- readr::read_rds("dados/tidy/editais_fapesq.rds")
discentes_pb <- readr::read_rds("dados/tidy/discentes_pb.rds") |> 
  dplyr::select(NM_DISCENTE, SG_ENTIDADE_ENSINO, DS_GRAU_ACADEMICO_DISCENTE, 
                ANO, NR_DOCUMENTO_DISCENTE, NM_PROGRAMA_IES, ID_PESSOA) |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 1]) |> 
  dplyr::mutate(NM_DISCENTE_SEGUNDO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 2])

  
bolsistas_capes <- readr::read_rds("dados/tidy/bolsas_pb.rds")
bolsistas_cnpq <- readr::read_rds("dados/tidy/bolsas_cnpq_pb.rds")

bolsistas_fapesq <- editais_fapesq |> 
  mutate(INICIO_BOLSA = as.Date(INICIO_BOLSA), FIM_BOLSA = as.Date(FIM_BOLSA)) |> 
  mutate(INICIO_BOLSA = lubridate::year(INICIO_BOLSA), FIM_BOLSA = lubridate::year(FIM_BOLSA)) |> 
  filter(!is.na(INICIO_BOLSA) & !is.na(FIM_BOLSA)) |>
  transmute(NM_DISCENTE, DS_GRAU_ACADEMICO_DISCENTE, NR_DOCUMENTO_DISCENTE, EDITAL, SG_ENTIDADE_ENSINO, GENERO, NM_PROGRAMA_IES, VL_BOLSA_MES, ANO = map2(INICIO_BOLSA, FIM_BOLSA, seq)) |>
  unnest(cols = c(ANO)) |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 1]) |> 
  dplyr::mutate(NM_DISCENTE_SEGUNDO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 2])



df <- bolsistas_fapesq |> 
  dplyr::left_join(discentes_pb,
                   by = c("NM_DISCENTE_PRIMEIRO_NOME", 
                          "NM_DISCENTE_SEGUNDO_NOME",
                          "SG_ENTIDADE_ENSINO", 
                          "DS_GRAU_ACADEMICO_DISCENTE", 
                          "ANO")
                   ) |> 
  dplyr::select(ID_PESSOA, NM_DISCENTE.x, NM_DISCENTE.y, NM_DISCENTE_PRIMEIRO_NOME, NM_DISCENTE_SEGUNDO_NOME, SG_ENTIDADE_ENSINO, DS_GRAU_ACADEMICO_DISCENTE, ANO, EDITAL, NM_PROGRAMA_IES.x, NM_PROGRAMA_IES.y, GENERO, VL_BOLSA_MES)


df |> filter(is.na(ID_PESSOA)) |> 
  # distinct(NM_DISCENTE, .keep_all = T) |> 
  filter(DS_GRAU_ACADEMICO_DISCENTE != "POS-DOUTORADO") |>
  filter(EDITAL != "Edital 03/2016") |> 
  select(NM_DISCENTE.x, NM_DISCENTE.y, ANO, SG_ENTIDADE_ENSINO, DS_GRAU_ACADEMICO_DISCENTE, EDITAL, NM_PROGRAMA_IES.x, NM_PROGRAMA_IES.y) |> View()


discentes_pb |> 
  select(NM_DISCENTE, ANO, SG_ENTIDADE_ENSINO, DS_GRAU_ACADEMICO_DISCENTE, NM_PROGRAMA_IES) |> View()
  




PATH_DADOS <- "C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/"
discentes <- read_rds(paste0(PATH_DADOS, "discentes.rds"))

discentes |> 
  select(NM_DISCENTE, AN_BASE, SG_ENTIDADE_ENSINO, DS_GRAU_ACADEMICO_DISCENTE, NM_PROGRAMA_IES) |> View()
