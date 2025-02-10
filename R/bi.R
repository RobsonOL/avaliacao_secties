pacman::p_load(tidyverse, vroom, readxl, genderBR)
rm(list = ls())

bolsistas_cnpq <- readr::read_rds("dados/tidy/bolsas_cnpq_pb.rds")
editais_fapesq <- readr::read_rds("dados/tidy/editais_fapesq.rds")

discentes_pb <- readr::read_rds("dados/tidy/discentes_pb.rds") |> 
  dplyr::select(NM_DISCENTE, SG_ENTIDADE_ENSINO, DS_GRAU_ACADEMICO_DISCENTE, 
                ANO, NR_DOCUMENTO_DISCENTE, NM_PROGRAMA_IES, ID_PESSOA) |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 1]) |> 
  dplyr::mutate(NM_DISCENTE_SEGUNDO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 2])

bolsistas_capes <- readr::read_rds("dados/tidy/bolsas_pb.rds") |> 
  dplyr::select(ANO, ID_PESSOA, NM_DISCENTE, SG_IES_ESTUDO, NM_PROGRAMA_PPG, VL_BOLSA_ANO, DS_NIVEL) |> 
  dplyr::filter(DS_NIVEL %in% c("MESTRADO", "DOUTORADO")) |> 
  dplyr::mutate(SG_IES_ESTUDO = dplyr::case_when(
    SG_IES_ESTUDO == "UFPB-JP" ~ "UFPB",
    SG_IES_ESTUDO == "UFPB-RT" ~ "UFPB",
    SG_IES_ESTUDO == "UFPB-AREIA" ~ "UFPB",
    SG_IES_ESTUDO == "UNIPÊ" ~ "UNIPE",
    TRUE ~ SG_IES_ESTUDO
  )) |> 
  dplyr::mutate(TIPO_BOLSA = "CAPES") |> 
  dplyr::rename(SG_ENTIDADE_ENSINO = SG_IES_ESTUDO, DS_GRAU_ACADEMICO_DISCENTE = DS_NIVEL)


# Fapesq só tem valor de bolsa por mês. 
# Converter em bolsa anual (primeiro precisa calcular a quantidade de bolsas por ano)
contagem_bolsas_ano <- editais_fapesq |> 
  dplyr::select(NM_DISCENTE, DS_GRAU_ACADEMICO_DISCENTE, SG_ENTIDADE_ENSINO, INICIO_BOLSA, FIM_BOLSA) |> 
  dplyr::mutate(INICIO_ANO = lubridate::year(INICIO_BOLSA), FIM_ANO = lubridate::year(FIM_BOLSA)) |> 
  dplyr::filter(!is.na(INICIO_ANO) & !is.na(FIM_ANO)) |>
  dplyr::mutate(MES_BOLSA = purrr::map2(INICIO_BOLSA, FIM_BOLSA, seq, by = "month")) |> 
  unnest(MES_BOLSA) |>
  dplyr::mutate(ANO = lubridate::year(MES_BOLSA)) |>
  dplyr::group_by(NM_DISCENTE, DS_GRAU_ACADEMICO_DISCENTE, SG_ENTIDADE_ENSINO, ANO) |>
  dplyr::summarise(QT_BOLSAS_ANO = n(), .groups = 'drop')


bolsistas_fapesq <- editais_fapesq |> 
  mutate(INICIO_BOLSA = as.Date(INICIO_BOLSA), FIM_BOLSA = as.Date(FIM_BOLSA)) |> 
  mutate(INICIO_BOLSA = lubridate::year(INICIO_BOLSA), FIM_BOLSA = lubridate::year(FIM_BOLSA)) |> 
  filter(!is.na(INICIO_BOLSA) & !is.na(FIM_BOLSA)) |>
  transmute(NM_DISCENTE, DS_GRAU_ACADEMICO_DISCENTE, NR_DOCUMENTO_DISCENTE, EDITAL, SG_ENTIDADE_ENSINO, GENERO, NM_PROGRAMA_IES, VL_BOLSA_MES, ANO = map2(INICIO_BOLSA, FIM_BOLSA, seq)) |>
  unnest(cols = c(ANO)) |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 1]) |> 
  dplyr::mutate(NM_DISCENTE_SEGUNDO_NOME = str_split(NM_DISCENTE, " ", simplify = T)[, 2]) |> 
  dplyr::left_join(contagem_bolsas_ano, by = c("NM_DISCENTE", "DS_GRAU_ACADEMICO_DISCENTE", "SG_ENTIDADE_ENSINO", "ANO")) |> 
  dplyr::mutate(VL_BOLSA_ANO = VL_BOLSA_MES * QT_BOLSAS_ANO) 



