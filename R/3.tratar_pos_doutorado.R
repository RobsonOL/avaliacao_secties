# Pós-Doutorado 
rm(list = ls())
pacman::p_load(tidyverse)

# DADOS ------
fapesq <- readr::read_rds("dados/tidy/editais_fapesq.rds")|> 
  dplyr::filter(DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO")


fapesq |> readr::write_rds("dados/tidy/fapesq_pos_doutorado.rds")

cnpq <- read_rds("dados/tidy/bolsas_cnpq_pb.rds") |>
  dplyr::filter(DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO") |>
  dplyr::group_by(NM_DISCENTE,
                  ANO,
                  TIPO_BOLSA,
                  DS_GRAU_ACADEMICO_DISCENTE,
                  SG_ENTIDADE_ENSINO) |>
  dplyr::summarise(VL_BOLSA_ANO = sum(VL_BOLSA_ANO, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::rename(VL_BOLSA_CNPQ = VL_BOLSA_ANO)

cnpq |> readr::write_rds("dados/tidy/cnpq_pos_doutorado.rds")

capes <- read_rds("dados/tidy/bolsas_pb.rds") |> 
  dplyr::select(-c(DS_PROJETO, CONCEITO_ANO,
                   contains(
                     c("ORIGEM", "TAXA", "MULTICAMPI", "STATUS")
                   ))) |>
  dplyr::filter(ANO >= 2017,
                DS_NIVEL %in% c("ESTÁGIO PÓS-DOUTORAL")) |>
  dplyr::mutate(DS_NIVEL = ifelse(DS_NIVEL == "ESTÁGIO PÓS-DOUTORAL", "POS-DOUTORADO", DS_NIVEL)) |> 
  dplyr::mutate(
    SG_IES_ESTUDO = dplyr::case_match(
      SG_IES_ESTUDO,
      "UFPB-JP" ~ "UFPB",
      "UFPB-RT" ~ "UFPB",
      "UFPB-AREIA" ~ "UFPB",
      .default = SG_IES_ESTUDO
    )
  ) |>
  dplyr::rename(SG_ENTIDADE_ENSINO = SG_IES_ESTUDO) |>
  dplyr::mutate(
    SG_PROGRAMA_CAPES = dplyr::case_match(
      SG_PROGRAMA_CAPES,
      "DS" ~ "CAPES",
      "FAPESQ" ~ "CAPES_FAPESQ",
      .default = "OUTROS"
    )
  ) |>
  dplyr::group_by(ID_PESSOA, ANO, DS_NIVEL, CD_PROGRAMA_PPG, SG_ENTIDADE_ENSINO, SG_PROGRAMA_CAPES) |>
  dplyr::mutate(QT_BOLSA_ANO = sum(QT_BOLSA_ANO),
                VL_BOLSA_ANO = sum(VL_BOLSA_ANO)) |> 
  dplyr::distinct(.keep_all = TRUE) 


capes |> readr::write_rds("dados/tidy/capes_pos_doutorado.rds")
