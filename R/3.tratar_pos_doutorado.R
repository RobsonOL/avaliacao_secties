# Pós-Doutorado 
rm(list = ls())
pacman::p_load(tidyverse)

# DADOS ------
artigos_autor_pb <- readr::read_rds("dados/tidy/artigos_autor_pb.rds") |> 
  dplyr::filter(
    !is.na(SG_ESTRATO), SG_ESTRATO != "NI"
  ) |>
  dplyr::group_by(
    NM_AUTOR, SG_ESTRATO
  ) |>
  dplyr::summarise(
    ARTIGOS = n()
  ) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(
    id_cols = NM_AUTOR, 
    names_from = SG_ESTRATO, 
    values_from = ARTIGOS, 
    names_prefix = "ARTIGO_", values_fill = 0
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    TOTAL_ARTIGOS = sum(c_across(starts_with('ARTIGO_')), na.rm = TRUE)
  ) 


fapesq <- readr::read_rds("dados/tidy/editais_fapesq.rds")|> 
  dplyr::mutate(TIPO_BOLSA = "FAPESQ") |>
  dplyr::select(-TIPO_BOLSA_MAIS_COMUM) |> 
  dplyr::filter(DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO") |> 
  dplyr::mutate(ANO = lubridate::year(INICIO_BOLSA)) |> 
  dplyr::mutate(IDADE = ANO - lubridate::year(DT_NASCIMENTO_DISCENTE))

fapesq_artigos <- fapesq |> 
  left_join(artigos_autor_pb, by = c("NM_DISCENTE" = "NM_AUTOR"))



fapesq_artigos|> readr::write_rds("dados/tidy/fapesq_pos_doutorado.rds")

fapesq_artigos |> readr::write_csv("dados/tidy/fapesq_pos_doutorado.csv")












# DEMAIS BOLSAS ----------
cnpq <- read_rds("dados/tidy/bolsas_cnpq_pb.rds") |>
  dplyr::filter(DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO") |>
  dplyr::mutate(TIPO_BOLSA = "CNPQ") |> 
  dplyr::group_by(NM_DISCENTE,
                  ANO,
                  TIPO_BOLSA,
                  DS_GRAU_ACADEMICO_DISCENTE,
                  SG_ENTIDADE_ENSINO) |>
  dplyr::summarise(VL_BOLSA_ANO = sum(VL_BOLSA_ANO, na.rm = TRUE)) |>
  dplyr::ungroup()

cnpq |> readr::write_rds("dados/tidy/cnpq_pos_doutorado.rds")

capes <- read_rds("dados/tidy/bolsas_pb.rds") |> 
  dplyr::mutate(TIPO_BOLSA = "CAPES") |>
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
  dplyr::ungroup() |> 
  dplyr::distinct(.keep_all = TRUE) 


capes |> readr::write_rds("dados/tidy/capes_pos_doutorado.rds")



# Unir as bases ----
df <- fapesq |> 
  dplyr::bind_rows(cnpq) |>
  dplyr::bind_rows(capes)

