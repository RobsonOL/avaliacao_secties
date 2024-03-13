# PACOTES ----
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, janitor, readr, tidyr, genderBR, geobr)

# DADOS ----

discentes_pb <- readr::read_rds("dados/tidy/discentes_pb.rds")
teses_pb <- readr::read_rds("dados/tidy/teses_dissertacoes_pb.rds")
bolsas_pb <- readr::read_rds("dados/tidy/bolsas_pb.rds") 
artigos_autor_pb <- readr::read_rds("dados/tidy/artigos_autor_pb.rds")
cnpq_pb <- readr::read_rds("dados/tidy/bolsas_cnpq_pb.rds")
editais_fapesq <- readr::read_rds("dados/tidy/editais_fapesq.rds")

## Dimensão Discente ----

# Funcao para calcular a moda
get_mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


dim_discentes <- discentes_pb |> 
  dplyr::filter(
    dplyr::between(ANO, 2017, 2020)
    ) |> 
  dplyr::group_by(
    NM_DISCENTE, AN_NASCIMENTO_DISCENTE
  ) |> 
  dplyr::reframe(
    across(
      c(NR_DOCUMENTO_DISCENTE, ID_PESSOA),
      ~get_mode(.x)
    )
  )

# dim_discentes |> write_rds("dados/tidy/dim_discentes.rds")

df_discentes <- dim_discentes |> 
  dplyr::mutate(
    GENERO = genderBR::get_gender(NM_DISCENTE)
    ) |> 
  dplyr::mutate(
    GENERO = dplyr::case_match(
      GENERO,
      "Male" ~ "MASCULINO",
      "Female" ~ "FEMININO")
    ) |> 
  dplyr::distinct(ID_PESSOA, .keep_all = TRUE) |> 
  dplyr::left_join(
    discentes_pb |> 
      dplyr::filter(
        dplyr::between(ANO, 2017, 2020) # FAPESQ só começou a ser informado em 2017
        ) |> 
      dplyr::select(
        ANO, ID_PESSOA,
        NM_SITUACAO_DISCENTE, DT_SITUACAO_DISCENTE, DT_MATRICULA_DISCENTE,
        DS_GRAU_ACADEMICO_DISCENTE, NM_PAIS_NACIONALIDADE_DISCENTE,
        SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_PROGRAMA_IES, 
        NM_MUNICIPIO_PROGRAMA_IES, CD_CONCEITO_CURSO, CD_CONCEITO_PROGRAMA,
        QT_MES_TITULACAO, NM_ORIENTADOR_PRINCIPAL,
        CD_AREA_AVALIACAO, NM_AREA_AVALIACAO),
    by = c("ID_PESSOA")
    ) |> 
  dplyr::group_by(
    ID_PESSOA,
    NM_DISCENTE,
    AN_NASCIMENTO_DISCENTE,
    NR_DOCUMENTO_DISCENTE,
    GENERO,
    DS_GRAU_ACADEMICO_DISCENTE,
    CD_PROGRAMA_IES,
    NM_MUNICIPIO_PROGRAMA_IES,
    SG_ENTIDADE_ENSINO,
    NM_AREA_AVALIACAO,
    NM_PAIS_NACIONALIDADE_DISCENTE
  ) |> 
  dplyr::summarise(
    DT_MATRICULA = dplyr::first(DT_MATRICULA_DISCENTE),
    NM_SITUACAO_DISCENTE = NM_SITUACAO_DISCENTE[which.max(DT_SITUACAO_DISCENTE)]
    ) |> 
  dplyr::ungroup() |> 
  dplyr::distinct() |> 
  dplyr::mutate(
    SG_ENTIDADE_ENSINO = dplyr::case_match(
      SG_ENTIDADE_ENSINO,
      "UFPB-JP" ~ "UFPB",
      "UFPB-RT" ~ "UFPB",
      "UFPB-AREIA" ~ "UFPB",
      .default = SG_ENTIDADE_ENSINO)
    )
  

## Bolsas ----

df_bolsas <- bolsas_pb |>
  dplyr::select(-c(DS_PROJETO, CONCEITO_ANO,
                   contains(
                     c("ORIGEM", "TAXA", "MULTICAMPI", "STATUS")
                   ))) |>
  dplyr::filter(ANO >= 2017,
                DS_NIVEL %in% c("DOUTORADO", "MESTRADO", "ESTÁGIO PÓS-DOUTORAL")) |>
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
    SG_PROGRAMA_CAPES = case_match(
      SG_PROGRAMA_CAPES,
      "DS" ~ "CAPES",
      "FAPESQ" ~ "CAPES_FAPESQ",
      .default = "OUTROS"
    )
  ) |>
  dplyr::group_by(ID_PESSOA, ANO, DS_NIVEL, CD_PROGRAMA_PPG, SG_ENTIDADE_ENSINO, SG_PROGRAMA_CAPES) |>
  dplyr::mutate(QT_BOLSA_ANO = sum(QT_BOLSA_ANO),
                VL_BOLSA_ANO = sum(VL_BOLSA_ANO)) |> 
  dplyr::distinct(.keep_all = TRUE) |> 
  dplyr::group_by(NM_DISCENTE,
                  ID_PESSOA,
                  ANO,
                  DS_NIVEL,
                  CD_PROGRAMA_PPG,
                  SG_ENTIDADE_ENSINO) |>
  tidyr::pivot_wider(names_from = SG_PROGRAMA_CAPES,
                     values_from = c(QT_BOLSA_ANO, VL_BOLSA_ANO)) |> 
  dplyr::mutate(
    QT_BOLSA_TOTAL = sum(
      QT_BOLSA_ANO_CAPES,
      QT_BOLSA_ANO_CAPES_FAPESQ,
      QT_BOLSA_ANO_OUTROS,
      na.rm = TRUE
    ),
    VL_BOLSA_TOTAL = sum(
      VL_BOLSA_ANO_CAPES,
      VL_BOLSA_ANO_CAPES_FAPESQ,
      VL_BOLSA_ANO_OUTROS,
      na.rm = TRUE
    )
  ) |>
  dplyr::ungroup() |> 
  dplyr::group_by(NM_DISCENTE, ID_PESSOA, DS_NIVEL, CD_PROGRAMA_PPG, SG_ENTIDADE_ENSINO) |>
  dplyr::summarise(across(starts_with(c("QT_", "VL_")), sum, na.rm = TRUE)) |>
  dplyr::rename_with(~ stringr::str_remove(., "ANO_")) |> 
  dplyr::mutate(
    BOLSA_APENAS_FAPESQ = case_when(
      QT_BOLSA_CAPES == 0 & QT_BOLSA_CAPES_FAPESQ > 0 & QT_BOLSA_OUTROS == 0 ~ 1,
      TRUE ~ 0
    ),
    TIPO_BOLSA_MAIS_COMUM = case_when(
      QT_BOLSA_CAPES > QT_BOLSA_CAPES_FAPESQ & QT_BOLSA_CAPES > QT_BOLSA_OUTROS ~ "CAPES",
      QT_BOLSA_CAPES_FAPESQ > QT_BOLSA_CAPES & QT_BOLSA_CAPES_FAPESQ > QT_BOLSA_OUTROS ~ "CAPES-FAPESQ",
      QT_BOLSA_OUTROS > QT_BOLSA_CAPES & QT_BOLSA_OUTROS > QT_BOLSA_CAPES_FAPESQ ~ "OUTROS",
      TRUE ~ "MÚLTIPLAS BOLSAS")
    ) |> 
  dplyr::rename(DS_GRAU_ACADEMICO_DISCENTE = DS_NIVEL,
                CD_PROGRAMA_IES = CD_PROGRAMA_PPG)


df_discentes_bolsa <- df_discentes |>
  dplyr::left_join(
    df_bolsas, 
    by = c("ID_PESSOA", "NM_DISCENTE", "CD_PROGRAMA_IES", 
           "DS_GRAU_ACADEMICO_DISCENTE", "SG_ENTIDADE_ENSINO")
    ) 

## Tese e Dissertação ----

df_discentes_bolsa_tese <- df_discentes_bolsa |> 
  dplyr::left_join(
    teses_pb |> 
      dplyr::select(
        ID_PESSOA, NM_PRODUCAO, NM_GRAU_ACADEMICO,
        CD_PROGRAMA, DT_TITULACAO),
    by = c("ID_PESSOA", "CD_PROGRAMA_IES"="CD_PROGRAMA",
           "DS_GRAU_ACADEMICO_DISCENTE" = "NM_GRAU_ACADEMICO")
    )  |> 
  dplyr::mutate(
    MESES_FORMACAO = ifelse(!is.na(DT_TITULACAO), 
                            round(difftime(DT_TITULACAO, DT_MATRICULA, units = "weeks")/4.345),
                            NA)
    ) |> 
  dplyr::mutate(
    TEM_TESE = ifelse(!is.na(NM_PRODUCAO), 1, 0)
    ) |> 
  dplyr::mutate(NM_PRODUCAO = if_else(NM_PRODUCAO == "NA", NA, NM_PRODUCAO)) 

# Casos estranhos:
# df_discentes_bolsa_tese |> group_by(ID_PESSOA, CD_PROGRAMA_IES, DS_GRAU_ACADEMICO_DISCENTE) |> 
#   tally() |> filter(n > 1) |> View()




## Publicações ----

artigos_qualis <- artigos_autor_pb |> 
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


## Base final ----

ies_uf <- read_rds("dados/tidy/ies_uf.rds") |> 
  dplyr::rename(
    CD_PROGRAMA_IES = CD_PROGRAMA,
    NM_PROGRAMA_IES = NM_PROGRAMA
    ) |> 
  dplyr::select(
    NM_PROGRAMA_IES, CD_PROGRAMA_IES
    ) |> 
  dplyr::distinct(
    CD_PROGRAMA_IES, .keep_all = TRUE
    )


base_capes <- df_discentes_bolsa_tese |>
  dplyr::left_join(artigos_qualis, by = c("NM_DISCENTE"="NM_AUTOR")) |> 
  dplyr::mutate(across(starts_with("ARTIGO"), ~ replace_na(., 0))) |> 
  dplyr::mutate(across(starts_with("TOTAL_ARTIGOS"), ~ replace_na(., 0))) |> 
  dplyr::rename(NM_DISSERTACAO_TESE = NM_PRODUCAO) |> 
  dplyr::left_join(ies_uf, by = "CD_PROGRAMA_IES") |> 
  dplyr::mutate(IDADE_DISCENTE_MATRICULA = lubridate::year(DT_MATRICULA) - AN_NASCIMENTO_DISCENTE) |> 
  dplyr::relocate(c(DT_TITULACAO, MESES_FORMACAO, NM_DISSERTACAO_TESE), .after = DT_MATRICULA) |> 
  dplyr::relocate(NM_PROGRAMA_IES, .after = CD_PROGRAMA_IES) |> 
  dplyr::relocate(IDADE_DISCENTE_MATRICULA, .after = AN_NASCIMENTO_DISCENTE) |> 
  dplyr::arrange(NM_DISCENTE, DT_MATRICULA) |> 
  dplyr::mutate(NM_DISSERTACAO_TESE = stringr::str_to_upper(
    janitor::make_clean_names(NM_DISSERTACAO_TESE, case = "sentence", allow_dupes = TRUE))) |> 
  dplyr::mutate(NM_DISSERTACAO_TESE = dplyr::na_if(NM_DISSERTACAO_TESE, "NA"))
    

## Adicionar bolsas CNPQ & FAPESQ ----
cnpq_discente <- cnpq_pb |> dplyr::group_by(NM_DISCENTE,
                                         TIPO_BOLSA,
                                         DS_GRAU_ACADEMICO_DISCENTE,
                                         SG_ENTIDADE_ENSINO) |> 
  dplyr::summarise(VL_BOLSA_ANO = sum(VL_BOLSA_ANO, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::rename(VL_BOLSA_CNPQ = VL_BOLSA_ANO) 


fapesq_discente <- editais_fapesq |>
  dplyr::mutate(TIPO_BOLSA = "FAPESQ") |> 
  dplyr::group_by(NM_DISCENTE,
                  TIPO_BOLSA,
                  DS_GRAU_ACADEMICO_DISCENTE,
                  SG_ENTIDADE_ENSINO) |>
  dplyr::summarise(VL_BOLSA_FAPESQ = QT_BOLSA_FAPESQ * VL_BOLSA_MES,
                   QT_BOLSA_FAPESQ = sum(QT_BOLSA_FAPESQ, na.rm = TRUE)) |> 
  dplyr::ungroup() 



base_capes_cnpq <- base_capes |>
  dplyr::left_join(
    cnpq_discente,
    by = c(
      "NM_DISCENTE",
      "DS_GRAU_ACADEMICO_DISCENTE",
      "SG_ENTIDADE_ENSINO"
    )
  ) |>
  # dplyr::left_join(
  #   fapesq_discente,
  #   by = c(
  #     "NM_DISCENTE",
  #     "DS_GRAU_ACADEMICO",
  #     "SG_ENTIDADE_ENSINO"
  #   )
  # ) |> 
  dplyr::relocate(VL_BOLSA_CNPQ, .after = VL_BOLSA_CAPES_FAPESQ) |>
  dplyr::mutate(VL_BOLSA_TOTAL = 0) |>
  dplyr::rowwise() |>
  dplyr::mutate(VL_BOLSA_TOTAL = sum(c_across(starts_with('VL_BOLSA')), na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  dplyr::mutate(across(starts_with('VL_BOLSA'), ~ ifelse(is.na(.), 0, .))) |>
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = case_when(
      VL_BOLSA_CAPES > VL_BOLSA_CAPES_FAPESQ & VL_BOLSA_CAPES > VL_BOLSA_CNPQ &
        VL_BOLSA_CAPES > VL_BOLSA_OUTROS ~ "CAPES",
      VL_BOLSA_CAPES_FAPESQ > VL_BOLSA_CAPES &
        VL_BOLSA_CAPES_FAPESQ > VL_BOLSA_CNPQ &
        VL_BOLSA_CAPES_FAPESQ > VL_BOLSA_OUTROS ~ "CAPES - FAPESQ",
      VL_BOLSA_OUTROS > VL_BOLSA_CAPES &
        VL_BOLSA_OUTROS > VL_BOLSA_CNPQ &
        VL_BOLSA_OUTROS > VL_BOLSA_CAPES_FAPESQ ~ "OUTROS",
      VL_BOLSA_CNPQ > VL_BOLSA_CAPES &
        VL_BOLSA_CNPQ > VL_BOLSA_CAPES_FAPESQ &
        VL_BOLSA_CNPQ > VL_BOLSA_OUTROS ~ "CNPQ",
      TRUE ~ "MÚLTIPLAS BOLSAS"
    )
  ) |>
  dplyr::mutate(TIPO_BOLSA_MAIS_COMUM = ifelse(VL_BOLSA_TOTAL == 0, "SEM BOLSA", TIPO_BOLSA_MAIS_COMUM)) |>
  dplyr::filter(TIPO_BOLSA_MAIS_COMUM != "MÚLTIPLAS BOLSAS") |>
  dplyr::filter(VL_BOLSA_TOTAL < 150000) |>
  dplyr::select(-TIPO_BOLSA)



df_pb <- geobr::read_municipality(code_muni = "PB", year = 2010) |> dplyr::select(code_muni, name_muni) |>
  dplyr::mutate(name_muni = stringr::str_to_upper(
    janitor::make_clean_names(name_muni, case = "sentence", allow_dupes = TRUE))) |>
  dplyr::rename(NM_MUNICIPIO_PROGRAMA_IES = name_muni,
                CD_MUNICIPIO_PROGRAMA_IES = code_muni)


df <- base_capes_cnpq |> 
  dplyr::mutate(NM_MUNICIPIO_PROGRAMA_IES = stringr::str_to_upper(
    janitor::make_clean_names(NM_MUNICIPIO_PROGRAMA_IES, case = "sentence", allow_dupes = TRUE))) |> 
  dplyr::left_join(df_pb, by = "NM_MUNICIPIO_PROGRAMA_IES")


df_avaliacao <- discentes_pb |> 
  dplyr::distinct(CD_PROGRAMA_IES, CD_CONCEITO_PROGRAMA) |> 
  dplyr::mutate(CD_CONCEITO_PROGRAMA = ifelse(
    CD_CONCEITO_PROGRAMA == "A", NA, CD_CONCEITO_PROGRAMA
  )) |> 
  distinct(CD_PROGRAMA_IES, .keep_all = TRUE) 

df <- df |> 
  dplyr::left_join(df_avaliacao, by = "CD_PROGRAMA_IES") |> 
  dplyr::select(-geom)

df <- df |> sf::st_drop_geometry() |> as.data.frame()

df |> write_rds("dados/tidy/discentes_bolsa_tese_pub.rds")







editais_fapesq |> group_by(EDITAL) |> summarise(bolsa = mean(VL_BOLSA_MES, na.rm = TRUE)) 
