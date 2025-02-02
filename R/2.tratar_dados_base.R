# PACOTES ----
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, janitor, readr, tidyr, genderBR, geobr, RSQLite)

# DADOS ----

teses_pb <- readr::read_rds("dados/tidy/teses_dissertacoes_pb.rds")
bolsas_pb <- readr::read_rds("dados/tidy/bolsas_pb.rds") 
artigos_autor_pb <- readr::read_rds("dados/tidy/artigos_autor_pb.rds")
cnpq_pb <- readr::read_rds("dados/tidy/bolsas_cnpq_pb.rds")
editais_fapesq <- readr::read_rds("dados/tidy/editais_fapesq.rds")

## Dimensão Avaliação de Curso ----
df_avaliacao <- readr::read_rds("dados/tidy/discentes_pb.rds") |> 
  dplyr::distinct(CD_PROGRAMA_IES, CD_CONCEITO_PROGRAMA) |> 
  dplyr::mutate(CD_CONCEITO_PROGRAMA = ifelse(
    CD_CONCEITO_PROGRAMA == "A", NA, CD_CONCEITO_PROGRAMA
  )) |> 
  distinct(CD_PROGRAMA_IES, .keep_all = TRUE) 



## Dimensão Discente ----

# Funcao para calcular a moda
get_mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

dim_discentes <- readr::read_rds("dados/tidy/discentes_pb.rds") |> 
  dplyr::filter(
    dplyr::between(ANO, 2017, 2023)
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


dim_discentes <- dim_discentes |> 
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
    readr::read_rds("dados/tidy/discentes_pb.rds") |> 
      dplyr::filter(
        dplyr::between(ANO, 2017, 2023) 
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
    ) |> 
  dplyr::mutate(IDADE_DISCENTE_MATRICULA = lubridate::year(DT_MATRICULA) - AN_NASCIMENTO_DISCENTE)

# con <- dbConnect(SQLite(), dbname = 'fapesqdata.db')
# dbExecute(con, "
#   CREATE TABLE dim_discentes (
#     ID_PESSOA INTEGER,
#     NM_DISCENTE TEXT,
#     AN_NASCIMENTO_DISCENTE INTEGER,
#     NR_DOCUMENTO_DISCENTE TEXT,
#     GENERO TEXT,
#     DS_GRAU_ACADEMICO_DISCENTE TEXT,
#     CD_PROGRAMA_IES TEXT,
#     NM_MUNICIPIO_PROGRAMA_IES TEXT,
#     SG_ENTIDADE_ENSINO TEXT,
#     NM_AREA_AVALIACAO TEXT,
#     NM_PAIS_NACIONALIDADE_DISCENTE TEXT,
#     DT_MATRICULA TIMESTAMP,
#     NM_SITUACAO_DISCENTE TEXT,
#     IDADE_DISCENTE_MATRICULA TEXT,
#     PRIMARY KEY (ID_PESSOA, CD_PROGRAMA_IES, DT_MATRICULA)
#   )
# ")
# # dbExecute(con, "DROP TABLE IF EXISTS dim_discentes")
# dbWriteTable(con, "dim_discentes", dim_discentes, append = TRUE, row.names = FALSE)
# dbDisconnect(con)

## Tratar Bolsas CAPES ----

dim_bolsas <- bolsas_pb |>
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
  # Solução 1: Chamar todas as bolsas de CAPES. Isso facilita na hora de calcular a data inicial da bolsa
  # Solução 2: Chamar as bolsas de CAPES, CAPES_FAPESQ e CAPES - OUTRAS. 
  # Mas se torna complexo definir o início e o fim de cada bolsa, uma vez que ela pode ser alterada no meio do ano.
  # dplyr::mutate(
  #   SG_PROGRAMA_CAPES = dplyr::case_match(
  #     SG_PROGRAMA_CAPES,
  #     "DS" ~ "CAPES",
  #     "FAPESQ" ~ "CAPES_FAPESQ",
  #     .default = "CAPES_OUTROS"
  #   )
  # ) |>
  dplyr::mutate(
    SG_PROGRAMA_CAPES = "CAPES"
  ) |>
  # O primeiro group_by é para calcular a quantidade e o valor total de bolsas por aluno.
  # Algumas vezes o mesmo aluno está registrado em duas linhas (5 bolsas e 7 bolsas = 12 bolsas anuais).
  dplyr::group_by(ID_PESSOA, ANO, DS_NIVEL, CD_PROGRAMA_PPG, SG_ENTIDADE_ENSINO, SG_PROGRAMA_CAPES) |>
  dplyr::mutate(
    QT_BOLSA_ANO = sum(QT_BOLSA_ANO),
    VL_BOLSA_ANO = sum(VL_BOLSA_ANO)
    ) |> 
  dplyr::distinct(.keep_all = TRUE) |> 
  # Esse group_by só é necessário caso a solução 2 seja adotada.
  # dplyr::group_by(NM_DISCENTE,
  #                 ID_PESSOA,
  #                 ANO,
  #                 DS_NIVEL,
  #                 CD_PROGRAMA_PPG,
  #                 SG_ENTIDADE_ENSINO) |>
  # tidyr::pivot_wider(names_from = SG_PROGRAMA_CAPES,
  #                    values_from = c(QT_BOLSA_ANO, VL_BOLSA_ANO)) |> 
  # dplyr::mutate(
  #   QT_BOLSA_TOTAL = sum(
  #     QT_BOLSA_ANO_CAPES,
  #     QT_BOLSA_ANO_CAPES_FAPESQ,
  #     QT_BOLSA_ANO_CAPES_OUTROS,
  #     na.rm = TRUE
  #   ),
  #   VL_BOLSA_TOTAL = sum(
  #     VL_BOLSA_ANO_CAPES,
  #     VL_BOLSA_ANO_CAPES_FAPESQ,
  #     VL_BOLSA_ANO_CAPES_OUTROS,
  #     na.rm = TRUE
  #   )
  # ) |>
  # dplyr::ungroup() |> 
  # Esse group by é necessário para calcular a quantidade de bolsas por aluno.
  # E definir o início da bolsa.
  dplyr::group_by(NM_DISCENTE, ID_PESSOA, DS_NIVEL, CD_PROGRAMA_PPG, SG_ENTIDADE_ENSINO) |>
  dplyr::summarise(across(starts_with(c("QT_", "VL_")), \(x) sum(x, na.rm = TRUE)),
                   INICIO_BOLSA_CAPES = min(ANO)) |>
  dplyr::ungroup() |>
  dplyr::rename_with(~ stringr::str_remove(., "ANO_")) |> 
  # Para definir o tipo de bolsa mais comum. Só é necessária se a solução 2 for adotada.
  # dplyr::mutate(
  #   BOLSA_APENAS_FAPESQ = dplyr::case_when(
  #     QT_BOLSA_CAPES == 0 & 
  #       QT_BOLSA_CAPES_FAPESQ > 0 & 
  #       QT_BOLSA_OUTROS == 0 ~ 1,
  #     TRUE ~ 0
  #   ),
  #   TIPO_BOLSA_MAIS_COMUM = dplyr::case_when(
  #     QT_BOLSA_CAPES > QT_BOLSA_CAPES_FAPESQ & QT_BOLSA_CAPES > QT_BOLSA_OUTROS ~ "CAPES",
  #     QT_BOLSA_CAPES_FAPESQ > QT_BOLSA_CAPES & QT_BOLSA_CAPES_FAPESQ > QT_BOLSA_OUTROS ~ "CAPES - FAPESQ",
  #     QT_BOLSA_OUTROS > QT_BOLSA_CAPES & QT_BOLSA_OUTROS > QT_BOLSA_CAPES_FAPESQ ~ "CAPES - OUTROS",
  #     TRUE ~ "MÚLTIPLAS BOLSAS")
  #   ) |> 
  dplyr::rename(DS_GRAU_ACADEMICO_DISCENTE = DS_NIVEL,
                CD_PROGRAMA_IES = CD_PROGRAMA_PPG,
                VL_BOLSA_CAPES_ANO = VL_BOLSA_ANO,
                QT_BOLSA_CAPES_ANO = QT_BOLSA_ANO)


df_discentes_bolsa <- dim_discentes |>
  dplyr::left_join(
    dim_bolsas, 
    by = c("ID_PESSOA", "NM_DISCENTE", "CD_PROGRAMA_IES", 
           "DS_GRAU_ACADEMICO_DISCENTE", "SG_ENTIDADE_ENSINO")
    ) |> 
  dplyr::mutate(across(starts_with("QT_"), ~ replace_na(., 0)),
                across(starts_with("VL_"), ~ replace_na(., 0)))

## Dimensão de Tese e Dissertação ----

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
  dplyr::mutate(NM_PRODUCAO = if_else(NM_PRODUCAO == "NA", NA, NM_PRODUCAO)) |> 
  dplyr::rename(NM_DISSERTACAO_TESE = NM_PRODUCAO) |> 
  dplyr::mutate(NM_DISSERTACAO_TESE = stringr::str_to_upper(
    janitor::make_clean_names(NM_DISSERTACAO_TESE, case = "sentence", allow_dupes = TRUE))) |> 
  dplyr::mutate(NM_DISSERTACAO_TESE = dplyr::na_if(NM_DISSERTACAO_TESE, "NA"))


# Casos estranhos:
# df_discentes_bolsa_tese |> count(ID_PESSOA, CD_PROGRAMA_IES, DS_GRAU_ACADEMICO_DISCENTE) |> filter(n > 1) |> View()




## Publicações ----

# Explicação: As publicações não virão mais da base da capes, mas sim do lattes

# artigos_qualis <- artigos_autor_pb |> 
#   dplyr::filter(
#     !is.na(SG_ESTRATO), SG_ESTRATO != "NI"
#     ) |>
#   dplyr::group_by(
#     NM_AUTOR, SG_ESTRATO
#     ) |>
#   dplyr::summarise(
#     ARTIGOS = n()
#     ) |> 
#   dplyr::ungroup() |> 
#   tidyr::pivot_wider(
#     id_cols = NM_AUTOR, 
#     names_from = SG_ESTRATO, 
#     values_from = ARTIGOS, 
#     names_prefix = "ARTIGO_", values_fill = 0
#     ) |> 
#   dplyr::rowwise() |> 
#   dplyr::mutate(
#     TOTAL_ARTIGOS = sum(c_across(starts_with('ARTIGO_')), na.rm = TRUE)
#     ) 


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
  # Explicação: publicações virão da base de lattes
  # dplyr::left_join(artigos_qualis, by = c("NM_DISCENTE"="NM_AUTOR")) |> 
  # dplyr::mutate(across(starts_with("ARTIGO"), ~ replace_na(., 0))) |> 
  # dplyr::mutate(across(starts_with("TOTAL_ARTIGOS"), ~ replace_na(., 0))) |> 
  dplyr::left_join(ies_uf, by = "CD_PROGRAMA_IES") |> 
  dplyr::relocate(c(DT_TITULACAO, MESES_FORMACAO, NM_DISSERTACAO_TESE), .after = DT_MATRICULA) |> 
  dplyr::relocate(NM_PROGRAMA_IES, .after = CD_PROGRAMA_IES) |> 
  dplyr::relocate(IDADE_DISCENTE_MATRICULA, .after = AN_NASCIMENTO_DISCENTE) |> 
  dplyr::arrange(NM_DISCENTE, DT_MATRICULA)
    

## Adicionar bolsas CNPQ ----
cnpq_discente <- cnpq_pb |> dplyr::group_by(NM_DISCENTE,
                                         TIPO_BOLSA,
                                         DS_GRAU_ACADEMICO_DISCENTE,
                                         SG_ENTIDADE_ENSINO) |> 
  dplyr::summarise(VL_BOLSA_ANO = sum(VL_BOLSA_ANO, na.rm = TRUE),
                   INICIO_BOLSA_CNPQ = min(ANO)) |> 
  dplyr::ungroup() |> 
  dplyr::rename(VL_BOLSA_CNPQ_ANO = VL_BOLSA_ANO) 


base_capes_cnpq <- base_capes |>
  dplyr::left_join(
    cnpq_discente,
    by = c(
      "NM_DISCENTE",
      "DS_GRAU_ACADEMICO_DISCENTE",
      "SG_ENTIDADE_ENSINO"
    )
  ) |> 
  dplyr::mutate(across(
    starts_with("VL_BOLSA"),
    ~ replace_na(., 0)
  )) |> 
  dplyr::mutate(NM_MUNICIPIO_PROGRAMA_IES = stringr::str_to_upper(
    janitor::make_clean_names(
      NM_MUNICIPIO_PROGRAMA_IES,
      case = "sentence",
      allow_dupes = TRUE
    )
  )) |>
  dplyr::left_join(df_avaliacao, by = "CD_PROGRAMA_IES")


## Adicionar bolsas FAPESQ ----

# editais_fapesq <- editais_fapesq |> 
#   mutate(INICIO_BOLSA = as.Date(INICIO_BOLSA), FIM_BOLSA = as.Date(FIM_BOLSA)) |> 
#   mutate(INICIO_BOLSA = lubridate::year(INICIO_BOLSA), FIM_BOLSA = lubridate::year(FIM_BOLSA)) |> 
#   filter(!is.na(INICIO_BOLSA) & !is.na(FIM_BOLSA)) |>
#   transmute(NM_DISCENTE, DS_GRAU_ACADEMICO_DISCENTE, ANO = map2(INICIO_BOLSA, FIM_BOLSA, seq)) |>
#   unnest(cols = ANO) |> 
#   distinct()

fapesq_discente <- editais_fapesq |>
  dplyr::filter(EDITAL != "Edital 03/2016") |> 
  dplyr::filter(DS_GRAU_ACADEMICO_DISCENTE %in% c("MESTRADO", "DOUTORADO")) |> 
  dplyr::group_by(NM_DISCENTE,
                  DS_GRAU_ACADEMICO_DISCENTE,
                  EDITAL,
                  NR_DOCUMENTO_DISCENTE,
                  SG_ENTIDADE_ENSINO) |>
  dplyr::summarise(VL_BOLSA_FAPESQ_ANO = QT_BOLSA_FAPESQ * VL_BOLSA_MES,
                   QT_BOLSA_FAPESQ_ANO = sum(QT_BOLSA_FAPESQ, na.rm = TRUE),
                   INICIO_BOLSA_FAPESQ = lubridate::year(INICIO_BOLSA),
                   FIM_BOLSA_FAPESQ = lubridate::year(FIM_BOLSA)) |> 
  dplyr::ungroup() 

df_fapesq <- base_capes_cnpq |> 
  mutate(id = row_number()) |> 
  left_join(fapesq_discente, ## |> dplyr::filter(!is.na(NR_DOCUMENTO_DISCENTE)), 
            by = c(
              "NM_DISCENTE", 
              #"CPF_DIGITOS",
              "DS_GRAU_ACADEMICO_DISCENTE", 
              "SG_ENTIDADE_ENSINO"
              )
            ) |> 
  dplyr::mutate(VL_BOLSA_TOTAL_ANO = 0) |>
  dplyr::rowwise() |>
  dplyr::mutate(VL_BOLSA_TOTAL = sum(dplyr::c_across(dplyr::starts_with('VL_BOLSA')), na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  dplyr::mutate(dplyr::across(dplyr::starts_with('VL_BOLSA'), ~ ifelse(is.na(.), 0, .))) |>
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = dplyr::case_when(
      VL_BOLSA_CAPES_ANO > VL_BOLSA_FAPESQ_ANO & 
        VL_BOLSA_CAPES_ANO > VL_BOLSA_CNPQ_ANO ~ "CAPES",
      VL_BOLSA_CNPQ_ANO > VL_BOLSA_CAPES_ANO &
        VL_BOLSA_CNPQ_ANO > VL_BOLSA_FAPESQ_ANO ~ "CNPQ",
      VL_BOLSA_FAPESQ_ANO > VL_BOLSA_CAPES_ANO &
        VL_BOLSA_FAPESQ_ANO > VL_BOLSA_CAPES_ANO &
        VL_BOLSA_FAPESQ_ANO > VL_BOLSA_CNPQ_ANO ~ "FAPESQ",
      TRUE ~ "MÚLTIPLAS BOLSAS"
    )
  ) |>
  dplyr::mutate(TIPO_BOLSA_MAIS_COMUM = ifelse(VL_BOLSA_TOTAL == 0, "SEM INFO", TIPO_BOLSA_MAIS_COMUM)) |>
  dplyr::select(-NR_DOCUMENTO_DISCENTE.y) |> 
  dplyr::rename(NR_DOCUMENTO_DISCENTE = NR_DOCUMENTO_DISCENTE.x)

df_fapesq <- df_fapesq |> 
  # Estudante sem merge: possuem informações na fapesq mas sucupira ainda não informou
  dplyr::bind_rows(
    fapesq_discente |> 
      dplyr::filter(!NM_DISCENTE %in% df_fapesq$NM_DISCENTE) |> 
      dplyr::mutate(TIPO_BOLSA_MAIS_COMUM = "FAPESQ")
  ) |> 
  dplyr::select(ID_PESSOA, NM_DISCENTE, AN_NASCIMENTO_DISCENTE, IDADE_DISCENTE_MATRICULA,
                NR_DOCUMENTO_DISCENTE, GENERO, DS_GRAU_ACADEMICO_DISCENTE,
                CD_PROGRAMA_IES, NM_PROGRAMA_IES, NM_MUNICIPIO_PROGRAMA_IES,
                CD_CONCEITO_PROGRAMA, SG_ENTIDADE_ENSINO, NM_AREA_AVALIACAO, 
                NM_PAIS_NACIONALIDADE_DISCENTE, DT_MATRICULA, DT_TITULACAO, MESES_FORMACAO, 
                NM_DISSERTACAO_TESE, NM_SITUACAO_DISCENTE, 
                TIPO_BOLSA_MAIS_COMUM,
                VL_BOLSA_CAPES_ANO, VL_BOLSA_CNPQ_ANO, VL_BOLSA_FAPESQ_ANO,
                QT_BOLSA_CAPES_ANO, QT_BOLSA_FAPESQ_ANO,
                INICIO_BOLSA_CAPES, INICIO_BOLSA_CNPQ, INICIO_BOLSA_FAPESQ
                )
  


df_fapesq |> write_rds("dados/tidy/discentes_bolsa_tese_pub.rds")
df_fapesq |> write_csv2("dados/tidy/discentes_bolsa_tese_pub.csv")
