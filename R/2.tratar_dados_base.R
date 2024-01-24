#### Packages -----------
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, janitor, readr, tidyr, genderBR)

# DADOS ------

discentes_pb <- readr::read_rds("dados/tidy/discentes_pb.rds")
teses_pb <- readr::read_rds("dados/tidy/teses_dissertacoes_pb.rds")
bolsas_pb <- readr::read_rds("dados/tidy/bolsas_pb.rds") 
artigos_autor_pb <- readr::read_rds("dados/tidy/artigos_autor_pb.rds")

###### Informações básicas do Discente -------

# Funcao para calcular a moda
get_mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


##### Criar dimensao 'discente' -----
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
  dplyr::left_join(
    discentes_pb |> 
      dplyr::filter(
        dplyr::between(ANO, 2017, 2020) # FAPESQ só começou a ser informado em 2017
        ) |> 
      dplyr::select(
        NM_DISCENTE, ID_PESSOA, ANO, 
        DT_MATRICULA_DISCENTE,
        NM_SITUACAO_DISCENTE, DT_SITUACAO_DISCENTE,
        DS_GRAU_ACADEMICO_DISCENTE,
        SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_PROGRAMA_IES, 
        CD_AREA_AVALIACAO, NM_AREA_AVALIACAO),
    by = c("NM_DISCENTE", "ID_PESSOA")
    ) |> 
  dplyr::group_by(
    ID_PESSOA, NM_DISCENTE, AN_NASCIMENTO_DISCENTE, NR_DOCUMENTO_DISCENTE,
    GENERO, DS_GRAU_ACADEMICO_DISCENTE, CD_PROGRAMA_IES,
    SG_ENTIDADE_ENSINO, NM_AREA_AVALIACAO
    ) |> 
  dplyr::summarise(
    DT_MATRICULA = dplyr::first(DT_MATRICULA_DISCENTE),
    NM_SITUACAO_DISCENTE = NM_SITUACAO_DISCENTE[which.max(DT_SITUACAO_DISCENTE)]
    ) |> 
  dplyr::ungroup() |> 
  dplyr::distinct()

###### Informações de Bolsa --------------
# FIXME: Base de bolsas: Discentes como LORENA MARIA AUGUSTO PEQUENO aparecem repetidas vezes no mesmo ano.
# O PPG pode renovar a bolsa (da mesma agência) dentro de um mesmo ano. Assim, 
# As diversas ocorrências serão somadas.
df_bolsas <- bolsas_pb |>
  dplyr::filter(ANO >= 2017,
                DS_NIVEL %in% c("DOUTORADO", "MESTRADO")) |>
  dplyr::group_by(ID_PESSOA, ANO, DS_NIVEL, CD_PROGRAMA_PPG) |>
  dplyr::mutate(QT_BOLSA_ANO = sum(QT_BOLSA_ANO),
                VL_BOLSA_ANO = sum(VL_BOLSA_ANO)) |>
  dplyr::ungroup() |>
  distinct(ANO, NM_DISCENTE, DS_NIVEL,
           .keep_all = T) |>
dplyr::group_by(NM_DISCENTE, ID_PESSOA, DS_NIVEL, CD_PROGRAMA_PPG) |>
  dplyr::reframe(
    QT_BOLSA_TOTAL = sum(QT_BOLSA_ANO),
    VL_BOLSA_TOTAL = sum(VL_BOLSA_ANO),
    QT_BOLSA_FAPESQ = ifelse(SG_PROGRAMA_CAPES == "FAPESQ", sum(QT_BOLSA_ANO), 0),
    VL_BOLSA_FAPESQ = ifelse(SG_PROGRAMA_CAPES == "FAPESQ", sum(VL_BOLSA_ANO), 0),
    QT_BOLSA_NAO_FAPESQ = ifelse(SG_PROGRAMA_CAPES != "FAPESQ", sum(QT_BOLSA_ANO), 0),
    VL_BOLSA_NAO_FAPESQ = ifelse(SG_PROGRAMA_CAPES != "FAPESQ", sum(VL_BOLSA_ANO), 0)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    BOLSA_APENAS_FAPESQ = if_else(QT_BOLSA_FAPESQ > 0 &
                                    QT_BOLSA_NAO_FAPESQ == 0, 1, 0),
    BOLSA_MUDOU = if_else(QT_BOLSA_FAPESQ > 0 &
                            QT_BOLSA_NAO_FAPESQ > 0, 1, 0)
  ) |>
  dplyr::rename(DS_GRAU_ACADEMICO_DISCENTE = DS_NIVEL,
                CD_PROGRAMA_IES = CD_PROGRAMA_PPG) |>
  dplyr::distinct()


df_discentes_bolsa <- df_discentes |>
  dplyr::mutate(
    DS_GRAU_ACADEMICO_DISCENTE = case_match(
      DS_GRAU_ACADEMICO_DISCENTE,
      "DOUTORADO" ~ "DOUTORADO",
      "MESTRADO" ~ "MESTRADO",
      "MESTRADO PROFISSIONAL" ~ "MESTRADO")
    ) |> 
  dplyr::left_join(
    df_bolsas, 
    by = c("ID_PESSOA", "NM_DISCENTE",
           "CD_PROGRAMA_IES", "DS_GRAU_ACADEMICO_DISCENTE")
    ) 



###### Tese e Dissertação -------------

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
    ) 


###### Informações de Publicação --------------

# TODO: Publicacoes de 2017-2020 possuem o mesmo ANO == 2017. A não ser que se recupere o ano com DOI.
# O ano de publicação está em PUBLICACAO_DETALHES:
# https://dadosabertos.capes.gov.br/dataset/detalhes-da-producao-intelectual-artistica-2013a2016
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-detalhes-da-producao-intelectual-bibliografica-de-programas-de-pos-graduacao

artigos_qualis <- artigos_autor_pb |> 
  dplyr::filter(
    !is.na(SG_ESTRATO), SG_ESTRATO != "NI"
    ) |>
  dplyr::group_by(
    ID_PESSOA, SG_ESTRATO
    ) |>
  dplyr::summarise(
    ARTIGOS = n()
    ) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(
    id_cols = ID_PESSOA, 
    names_from = SG_ESTRATO, 
    values_from = ARTIGOS, 
    names_prefix = "ARTIGO_", values_fill = 0
    ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    TOTAL_ARTIGOS = sum(c_across(starts_with('ARTIGO_')))
    ) 


###### Unir bases de discentes, bolsistas, teses e publicações --------------
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
  dplyr::left_join(artigos_qualis, by = "ID_PESSOA") |> 
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
    janitor::make_clean_names(NM_DISSERTACAO_TESE, case = "sentence", allow_dupes = TRUE))) 



base_capes |> write_rds("dados/tidy/discentes_bolsa_tese_pub.rds")


# base_capes |> 
#   filter(ID_PESSOA == 1241687) |> View()
# 
# base_capes |> 
#   filter(ID_PESSOA == 2790152) |> View()
