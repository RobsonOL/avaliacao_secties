#### Packages -----------
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, janitor, readr, tidyr)

# DADOS ------

discentes_pb <- read_rds("dados/tidy/discentes_pb.rds")
teses_pb <- read_rds("dados/tidy/teses_dissertacoes_pb.rds")
bolsas_pb <- read_rds("dados/tidy/bolsas_pb.rds")
artigos_autor_pb <- read_rds("dados/tidy/artigos_autor_pb.rds")

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
  dplyr::mutate(
    NM_DISCENTE = stringr::str_to_upper(
      janitor::make_clean_names(NM_DISCENTE, case = "sentence", allow_dupes = TRUE)
    )
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

dim_discentes |> write_rds("dados/tidy/dim_discentes.rds")

df_discentes <- dim_discentes |> 
  # O mesmo ID_PESSOA ainda gera variações do mesmo nome. Ex: ID_PESSOA == 51331
  dplyr::distinct(ID_PESSOA, .keep_all = TRUE) |> 
  tidyr::drop_na(ID_PESSOA) |> 
  # A partir de 2017, quando as informações sobre bolsa FAPESQ passaram a estar disponíveis
  tidyr::crossing(ANO = seq(2017, 2020)) |>
  dplyr::relocate(ANO, .before = NM_DISCENTE) |> 
  dplyr::mutate(IDADE_DISCENTE = ANO - AN_NASCIMENTO_DISCENTE) |> 
  dplyr::left_join(discentes_pb |> 
                     dplyr::select(NM_DISCENTE, ID_PESSOA, ANO, 
                            DT_MATRICULA_DISCENTE,
                            NM_SITUACAO_DISCENTE, DT_SITUACAO_DISCENTE,
                            DS_GRAU_ACADEMICO_DISCENTE,
                            SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_PROGRAMA_IES, 
                            CD_AREA_AVALIACAO, NM_AREA_AVALIACAO),
                   by = c("ANO", "NM_DISCENTE", "ID_PESSOA"))


  
###### Informações de Bolsa --------------
# FIXME: Base de bolsas: Discentes como LORENA MARIA AUGUSTO PEQUENO aparecem repetidas vezes no mesmo ano.
# A única diferença é o total de bolsas recebidas no ano. Assim, vou considerar a maior quantidade de bolsas recebidas no ano.
# E remover as demais. Me parece que a base é alimentada diversas vezes no mesmo ano.
bolsas_pb <- bolsas_pb |> 
  dplyr::filter(ANO >= 2017) |> 
  group_by(ID_PESSOA, ANO) |> 
  filter(QT_BOLSA_ANO == max(QT_BOLSA_ANO)) |> 
  slice(1)  |> 
  ungroup() 
  

df_discentes_bolsa <- df_discentes |>
    dplyr::left_join(bolsas_pb |>
                     dplyr::select(ID_PESSOA, SG_PROGRAMA_CAPES, ANO,
                                   QT_BOLSA_ANO, VL_BOLSA_ANO, DS_NIVEL) |> 
                       filter(DS_NIVEL %in% c("DOUTORADO", "MESTRADO")),
                   by = c("ID_PESSOA", "ANO")) |> 
  # Grande maioria dos que receberam bolsa estavam em situação ativo ou de desligamento.
  # Cerca de 200 pessoas não tinham alguma situação (ABANDONO, DESLIGAMENTO, MATRICULA ATIVA OU TITULADO), 
  # mas aparecem recebendo bolsa. COmo são uma minoria, vou considerar como ativo.
  dplyr::mutate(CURSOU_PPG_ANO = ifelse(!is.na(NM_SITUACAO_DISCENTE), 1, 0)) |>
  dplyr::mutate(RECEBEU_BOLSA_ANO = ifelse(!is.na(QT_BOLSA_ANO), 1, 0)) |>
  # dplyr::mutate(CURSOU_PPG_ANO = ifelse(RECEBEU_BOLSA_ANO == 1, 1, CURSOU_PPG_ANO)) |>
  dplyr::mutate(TIPO_BOLSA = case_when(
                    CURSOU_PPG_ANO == 1 & RECEBEU_BOLSA_ANO == 1 & SG_PROGRAMA_CAPES == "FAPESQ" ~ "BOLSISTA FAPESQ",
                    CURSOU_PPG_ANO == 1 & RECEBEU_BOLSA_ANO == 1 & SG_PROGRAMA_CAPES != "FAPESQ" ~ "BOLSISTA NÃO-FAPESQ",
                    CURSOU_PPG_ANO == 1 & RECEBEU_BOLSA_ANO == 0 ~ "NÃO-BOLSISTA",
                    CURSOU_PPG_ANO == 0 ~ "NÃO CURSOU PPG NO ANO"
                  )) 


###### Tese e Dissertação -------------
df_discentes_bolsa |> filter(ID_PESSOA == 96038) |> View()

df_discentes_bolsa_tese <- df_discentes_bolsa |> 
  dplyr::left_join(
    teses_pb |> 
      dplyr::select(ID_PESSOA, ANO, NM_PRODUCAO, CD_PROGRAMA, NM_PROGRAMA, DT_TITULACAO),
    by = c("ID_PESSOA", "ANO", "CD_PROGRAMA_IES"="CD_PROGRAMA"))  |> 
  dplyr::mutate(MESES_FORMACAO = ifelse(!is.na(DT_TITULACAO), 
                                        round(difftime(DT_TITULACAO, DT_MATRICULA_DISCENTE, units = "weeks")/4.345), 
                                        NA)) |> 
  dplyr::mutate(TEM_TESE = ifelse(!is.na(NM_PRODUCAO), 1, 0)) 


###### Informações de Publicação --------------

# TODO: Publicacoes de 2017-2020 possuem o mesmo ANO == 2017. A não ser que se recupere o ano com DOI.
# O ano de publicação está em PUBLICACAO_DETALHES:
# https://dadosabertos.capes.gov.br/dataset/detalhes-da-producao-intelectual-artistica-2013a2016
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-detalhes-da-producao-intelectual-bibliografica-de-programas-de-pos-graduacao
artigos_autor |> glimpse()



