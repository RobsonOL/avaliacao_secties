#### Packages -----------
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, janitor)

# DADOS ------
discentes_bolsa_tese_pub <- readr::read_rds("dados/tidy/discentes_bolsa_tese_pub.rds")


# Bolsistas Fapesq -----

bolsista_fapesq <- discentes_bolsa_tese_pub |> 
  dplyr::filter(
    BOLSA_APENAS_FAPESQ == 1
  )

# Numero de bolsistas
n_bolsistas <- bolsista_fapesq |> 
  dplyr::group_by(
    DS_GRAU_ACADEMICO_DISCENTE
  ) |>
  dplyr::summarise(
    n_bolsistas = n()
  )

# Numero de bolsistas por genero
n_bolsistas_genero <- bolsista_fapesq |> 
  dplyr::group_by(
    DS_GRAU_ACADEMICO_DISCENTE, GENERO
  ) |>
  dplyr::summarise(
    n_bolsistas_genero = n()
  ) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(
    DS_GRAU_ACADEMICO_DISCENTE
  ) |> 
  dplyr::mutate(
    freq = n_bolsistas_genero/sum(n_bolsistas_genero) * 100
  )
  
# Taxa de titulacao
tx_conclusao <- bolsista_fapesq |> 
  dplyr::group_by(
    DS_GRAU_ACADEMICO_DISCENTE
  ) |> 
  dplyr::summarise(
    TX_CONCLUSAO_FAPESQ = sum(TEM_TESE)/sum(BOLSA_APENAS_FAPESQ) * 100
  )

# Numero medio de bolsas
n_medio_bolsas <- bolsista_fapesq |> 
  dplyr::group_by(
    DS_GRAU_ACADEMICO_DISCENTE
  ) |>
  dplyr::summarise(
    n_medio_bolsas = mean(QT_BOLSA_TOTAL)
  )

# Valor medio de bolsas
valor_medio_bolsas <- bolsista_fapesq |> 
  dplyr::group_by(
    DS_GRAU_ACADEMICO_DISCENTE
  ) |>
  dplyr::summarise(
    valor_medio_bolsas = mean(VL_BOLSA_TOTAL)
  )

 

