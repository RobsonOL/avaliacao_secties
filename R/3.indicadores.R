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
  
out <- bolsista_fapesq |> 
  dplyr::group_by(
    DS_GRAU_ACADEMICO_DISCENTE
  ) |> 
  dplyr::summarise(
    TX_CONCLUSAO_FAPESQ = sum(TEM_TESE)/sum(BOLSA_APENAS_FAPESQ) * 100
  )


  
