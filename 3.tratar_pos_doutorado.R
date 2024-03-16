# PACOTES ----
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, janitor, readr, tidyr, genderBR, geobr)


cnpq <-  read_rds("dados/tidy/bolsas_cnpq_pb.rds") |> 
  dplyr::filter(DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO")

capes <- read_rds("dados/tidy/bolsas_pb.rds") |> 
  dplyr::mutate(DS_NIVEL = case_match(
    DS_NIVEL,
    "MESTRADO" ~ "MESTRADO",
    "DOUTORADO" ~ "DOUTORADO",
    "ESTÁGIO PÓS-DOUTORAL" ~ "POS-DOUTORADO",
    .default = "OUTROS")
    ) |> 
  dplyr::filter(DS_NIVEL == "POS-DOUTORADO")

fapesq <- read_rds("dados/tidy/editais_fapesq.rds") |> 
  dplyr::filter(DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO")




