#### Packages -----------
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, janitor)

# DADOS ------

discentes_bolsa_tese_pub <- readr::read_rds("dados/tidy/discentes_bolsa_tese_pub.rds")
