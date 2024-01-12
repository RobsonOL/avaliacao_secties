#### Packages -----------
rm(list = ls())

library(tidyverse)
library(vroom)
library(lubridate)

#### DADOS -------------
discentes_pb <- read_rds("dados/tidy/discentes_pb.rds")
teses_pb <- read_rds("dados/tidy/teses_dissertacoes_pb.rds")
bolsas_pb <- read_rds("dados/tidy/bolsas_pb.rds")

#### DISCENTES PARAÍBA

# OBS:
# 1. DISCENTES: Não existe ID_PESSOA antes de 2013
# 2. DISCENTES: Não existe CPF antes de 2013
# 3. DISCENTES e BOLSAS: sobra NM_DISCENTE e NM_BOLSISTA (??). Talvez área.

