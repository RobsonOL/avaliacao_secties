#### Packages -----------
rm(list = ls())

library(tidyverse)
library(vroom)
library(lubridate)

#### DADOS -------------
discentes_pb <- read_rds("dados/tidy/discentes_pb.rds")
teses_pb <- read_rds("dados/tidy/teses_dissertacoes_pb.rds")
bolsas_pb <- read_rds("dados/tidy/bolsas_pb.rds")
artigos_autor_pb <- read_rds("dados/tidy/artigos_autor_pb.rds")

#### DISCENTES PARAÍBA

# OBS:
# 1. DISCENTES: Não existe ID_PESSOA antes de 2013
# 2. DISCENTES: Não existe CPF antes de 2013
# 3. DISCENTES e BOLSAS: unir por NM_DISCENTE e NM_BOLSISTA (??). Talvez área.
# 4. BOLSAS: Existe ID_PESSOA antes de 2013

df <- discentes_pb |> 
  filter(ANO >= 2013) |> 
  distinct(ID_PESSOA, NM_DISCENTE, AN_NASCIMENTO_DISCENTE, NR_DOCUMENTO_DISCENTE) |> 
  crossing(ANO = seq(2013, 2020)) |> 
  relocate(ANO, .before = ID_PESSOA) |>
  mutate(IDADE_DISCENTE = ANO - AN_NASCIMENTO_DISCENTE) |> 
  # Algumas pessoas tem nomes diferentes ao longo dos anos:
  # ID_PESSOA == 51331 se chama ANNE KARINE DE SOUZA NASCIMENTO
  # e em outros momentos se chama ANNE KARINE DE SOUZA NASCIMENTO SOARES
  distinct(ID_PESSOA, ANO, .keep_all = TRUE)

# df |> group_by(ID_PESSOA) |>summarise(contagem = n()) |> 
#   group_by(contagem) |> summarise(n = n()) # todo mundo tem 8 anos de informações


# É BOLSISTA/ANO/PESSOA
df <- df |> left_join(
  bolsas_pb |> 
    select(ID_PESSOA, ANO, QT_BOLSA_ANO, VL_BOLSA_ANO, DS_NIVEL),
  by = c("ID_PESSOA", "ANO")
) |> 
  mutate(BOLSISTA = ifelse(
    !is.na(VL_BOLSA_ANO), 1, 0
  )) |> 
  relocate(BOLSISTA, .after = IDADE_DISCENTE)


