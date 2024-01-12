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
# 3. DISCENTES e BOLSAS: unir por NM_DISCENTE e NM_BOLSISTA (??). Talvez área.
# 4. BOLSAS: Existe ID_PESSOA antes de 2013


  
discentes_pb |> 
  select(ANO, NM_DISCENTE, NR_DOCUMENTO_DISCENTE, ID_PESSOA, IDADE, NM_SITUACAO_DISCENTE,
         SG_ENTIDADE_ENSINO, NM_PROGRAMA_IES, DS_GRAU_ACADEMICO_DISCENTE,
         NM_ORIENTADOR) |> 
  #group_by(ANO, NM_DISCENTE) |>
  #mutate(contagem = n()) |> 
  #ungroup() |> 
  left_join(bolsas_pb |> select(ANO, NM_DISCENTE, ID_PESSOA, NR_DOCUMENTO,
                                SG_IES_ESTUDO, DS_NIVEL, NM_PROGRAMA_PPG,
                                QT_BOLSA_ANO, VL_BOLSA_ANO),
            by = c("ANO", "NM_DISCENTE")) |> 
  relocate(NR_DOCUMENTO_DISCENTE, .after = NR_DOCUMENTO) |> 
  relocate(ID_PESSOA.x, .after = ID_PESSOA.y) |>
  View()
