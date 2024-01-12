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

# EDGE CASES

# Discentes com mais de uma ocorrência em um mesmo ano.
# contagem = 2 pode indicar que alguém defendeu no ano X e cursou doutorado no mesmo ano.
# mas contagem > 2? (215 observações)

discentes_pb |> group_by(NM_DISCENTE, ANO) |> 
  mutate(contagem = n()) |> group_by(contagem) |> count()

# contagem        n
#        1   124570
#        2     7480
#        3      207
#        4        8

# Jualiana aparece em 4 programas diferentes da UFCG em 2004; no ano de 2005, em 3;
# em 2006 e 2007, em 2; e no ano de 2008, em 1 programa. Em nenhum como concluído.
discentes_pb |> 
  filter(NM_DISCENTE == 'JULIANA DE CASTRO MACEDO FONSECA') |> 
  left_join(bolsas_pb) |> View()


discentes_pb |> 
  group_by(NM_DISCENTE, ANO) |> 
  mutate(contagem = n()) |> 
  ungroup() |> 
  filter(contagem > 2) |> 
  relocate(contagem, .before = everything()) |> 
  arrange(desc(contagem)) |> 
  View()


  
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



