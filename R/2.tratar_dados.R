#### Packages -----------
rm(list = ls())
needs::needs(tidyverse, vroom, lubridate)

#### DADOS -------------
discentes <- read_rds("dados/bruto/discentes.rds")
teses_dissertacoes <- read_rds("dados/bruto/teses_dissertacoes.rds")
bolsas_mobilidade <- read_rds("dados/bruto/bolsas_mobilidade.rds")

#### DISCENTES PARAÍBA
discentes_pb <- discentes |> 
  filter(SG_UF_PROGRAMA == "PB") |> 
  mutate(across(c(AN_BASE, CD_AREA_AVALIACAO, AN_NASCIMENTO_DISCENTE,
                  CD_ENTIDADE_CAPES, CD_CONCEITO_PROGRAMA, CD_CONCEITO_CURSO,
                  ID_PESSOA, QT_MES_TITULACAO), as.numeric)) |> 
  mutate(across(c(DT_MATRICULA_DISCENTE), parse_date_time, "%d%b%y:%H:%M:%S")) |>
  mutate(IDADE = AN_BASE - AN_NASCIMENTO_DISCENTE) |>
  select(AN_BASE, NM_DISCENTE, ID_PESSOA, NR_DOCUMENTO_DISCENTE, 
         NM_PAIS_NACIONALIDADE_DISCENTE, IDADE, NM_SITUACAO_DISCENTE, 
         DS_GRAU_ACADEMICO_DISCENTE, DT_MATRICULA_DISCENTE, QT_MES_TITULACAO,
         SG_ENTIDADE_ENSINO, CD_PROGRAMA_IES, NM_GRANDE_AREA_CONHECIMENTO,
         CD_ENTIDADE_CAPES, NM_PROGRAMA_IES, NM_MODALIDADE_PROGRAMA, 
         NM_GRAU_PROGRAMA, NM_MUNICIPIO_PROGRAMA_IES, CD_CONCEITO_PROGRAMA, 
         CD_CONCEITO_PROGRAMA) |> 
  mutate(across(where(is.character), ~na_if(., "NÃO SE APLICA"))) 

discentes_pb |> write_rds("dados/tidy/discentes_pb.rds")

##### TESES E DISSERTAÇÕES PARAÍBA

teses_dissertacoes_pb <- teses_dissertacoes |> filter(SG_UF_IES == "PB")


teses_dissertacoes_pb |> write_rds("dados/tidy/teses_dissertacoes_pb.rds")



# discentes_pb |> skimr::skim()



