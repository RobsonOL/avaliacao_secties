# 1. PACKAGES -----------
rm(list = ls())
library(vroom)
library(skimr)
library(tidyverse)




# 2. DADOS -------------

##### Quantidade de bolsas por programa de Pós-Graduação ---------------------------
quantidade_bolsas <- list.files(path = "dados/brutos/concessao/", 
                                pattern = "*.zip", full.names = TRUE) |> 
  map_df(read_csv, locale = locale(encoding = 'latin5'))

quantidade_bolsas |> write_rds("dados/brutos/quantidade_bolsas.rds")



##### Informações dos discentes de Pós-Graduação -----------------------------------
# https://dadosabertos.capes.gov.br/dataset/discentes-da-pos-graduacao-stricto-sensu-do-brasil-2017-a-2019

# White screen do RSTUDIO por incluir dados tão grandes no projeto (????)
PATH_DISCENTES <- "C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/discentes/"

discentes <- list.files(path = PATH_DISCENTES, 
                        pattern = "*.zip", full.names = TRUE) |>  
  map_df(vroom, delim = ";", locale = locale(encoding = 'latin5'), col_types = cols(.default = "c")) 

discentes |> write_rds("C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/discentes.rds")


##### Produção de artigos em periódicos --------------------------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-producao-intelectual-de-pos-graduacao-stricto-sensu-no-brasil
producao_artigos_periodicos <- list.files(path = "dados/brutos/producao/",
                                          pattern = "artpe",
                                          full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

producao_artigos_periodicos |> write_rds("dados/brutos/producao_artigos_periodicos.rds")


##### Teses e dissertaÃ§Ãµes de discentes de Pós-Graduação ---------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-2020-catalogo-de-teses-e-dissertacoes-da-capes
teses_dissertacoes_2013_2020 <- list.files(path = "dados/bruto/teses_dissertacoes/",
                                 pattern = "br-capes",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

teses_dissertacoes_2010_2012 <- list.files(path = "dados/bruto/teses_dissertacoes/",
                                           pattern = "dados_",
                                           full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))


teses_dissertacoes_2010_2012 <- teses_dissertacoes_2010_2012 |> 
  rename(AN_BASE = AnoBase,
         CD_PROGRAMA = CodigoPrograma,
         NM_REGIAO = Regiao,
         SG_UF_IES = Uf,
         SG_ENTIDADE_ENSINO = SiglaIes,
         NM_ENTIDADE_ENSINO = NomeIes,
         NM_PROGRAMA = NomePrograma,
         CD_GRANDE_AREA_CONHECIMENTO = GrandeAreaCodigo,
         NM_GRANDE_AREA_CONHECIMENTO = GrandeAreaDescricao,
         CD_AREA_CONHECIMENTO = AreaConhecimentoCodigo,
         NM_AREA_CONHECIMENTO = AreaConhecimento,
         NM_AREA_AVALIACAO = AreaAvaliacao,
         NM_DISCENTE = Autor,
         NM_PRODUCAO = TituloTese,
         NM_GRAU_ACADEMICO = Nivel,
         DT_TITULACAO = DataDefesa, # dúvida?
         DS_PALAVRA_CHAVE = PalavrasChave,
         NR_VOLUME = Volume,
         NR_PAGINAS = NumeroPaginas,
         DS_BIBLIOTECA_DEPOSITARIA = BibliotecaDepositaria,
         NM_IDIOMA = Idioma,
         DS_RESUMO = ResumoTese,
         NM_LINHA_PESQUISA = LinhaPesquisa,
         DS_URL_TEXTO_COMPLETO = URLTextoCompleto,
         NM_ORIENTADOR = Orientador_1
         ) |> 
  select(-starts_with("Documento"), -starts_with("Orientador"),
         -starts_with("CoOrientador"), -`%PDF-1.4`) |> 
  mutate(DT_TITULACAO = as.Date(DT_TITULACAO, format = "%d/%m/%Y"))


teses_dissertacoes_2013_2020 <- teses_dissertacoes_2013_2020 |> 
  mutate(DT_TITULACAO = case_when(
    AN_BASE <= 2016 ~ as.Date(DT_TITULACAO, format = "%d/%m/%Y"),
    AN_BASE > 2016 ~ parse_date_time(DT_TITULACAO, c("%d%b%y:%H:%M:%S"), 
                                     locale = Sys.setlocale("LC_TIME", "en")))) 

teses_dissertacoes <- teses_dissertacoes_2013_2020 |> 
  bind_rows(teses_dissertacoes_2010_2012)


teses_dissertacoes |> write_rds("dados/bruto/teses_dissertacoes.rds")

##### Bolsas de programas de Pós-Graduação -----------------------------------------
bolsas_programa <- list.files(path = "dados/brutos/bolsas_programas/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

bolsas_programa |> write_rds("dados/brutos/bolsas_programas.rds")


#####Autor da ProduÃ§Ã£o de PeriÃ³dicos -------
autor_producao_periodicos <- list.files(path = "dados/brutos/producao_autor/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

autor_producao_periodicos |> write_rds("dados/brutos/autor_producao_periodicos.rds")

##### Bolsas de Mobilidade Internacional -----------------------
bolsas_mobilidade <- list.files(path = "dados/brutos/bolsas_mobilidade_internacional/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

bolsas_mobilidade |> write_rds("dados/brutos/bolsas_mobilidade.rds")





# 3. FILTRAR PARA PARAÍBA -----------------------

##### Discentes -----------------------
discentes_pb <- discentes |> 
  mutate(across(c(AN_BASE, CD_AREA_AVALIACAO, AN_NASCIMENTO_DISCENTE,
                  AN_MATRICULA_DISCENTE, ME_MATRICULA_DISCENTE,
                  ID_PESSOA), as.numeric)) |> 
  select(AN_BASE, NM_DISCENTE, NR_DOCUMENTO_DISCENTE, ID_PESSOA,
         AN_NASCIMENTO_DISCENTE, DT_MATRICULA_DISCENTE, AN_MATRICULA_DISCENTE, 
         ME_MATRICULA_DISCENTE, NM_SITUACAO_DISCENTE, SG_ENTIDADE_ENSINO, 
         NM_ENTIDADE_ENSINO, CS_STATUS_JURIDICO, SG_UF_ENTIDADE_ENSINO, SG_UF_PROGRAMA,
         CD_PROGRAMA_IES, NM_PROGRAMA_IES, NM_MODALIDADE_PROGRAMA,
         CD_AREA_AVALIACAO, NM_AREA_AVALIACAO, NM_NIVEL_TITULACAO_DISCENTE,
         DS_GRAU_ACADEMICO_DISCENTE, NM_ORIENTADOR, NM_ORIENTADOR_PRINCIPAL,
         NM_NIVEL_PROGRAMA, NM_GRAU_PROGRAMA) |> 
  # SG_UF_PROGRAMA substituiu SG_UF_ENTIDADE_ENSINO em 2012
  mutate(SG_UF_PROGRAMA = if_else(AN_BASE <= 2012,
                                  SG_UF_ENTIDADE_ENSINO, 
                                  SG_UF_PROGRAMA)) |> 
  # FILTRAR PARA PARAÍBA
  filter(SG_UF_PROGRAMA == "PB") |> 
  # DT_MATRICULA_DISCENTE existe apenas a partir de 2013
  mutate(across(c(DT_MATRICULA_DISCENTE), parse_date_time, "%d%b%y:%H:%M:%S")) |>
  mutate(DT_MATRICULA_DISCENTE = if_else(AN_BASE <= 2012, 
                                         as.Date(paste0(AN_MATRICULA_DISCENTE, "-", ME_MATRICULA_DISCENTE, "-01")), 
                                         DT_MATRICULA_DISCENTE)) |> 
  mutate(IDADE = AN_BASE - AN_NASCIMENTO_DISCENTE) |>
  # ORIENTADOR:
  mutate(NM_ORIENTADOR = if_else(AN_BASE <= 2012,
                                  NM_ORIENTADOR_PRINCIPAL, 
                                 NM_ORIENTADOR)) |> 
  # NIVEL DO PROGRAMA
  mutate(NM_GRAU_PROGRAMA = if_else(AN_BASE <= 2012,
                                 NM_NIVEL_PROGRAMA, 
                                 NM_GRAU_PROGRAMA)) |> 
  # NIVEL DE TITULACAO DO DISCENTE
  mutate(DS_GRAU_ACADEMICO_DISCENTE = if_else(AN_BASE <= 2012,
                                    NM_NIVEL_TITULACAO_DISCENTE, 
                                    DS_GRAU_ACADEMICO_DISCENTE)) |> 
  select(-c(NM_ORIENTADOR_PRINCIPAL, NM_NIVEL_PROGRAMA, AN_MATRICULA_DISCENTE,
            ME_MATRICULA_DISCENTE, SG_UF_ENTIDADE_ENSINO,NM_NIVEL_TITULACAO_DISCENTE)) |> 
  mutate(across(where(is.character), ~ na_if(., "NI"))) |> 
  rename(ANO = AN_BASE) |> 
  relocate(IDADE, .after = AN_NASCIMENTO_DISCENTE) |> 
  mutate(SG_ENTIDADE_ENSINO = case_match(SG_ENTIDADE_ENSINO,
                                         "UFPB/J.P." ~ "UFPB-JP",
                                         "UFPB/RT" ~ "UFPB-RT",
                                         "UFPB/AREIA" ~ "UFPB-AREIA",
                                         .default = SG_ENTIDADE_ENSINO))

# discentes_pb |> filter(ANO %in% c(2012,2013)) |> group_by(ANO) |> sample_n(10) |> View()
# discentes_pb |> skimr::skim()
# discentes_pb |> group_by(NM_SITUACAO_DISCENTE) |> count() |> arrange(desc(n)) 
# discentes_pb |> group_by(NM_ENTIDADE_ENSINO) |> count() |> arrange(desc(n)) 
# discentes_pb |> group_by(NM_GRAU_PROGRAMA) |> count() |> arrange(desc(n)) 

discentes_pb |> write_rds("dados/tidy/discentes_pb.rds")


##### Teses e Dissertações da Paraíba ---------------------
teses_dissertacoes_pb <- teses_dissertacoes |> 
  filter(SG_UF_IES == "PB") |> 
  rename(ANO = AN_BASE)
teses_dissertacoes_pb |> write_rds("dados/tidy/teses_dissertacoes_pb.rds")



##### Bolsas da Paraíba ---------------------
bolsas <- bolsas |> 
  mutate(across(c(ID_PESSOA, AN_REFERENCIA), as.numeric)) |> 
  rename(ANO = AN_REFERENCIA)

bolsas_pb <- bolsas |> 
  filter(SG_UF_IES_ESTUDO == "PB")
bolsas_pb |> write_rds("dados/tidy/bolsas_pb.rds")


