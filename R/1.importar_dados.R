#### 1. Packages -----------
rm(list = ls())
needs::needs(tidyverse, vroom)

#### 2. DADOS -------------

# Quantidade de bolsas por programa de Pós-Graduação ---------------------------
quantidade_bolsas <- list.files(path = "dados/brutos/concessao/", pattern = "*.zip", full.names = TRUE) |> 
  map_df(read_csv, locale = locale(encoding = 'latin5'))

quantidade_bolsas |> write_rds("dados/brutos/quantidade_bolsas.rds")


# InformaÃ§Ãµes dos discentes de Pós-Graduação -----------------------------------
# https://dadosabertos.capes.gov.br/dataset/discentes-da-pos-graduacao-stricto-sensu-do-brasil-2017-a-2019

# White screen do RSTUDIO por incluir dados tão grandes no projeto (????)
PATH_DISCENTES <- "C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/discentes/"

discentes <- list.files(path = PATH_DISCENTES, 
                        pattern = "*.zip", full.names = TRUE) |>  
  map_df(vroom, delim = ";", locale = locale(encoding = 'latin5'), col_types = cols(.default = "c")) 

discentes |> write_rds("C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/discentes.rds")


# ProduÃ§Ã£o de artigos em periÃ³dicos --------------------------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-producao-intelectual-de-pos-graduacao-stricto-sensu-no-brasil
producao_artigos_periodicos <- list.files(path = "dados/brutos/producao/",
                                          pattern = "artpe",
                                          full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

producao_artigos_periodicos |> write_rds("dados/brutos/producao_artigos_periodicos.rds")


# Teses e dissertaÃ§Ãµes de discentes de Pós-Graduação ---------------------------
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

# Bolsas de programas de Pós-Graduação -----------------------------------------
bolsas_programa <- list.files(path = "dados/brutos/bolsas_programas/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

bolsas_programa |> write_rds("dados/brutos/bolsas_programas.rds")


# Autor da ProduÃ§Ã£o de PeriÃ³dicos
autor_producao_periodicos <- list.files(path = "dados/brutos/producao_autor/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

autor_producao_periodicos |> write_rds("dados/brutos/autor_producao_periodicos.rds")

######## Bolsas de Mobilidade Internacional -----------------------------
bolsas_mobilidade <- list.files(path = "dados/brutos/bolsas_mobilidade_internacional/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

bolsas_mobilidade |> write_rds("dados/brutos/bolsas_mobilidade.rds")






#### 3. FILTRAR PARA PARAÍBA -----------------------

x <- discentes |> group_by(AN_BASE) |> sample_n(100)

x |>   
  mutate(SG_UF_PROGRAMA = if_else(is.na(SG_UF_PROGRAMA), SG_UF_ENTIDADE_ENSINO, SG_UF_PROGRAMA)) |> 
  # filter(AN_BASE %in% c(2012,2013)) |> 
  filter(SG_UF_PROGRAMA == "PB") |> 
  group_by(AN_BASE, SG_UF_ENTIDADE_ENSINO, SG_UF_PROGRAMA) |> count() |> View()


discentes_pb <- discentes |> 
  # SG_UF_PROGRAMA == "PB" substituiu SG_UF_ENTIDADE_ENSINO em 2012
  mutate(SG_UF_PROGRAMA = if_else(
    is.na(SG_UF_PROGRAMA), SG_UF_ENTIDADE_ENSINO, SG_UF_PROGRAMA)) |> 
  filter(SG_UF_PROGRAMA == "PB") |> 
  mutate(across(c(AN_BASE, CD_AREA_AVALIACAO, AN_NASCIMENTO_DISCENTE,
                  CD_ENTIDADE_CAPES, CD_CONCEITO_PROGRAMA, CD_CONCEITO_CURSO,
                  ID_PESSOA, QT_MES_TITULACAO), as.numeric)) |> 
  mutate(across(c(DT_MATRICULA_DISCENTE), parse_date_time, "%d%b%y:%H:%M:%S")) |>
  mutate(IDADE = AN_BASE - AN_NASCIMENTO_DISCENTE) |>
  select(AN_BASE, NM_DISCENTE, ID_PESSOA, NR_DOCUMENTO_DISCENTE, 
         NM_PAIS_NACIONALIDADE_DISCENTE, IDADE, NM_SITUACAO_DISCENTE, 
         DS_GRAU_ACADEMICO_DISCENTE, DT_MATRICULA_DISCENTE, QT_MES_TITULACAO,
         SG_ENTIDADE_ENSINO, SG_UF_PROGRAMA,
         CD_PROGRAMA_IES, NM_GRANDE_AREA_CONHECIMENTO,
         CD_ENTIDADE_CAPES, NM_PROGRAMA_IES, NM_MODALIDADE_PROGRAMA, 
         NM_GRAU_PROGRAMA, NM_MUNICIPIO_PROGRAMA_IES, CD_CONCEITO_PROGRAMA, 
         CD_CONCEITO_PROGRAMA) |> 
  mutate(across(where(is.character), ~ na_if(., "NÃO SE APLICA"))) |> 
  rename(ANO = AN_BASE)

discentes |> select(DT_MATRICULA_DISCENTE) |> sample_n(100) |> View()


discentes_pb |> write_rds("dados/tidy/discentes_pb.rds")
# discentes_pb |> skimr::skim()



##### TESES E DISSERTAÇÕES PARAÍBA ---------------------
teses_dissertacoes_pb <- teses_dissertacoes |> 
  filter(SG_UF_IES == "PB") |> 
  rename(ANO = AN_BASE)
teses_dissertacoes_pb |> write_rds("dados/tidy/teses_dissertacoes_pb.rds")



##### BOLSAS  DA PARAÍBA ---------------------
bolsas <- bolsas |> 
  mutate(across(c(ID_PESSOA, AN_REFERENCIA), as.numeric)) |> 
  rename(ANO = AN_REFERENCIA)

bolsas_pb <- bolsas |> 
  filter(SG_UF_IES_ESTUDO == "PB")
bolsas_pb |> write_rds("dados/tidy/bolsas_pb.rds")


