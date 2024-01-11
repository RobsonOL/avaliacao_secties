#### Packages -----------
rm(list = ls())
needs::needs(tidyverse, vroom)

#### DADOS -------------

# Quantidade de bolsas por programa de Pós-Graduação ---------------------------
quantidade_bolsas <- list.files(path = "dados/brutos/concessao/", pattern = "*.zip", full.names = TRUE) |> 
  map_df(read_csv, locale = locale(encoding = 'latin5'))

quantidade_bolsas |> write_rds("dados/brutos/quantidade_bolsas.rds")


# InformaÃ§Ãµes dos discentes de Pós-Graduação -----------------------------------
# https://dadosabertos.capes.gov.br/dataset/discentes-da-pos-graduacao-stricto-sensu-do-brasil-2017-a-2019

discentes <- list.files(path = "dados/brutos/discentes/", pattern = "*.zip", full.names = TRUE) |>  
  map_df(vroom, delim = ";", locale = locale(encoding = 'latin5'), col_types = cols(.default = "c")) 

discentes |> write_rds("dados/brutos/discentes.rds")


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

# Bolsas de Mobilidade Internacional -----------------------------------------
bolsas_mobilidade <- list.files(path = "dados/brutos/bolsas_mobilidade_internacional/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

bolsas_mobilidade |> write_rds("dados/brutos/bolsas_mobilidade.rds")
