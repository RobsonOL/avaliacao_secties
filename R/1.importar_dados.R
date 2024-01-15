# 1. PACKAGES -----------
rm(list = ls())
library(vroom)
library(skimr)
library(tidyverse)




# 2. DADOS -------------
# White screen do RSTUDIO por incluir dados tão grandes na pasta do projeto (????)
# PATH_DADOS <- "C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/"
PATH_DADOS <- "dados/bruto"


##### Informações dos discentes de Pós-Graduação -----------------------------------
# https://dadosabertos.capes.gov.br/dataset/discentes-da-pos-graduacao-stricto-sensu-do-brasil-2017-a-2019

PATH_DISCENTES <- paste0(PATH_DADOS, "discentes/")

discentes <- list.files(path = PATH_DISCENTES, 
                        pattern = "*.zip", full.names = TRUE) |>  
  map_df(vroom, delim = ";", locale = locale(encoding = 'latin5'), col_types = cols(.default = "c")) 

discentes |> write_rds(paste0(PATH_DADOS, "discentes.rds"))


##### Produção de artigos em periódicos --------------------------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-producao-intelectual-de-pos-graduacao-stricto-sensu-no-brasil

producao_artigos_periodicos <- list.files(path = "dados/brutos/producao/",
                                          pattern = "artpe",
                                          full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

producao_artigos_periodicos |> write_rds("dados/brutos/producao_artigos_periodicos.rds")


##### Teses e dissertações de discentes de Pós-Graduação ---------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-2020-catalogo-de-teses-e-dissertacoes-da-capes
PATH_DISSERTACOES = paste0(PATH_DADOS, "teses_dissertacoes/")
  
teses_dissertacoes_2013_2020 <- list.files(path = PATH_DISSERTACOES,
                                 pattern = "br-capes",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

teses_dissertacoes_2010_2012 <- list.files(path = PATH_DISSERTACOES,
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
         NM_ORIENTADOR = Orientador_1,
         NR_CPF = DocumentoDiscente
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
  bind_rows(teses_dissertacoes_2010_2012) |> 
  rename(ANO = AN_BASE) |> 
  mutate(across(c(starts_with(c("AN", "ID", "CD", "NR")), 
                  -NR_CPF, -CD_PROGRAMA),as.numeric)) 

  


teses_dissertacoes |> write_rds(paste0(PATH_DADOS, "teses_dissertacoes.rds"))






##### Bolsas de programas de Pós-Graduação -----------------------------------------
PATH_BOLSAS = paste0(PATH_DADOS, "bolsas_programas/")


bolsas_programa <- list.files(path = PATH_BOLSAS,
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))


bolsas_programa <- bolsas_programa |> 
  rename(ANO = AN_REFERENCIA) |> 
  rename(NM_DISCENTE = NM_BOLSISTA) |> 
  mutate(across(where(is.character), ~ na_if(., "NI"))) |> 
  mutate(across(c(ANO, ID_PESSOA, CD_IES_ESTUDO, 
                  # CD_CONCEITO_ANO,
                  CD_AREA_AVALIACAO, QT_BOLSA_ANO,
                  QT_TAXA_ANO, CD_IES_ORIGEM), as.numeric)) |> 
  mutate(VL_BOLSA_ANO = parse_number(VL_BOLSA_ANO)) |>
  mutate(VL_TAXA_ANO = parse_number(VL_TAXA_ANO)) |> 
  relocate(NM_DISCENTE, .after = ANO) 
  

bolsas_programa |> write_rds(paste0(PATH_DADOS, "bolsas_programas.rds"))


#####Autor da ProduÃ§Ã£o de PeriÃ³dicos -------
autor_producao_periodicos <- list.files(path = "dados/brutos/producao_autor/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

autor_producao_periodicos |> write_rds(paste0(PATH_DADOS, "autor_producao_periodicos.rds"))

##### Bolsas de Mobilidade Internacional -----------------------
bolsas_mobilidade <- list.files(path = "dados/brutos/bolsas_mobilidade_internacional/",
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

bolsas_mobilidade |> write_rds(paste0(PATH_DADOS, "bolsas_mobilidade.rds"))





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
  # Filtrar para teses na Paraíba
  filter(SG_UF_IES == "PB") |> 
  # Remover colunas que só foram adicionadas a partir de 2012
  select(-c(ID_ADD_PRODUCAO_INTELECTUAL, ID_PRODUCAO_INTELECTUAL,
            ID_SUBTIPO_PRODUCAO, NM_SUBTIPO_PRODUCAO,
            ID_AREA_CONCENTRACAO, NM_AREA_CONCENTRACAO,
            ID_LINHA_PESQUISA, NM_LINHA_PESQUISA,
            ID_PROJETO, NM_PROJETO, DH_INICIO_AREA_CONC, DH_FIM_AREA_CONC,
            DH_INICIO_LINHA, DH_FIM_LINHA, DS_ABSTRACT,
            DS_KEYWORD, IN_TRABALHO_MESMA_AREA, NM_TP_VINCULO,
            IN_ORIENT_PARTICIPOU_BANCA, ID_TP_EXPECTATIVA_ATUACAO,
            ID_GRAU_ACADEMICO, DS_CATEGORIA_ORIENTADOR, NM_UF_IES,
            CD_SUBAREA_CONHECIMENTO, CD_ESPECIALIDADE, NM_ESPECIALIDADE,
            DS_URL_TEXTO_COMPLETO, IN_TCC_COM_VINCULO_PRODUCAO, 
            ID_ADD_PRODUCAO_VINCULO_CT)) |> 
  relocate(NM_DISCENTE, .after = ANO) |> 
  relocate(NR_CPF, .after = NM_DISCENTE) |> 
  relocate(NM_ORIENTADOR, .after = NR_CPF) |>
  relocate(NM_PRODUCAO, .after = NM_ORIENTADOR) |> 
  rename(ID_PESSOA = ID_PESSOA_DISCENTE) |> 
  mutate(SG_ENTIDADE_ENSINO = case_match(SG_ENTIDADE_ENSINO,
                                         "UFPB/J.P." ~ "UFPB-JP",
                                         "UFPB/RT" ~ "UFPB-RT",
                                         "UFPB/AREIA" ~ "UFPB-AREIA",
                                         .default = SG_ENTIDADE_ENSINO)) |> 
  mutate(NM_GRAU_ACADEMICO = case_match(NM_GRAU_ACADEMICO,
                                         "Mestrado" ~ "MESTRADO",
                                         "Doutorado" ~ "DOUTORADO",
                                         "Profissionalizante" ~ "MESTRADO PROFISSIONAL",
                                         .default = NM_GRAU_ACADEMICO)) 



teses_dissertacoes_pb |> group_by(NM_GRANDE_AREA_CONHECIMENTO) |> count() |> arrange(desc(n))

teses_dissertacoes_pb |> write_rds("dados/tidy/teses_dissertacoes_pb.rds")


##### Bolsas da Paraíba ---------------------
bolsas_pb <- bolsas_programa |> 
  filter(SG_UF_IES_ESTUDO == "PB")

bolsas_pb |> write_rds("dados/tidy/bolsas_pb.rds")


