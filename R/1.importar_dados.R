# 1. PACKAGES -----------
rm(list = ls())
library(vroom)
library(skimr)
library(tidyverse)




# 2. DADOS -------------

# PATH_DADOS <- "C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/"
PATH_DADOS <- "dados/bruto"


##### Informações dos discentes de Pós-Graduação -----------------------------------
# https://dadosabertos.capes.gov.br/dataset/discentes-da-pos-graduacao-stricto-sensu-do-brasil-2017-a-2019

PATH_DISCENTES <- paste0(PATH_DADOS, "discentes/")

discentes <- list.files(path = PATH_DISCENTES, 
                        pattern = "*.zip", full.names = TRUE) |>  
  map_df(vroom, delim = ";", locale = locale(encoding = 'latin5'), col_types = cols(.default = "c")) 

discentes |> write_rds(paste0(PATH_DADOS, "discentes.rds"))




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
  
# Unidade de Ensino por Estado
ies_uf <- bolsas_programa |> 
  select(CD_PROGRAMA_PPG, NM_PROGRAMA_PPG,
         SG_IES_ESTUDO, NM_IES_ESTUDO, CD_IES_ESTUDO, SG_UF_IES_ESTUDO) |> 
  # remover _ESTUDO do nome das colunas:
  rename_with(~ str_remove(., "_ESTUDO"), ends_with("_ESTUDO")) |>
  rename_with(~ str_remove(., "_PPG"), ends_with("_PPG")) |>
  distinct() 

ies_uf |> 
  write_rds("dados/tidy/ies_uf.rds")

bolsas_programa |> write_rds(paste0(PATH_DADOS, "bolsas_programas.rds"))




##### Produção de artigos em periódicos --------------------------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-producao-intelectual-de-pos-graduacao-stricto-sensu-no-brasil
PATH_PERIODICOS = paste0(PATH_DADOS, "producao/")

producao_artigos_periodicos <- list.files(path = PATH_PERIODICOS,
                                          pattern = "artpe",
                                          full.names = TRUE) |>
  map_df(vroom, delim = ";",
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

producao_artigos_periodicos |> write_rds(paste0(PATH_DADOS, "producao_artigos_periodicos.rds"))


##### Autor da Produção de Periódicos -------
# https://dadosabertos.capes.gov.br/dataset/2017-a-2020-autor-da-producao-intelectual-de-programas-de-pos-graduacao-stricto-sensu

PATH_AUTOR_PERIODICOS <- paste0(PATH_DADOS, "producao_autor/")


autor_producao_periodicos <- list.files(path = PATH_AUTOR_PERIODICOS,
                                 pattern = "*.zip",
                                 full.names = TRUE) |>
  map_df(vroom, delim = ";",
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

autor_producao_periodicos |> write_rds(paste0(PATH_DADOS, "autor_producao_periodicos.rds"))

##### Bolsas de Mobilidade Internacional -----------------------
PATH_MOBILIDADE <- paste0(PATH_DADOS, "bolsas_mobilidade_internacional/")

bolsas_mobilidade <- list.files(path = PATH_MOBILIDADE,
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

discentes_pb |> write_rds("dados/tidy/discentes_pb.rds")


##### Teses e Dissertações da Paraíba ---------------------

teses_dissertacoes_pb <- teses_dissertacoes |> 
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


##### Produção Intelectual da Paraíba ---------------------

# ID_PESSOA é unico para cada indivíduo. Existe 5 colunas com ID_PESSOA:
# ID_PESSOA_DISCENTE; ID_PESSOA_DOCENTE; ID_PESSOA_POS_DOC; ID_PESSOA_EGRESSO;
# ID_PESSOA_PART_EXTERNO. 
# Se um artigo foi publicado quando o estudante já se formou, ele deve aparecer
# com ID_PESSOA_EGRESSO == ID_PESSOA e NM_AUTOR. 

# producao_artigos_periodicos <- read_rds(paste0(PATH_DADOS, "producao_artigos_periodicos.rds"))
# autor_producao_periodicos <- read_rds(paste0(PATH_DADOS, "autor_producao_periodicos.rds"))

artigos_autor <- producao_artigos_periodicos |> 
  select(ID_ADD_PRODUCAO_INTELECTUAL, NM_PRODUCAO, CD_PROGRAMA_IES,
         NM_PROGRAMA_IES, SG_ENTIDADE_ENSINO, SG_ENTIDADE_ENSINO,
         AN_BASE, NM_SUBTIPO_PRODUCAO, NM_AREA_CONCENTRACAO,
         SG_ESTRATO, DS_TITULO_PADRONIZADO, CD_IDENTIFICADOR_VEICULO) |> 
  rename(ANO = AN_BASE) |> 
  left_join(autor_producao_periodicos |> 
              select(ID_ADD_PRODUCAO_INTELECTUAL, 
                     NM_AUTOR, TP_AUTOR, NM_NIVEL_DISCENTE,
                     starts_with("ID_PESSOA")), 
            by = c("ID_ADD_PRODUCAO_INTELECTUAL")) |> 
  mutate(across(starts_with("ID_PESSOA"), as.numeric)) |> 
  mutate(across(starts_with("ID_PESSOA"), ~ ifelse(is.na(.), 0, 1),
                .names = "{sub('ID_PESSOA_', '', .col)}")) |> 
  mutate(ID_PESSOA = coalesce(ID_PESSOA_DISCENTE, ID_PESSOA_DOCENTE, 
                              ID_PESSOA_EGRESSO, ID_PESSOA_POS_DOC, 
                              ID_PESSOA_PART_EXTERNO))

# Filtrar para Programa de Pós-Graduação da Paraíba
ies_uf <-  read_rds("dados/tidy/ies_uf.rds")
cd_programa_paraiba <- ies_uf |> filter(SG_UF_IES == 'PB') |> pull(CD_PROGRAMA)

artigos_autor_pb <- artigos_autor |> filter(CD_PROGRAMA_IES %in% cd_programa_paraiba)
artigos_autor_pb |> write_rds("dados/tidy/artigos_autor_pb.rds")
