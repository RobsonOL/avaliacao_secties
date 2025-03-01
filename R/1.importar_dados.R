# 1. PACKAGES -----------
rm(list = ls())
pacman::p_load(tidyverse, janitor, readr, tidyr, skimr, vroom)

# 2. DADOS -------------

# PATH_DADOS <- "C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/"
PATH_DADOS <- "dados/bruto"


##### Informações dos discentes de Pós-Graduação -----------------------------------
# https://dadosabertos.capes.gov.br/dataset/discentes-da-pos-graduacao-stricto-sensu-do-brasil-2017-a-2019

PATH_DISCENTES <- paste0(PATH_DADOS, "discentes/")

discentes <- list.files(path = PATH_DISCENTES, 
                        pattern = "*.zip", full.names = TRUE) |>  
  purrr::map_df(vroom::vroom, delim = ";", locale = vroom::locale(encoding = 'latin5'), col_types = vroom::cols(.default = "c")) 

discentes |> readr::write_rds(paste0(PATH_DADOS, "discentes.rds"))

##### Teses e dissertações de discentes de Pós-Graduação ---------------------------
# https://dadosabertos.capes.gov.br/dataset/2017-2020-catalogo-de-teses-e-dissertacoes-da-capes
PATH_DISSERTACOES = paste0(PATH_DADOS, "teses_dissertacoes/")
  
teses_dissertacoes_2013_2022 <- list.files(path = PATH_DISSERTACOES,
                                 pattern = "br-capes",
                                 full.names = TRUE) |>
  map_df(read_csv2,
         locale = locale(encoding = 'latin5'),
         col_types = cols(.default = "c"))

# teses_dissertacoes_2010_2012 <- list.files(path = PATH_DISSERTACOES,
#                                            pattern = "dados_",
#                                            full.names = TRUE) |>
#   map_df(read_csv2,
#          locale = locale(encoding = 'latin5'),
#          col_types = cols(.default = "c"))


# teses_dissertacoes_2010_2012 <- teses_dissertacoes_2010_2012 |> 
#   rename(AN_BASE = AnoBase,
#          CD_PROGRAMA = CodigoPrograma,
#          NM_REGIAO = Regiao,
#          SG_UF_IES = Uf,
#          SG_ENTIDADE_ENSINO = SiglaIes,
#          NM_ENTIDADE_ENSINO = NomeIes,
#          NM_PROGRAMA = NomePrograma,
#          CD_GRANDE_AREA_CONHECIMENTO = GrandeAreaCodigo,
#          NM_GRANDE_AREA_CONHECIMENTO = GrandeAreaDescricao,
#          CD_AREA_CONHECIMENTO = AreaConhecimentoCodigo,
#          NM_AREA_CONHECIMENTO = AreaConhecimento,
#          NM_AREA_AVALIACAO = AreaAvaliacao,
#          NM_DISCENTE = Autor,
#          NM_PRODUCAO = TituloTese,
#          NM_GRAU_ACADEMICO = Nivel,
#          DT_TITULACAO = DataDefesa, # dúvida?
#          DS_PALAVRA_CHAVE = PalavrasChave,
#          NR_VOLUME = Volume,
#          NR_PAGINAS = NumeroPaginas,
#          DS_BIBLIOTECA_DEPOSITARIA = BibliotecaDepositaria,
#          NM_IDIOMA = Idioma,
#          DS_RESUMO = ResumoTese,
#          NM_LINHA_PESQUISA = LinhaPesquisa,
#          DS_URL_TEXTO_COMPLETO = URLTextoCompleto,
#          NM_ORIENTADOR = Orientador_1,
#          NR_CPF = DocumentoDiscente
#          ) |> 
#   select(-starts_with("Documento"), -starts_with("Orientador"),
#          -starts_with("CoOrientador"), -`%PDF-1.4`) |> 
#   mutate(DT_TITULACAO = as.Date(DT_TITULACAO, format = "%d/%m/%Y"))


teses_dissertacoes_2013_2022 <- teses_dissertacoes_2013_2022 |>
  mutate(DT_TITULACAO = case_when(
    AN_BASE <= 2016 ~ as.Date(DT_TITULACAO, format = "%d/%m/%Y"),
    AN_BASE > 2016 ~ parse_date_time(DT_TITULACAO, c("%d%b%y:%H:%M:%S"),
                                     locale = Sys.setlocale("LC_TIME", "en"))
  )) |>
  rename(ANO = AN_BASE) |>
  mutate(across(c(starts_with(
    c("AN", "ID", "CD", "NR")
  ),
  # -NR_CPF,
  -CD_PROGRAMA), as.numeric))


teses_dissertacoes <- teses_dissertacoes_2013_2024
  # %>% bind_rows(teses_dissertacoes_2010_2012) 





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


##### Detalhes da Produção de Periódicos -------
# Contém o DOI da publicação

PATH_DETALHES <- paste0(PATH_DADOS, "producao_detalhes/")

# detalhes_producao_periodicos <- list.files(path = PATH_DETALHES,
#                                            pattern = "*.zip",
#                                            full.names = TRUE) |>
#   map_df(read_csv2,
#          locale = locale(encoding = 'latin5'),
#          col_types = cols(.default = "c"))



# 3. FILTRAR PARA PARAÍBA -----------------------

##### Discentes -----------------------

# discentes <- read_rds(paste0(PATH_DADOS, "discentes.rds"))

discentes_pb <- discentes |>
  mutate(across(
    c(AN_BASE, CD_AREA_AVALIACAO, AN_NASCIMENTO_DISCENTE,
      # AN_MATRICULA_DISCENTE, ME_MATRICULA_DISCENTE,
      ID_PESSOA),
    \(x) as.numeric(x)
  )) |> 
  filter(SG_UF_PROGRAMA == "PB") |>
  mutate(across(c(DT_MATRICULA_DISCENTE), \(x) parse_date_time(x, "%d%b%y:%H:%M:%S"))) |>
  mutate(across(c(DT_SITUACAO_DISCENTE), \(x) parse_date_time(x, "%d%b%y:%H:%M:%S"))) |>
  mutate(IDADE = AN_BASE - AN_NASCIMENTO_DISCENTE) |>
  # As linhas abaixo são para a inclusão das bases anteriores a 2012:
  # mutate(DT_SITUACAO_DISCENTE = if_else(AN_BASE <= 2012,
  #                                       as.Date(
  #                                         paste0(AN_SITUACAO_DISCENTE, "-", ME_SITUACAO_DISCENTE, "-01")
  #                                       ),
  #                                       DT_SITUACAO_DISCENTE)) |>
  # mutate(SG_UF_PROGRAMA = if_else(AN_BASE <= 2012,
  #                                 SG_UF_ENTIDADE_ENSINO,
  #                                 SG_UF_PROGRAMA)) |>
  # mutate(DT_MATRICULA_DISCENTE = if_else(AN_BASE <= 2012,
  #                                        as.Date(
  #                                          paste0(AN_MATRICULA_DISCENTE, "-", ME_MATRICULA_DISCENTE, "-01")
  #                                        ),
  #                                        DT_MATRICULA_DISCENTE)) |>
  # mutate(NM_ORIENTADOR = if_else(AN_BASE <= 2012,
  #                                NM_ORIENTADOR_PRINCIPAL,
  #                                NM_ORIENTADOR)) |>
  # mutate(NM_GRAU_PROGRAMA = if_else(AN_BASE <= 2012,
  #                                   NM_NIVEL_PROGRAMA,
  #                                   NM_GRAU_PROGRAMA)) |>
  # mutate(DS_GRAU_ACADEMICO_DISCENTE = if_else(AN_BASE <= 2012,
  #                                             NM_NIVEL_TITULACAO_DISCENTE,
  #                                             DS_GRAU_ACADEMICO_DISCENTE)) |>
  # select(
  #   -c(
  #     # NM_ORIENTADOR_PRINCIPAL,
  #     # NM_NIVEL_PROGRAMA,
  #     # AN_MATRICULA_DISCENTE,
  #     # ME_MATRICULA_DISCENTE,
  #     # AN_SITUACAO_DISCENTE,
  #     # ME_SITUACAO_DISCENTE,
  #     # SG_UF_ENTIDADE_ENSINO,
  #     # NM_NIVEL_TITULACAO_DISCENTE
  #   )
  # ) |>
  mutate(across(where(is.character), \(x) na_if(x, "NI"))) |>
  rename(ANO = AN_BASE) |>
  relocate(IDADE, .after = AN_NASCIMENTO_DISCENTE) |>
  mutate(
    SG_ENTIDADE_ENSINO = case_match(
      SG_ENTIDADE_ENSINO,
      "UFPB/J.P." ~ "UFPB",
      "UFPB/RT" ~ "UFPB",
      "UFPB/AREIA" ~ "UFPB",
      "UFPB-JP" ~ "UFPB",
      "UFPB-RT" ~ "UFPB",
      "UNIPÊ" ~ "UNIPE",
      .default = SG_ENTIDADE_ENSINO
    )
  ) |>
  dplyr::mutate(SG_ENTIDADE_ENSINO = stringr::str_to_upper(
    janitor::make_clean_names(SG_ENTIDADE_ENSINO, case = "sentence", allow_dupes = TRUE)
  )) |> 
  dplyr::mutate(NM_DISCENTE = stringr::str_to_upper(
    janitor::make_clean_names(NM_DISCENTE, case = "sentence", allow_dupes = TRUE)
  )) |> 
  dplyr::mutate(DS_GRAU_ACADEMICO_DISCENTE = case_match(
    DS_GRAU_ACADEMICO_DISCENTE,
    "MESTRADO" ~ "MESTRADO",
    "MESTRADO PROFISSIONAL" ~ "MESTRADO",
    "DOUTORADO" ~ "DOUTORADO",
    "DOUTORADO PROFISSIONAL" ~ "DOUTORADO",
    .default = DS_GRAU_ACADEMICO_DISCENTE
  ))
  

discentes_pb |> write_rds("dados/tidy/discentes_pb.rds")


##### Teses e Dissertações da Paraíba ---------------------

# teses_dissertacoes <- read_rds(paste0(PATH_DADOS, "teses_dissertacoes.rds"))

teses_dissertacoes_pb <- teses_dissertacoes |> 
  filter(SG_UF_IES == "PB") |> 
  # Remover colunas que só foram adicionadas a partir de 2012
  # select(-c(ID_ADD_PRODUCAO_INTELECTUAL, ID_PRODUCAO_INTELECTUAL,
  #           ID_SUBTIPO_PRODUCAO, NM_SUBTIPO_PRODUCAO,
  #           ID_AREA_CONCENTRACAO, NM_AREA_CONCENTRACAO,
  #           ID_LINHA_PESQUISA, NM_LINHA_PESQUISA,
  #           ID_PROJETO, NM_PROJETO, DH_INICIO_AREA_CONC, DH_FIM_AREA_CONC,
  #           DH_INICIO_LINHA, DH_FIM_LINHA, DS_ABSTRACT,
  #           DS_KEYWORD, IN_TRABALHO_MESMA_AREA, NM_TP_VINCULO,
  #           IN_ORIENT_PARTICIPOU_BANCA, ID_TP_EXPECTATIVA_ATUACAO,
  #           ID_GRAU_ACADEMICO, DS_CATEGORIA_ORIENTADOR, NM_UF_IES,
  #           CD_SUBAREA_CONHECIMENTO, CD_ESPECIALIDADE, NM_ESPECIALIDADE,
  #           DS_URL_TEXTO_COMPLETO, IN_TCC_COM_VINCULO_PRODUCAO, 
  #           ID_ADD_PRODUCAO_VINCULO_CT)) |> 
  relocate(NM_DISCENTE, .after = ANO) |> 
  # relocate(NR_CPF, .after = NM_DISCENTE) |> 
  # relocate(NM_ORIENTADOR, .after = NR_CPF) |>
  relocate(NM_PRODUCAO, .after = NM_ORIENTADOR) |> 
  rename(ID_PESSOA = ID_PESSOA_DISCENTE) |> 
  mutate(SG_ENTIDADE_ENSINO = case_match(SG_ENTIDADE_ENSINO,
                                         "UFPB/J.P." ~ "UFPB-JP",
                                         "UFPB/RT" ~ "UFPB-RT",
                                         "UFPB/AREIA" ~ "UFPB-AREIA",
                                         .default = SG_ENTIDADE_ENSINO)) |> 
  mutate(NM_GRAU_ACADEMICO = case_match(NM_GRAU_ACADEMICO,
                                        "MESTRADO" ~ "MESTRADO",
                                        "MESTRADO PROFISSIONAL" ~ "MESTRADO",
                                        "DOUTORADO" ~ "DOUTORADO",
                                        "DOUTORADO PROFISSIONAL" ~ "DOUTORADO",
                                         "Profissionalizante" ~ "MESTRADO PROFISSIONAL",
                                         .default = NM_GRAU_ACADEMICO)) |> 
  dplyr::mutate(
    NM_DISCENTE = stringr::str_to_upper(
      janitor::make_clean_names(NM_DISCENTE, case = "sentence", allow_dupes = TRUE)
    )
  )

teses_dissertacoes_pb |> write_rds("dados/tidy/teses_dissertacoes_pb.rds")


##### Bolsas da Paraíba ---------------------
# bolsas_programa <- read_rds(paste0(PATH_DADOS, "bolsas_programas.rds"))

bolsas_pb <- bolsas_programa |> 
  filter(SG_UF_IES_ESTUDO == "PB") |> 
  dplyr::mutate(
    NM_DISCENTE = stringr::str_to_upper(
      janitor::make_clean_names(NM_DISCENTE, case = "sentence", allow_dupes = TRUE)
    )
  )

bolsas_pb |> write_rds("dados/tidy/bolsas_pb.rds")



## CNPQ ----

# Bolsas de doutorado e mestrado na PB
DADOS_CNPQ <- paste0(PATH_DADOS, "bolsas_cnpq/")

colunas_selecionadas <- c("ano_referencia", "beneficiario", "linha_de_fomento", "modalidade",
                          "programa_cn_pq", "grande_area", "area", "subarea", "sigla_uf_origem",
                          "sigla_uf_destino", "instituicao_origem", "sigla_instituicao_destino", "valor_pago")

colunas_numericas <- c("ano_referencia", "valor_pago")
cnpq_2017 <-
  readr::read_delim(paste0(DADOS_CNPQ, "investimentos_cnpq_2017.zip"), 
                    delim = ";", locale = readr::locale(decimal_mark = ",")) |> 
  janitor::clean_names() |>
  dplyr::select(all_of(colunas_selecionadas)) |> 
  dplyr::mutate(across(colunas_numericas, as.numeric))

cnpq_2018 <-
  readr::read_delim(paste0(DADOS_CNPQ, "investimentos_cnpq_2018.zip"), 
                    delim = ",", locale = readr::locale(decimal_mark = ",")) |> 
  janitor::clean_names() |>
  dplyr::select(all_of(colunas_selecionadas)) |> 
  dplyr::mutate(across(colunas_numericas, as.numeric))

cnpq_2019 <-
  readr::read_delim(paste0(DADOS_CNPQ, "investimentos_cnpq_2019.zip"), 
                    delim = ",", locale = readr::locale(decimal_mark = ",")) |> 
  janitor::clean_names() |>
  dplyr::select(all_of(colunas_selecionadas)) |> 
  dplyr::mutate(across(colunas_numericas, as.numeric))

cnpq_2020 <-
  readr::read_delim(paste0(DADOS_CNPQ, "investimentos_cnpq_2020.zip"), 
                    delim=";",
                    locale = readr::locale(decimal_mark = ',')) |> 
  janitor::clean_names() |>
  select(-1) |> 
  dplyr::select(all_of(colunas_selecionadas)) |> 
  dplyr::mutate(across(colunas_numericas, as.numeric))

cnpq_2021 <-
  readr::read_delim(paste0(DADOS_CNPQ, "investimentos_cnpq_2021.zip"),
                    delim = ";",
                    locale = readr::locale(decimal_mark = ",")) |> 
  janitor::clean_names() |>
  dplyr::select(all_of(colunas_selecionadas)) |> 
  dplyr::mutate(across(colunas_numericas, as.numeric))

cnpq_2022 <-
  readr::read_delim(paste0(DADOS_CNPQ, "investimentos_cnpq_2022.zip"), 
                    skip = 5,
                    delim = ",",
                    locale=readr::locale(decimal_mark=",")) |> 
  janitor::clean_names() |>
  dplyr::mutate(valor_pago = readr::parse_number(valor_pago, locale = readr::locale(decimal_mark = ","))) |> 
  dplyr::select(all_of(colunas_selecionadas)) |> 
  dplyr::mutate(across(colunas_numericas, as.numeric)) |> 
  dplyr::filter(!is.na(ano_referencia))

modalidades_cnpq <- c("GD - Doutorado", "GM - Mestrado", 
                      "PDJ - Pós-doutorado Júnior", "GDE - Doutorado no Exterior",
                      "SWE - Doutorado Sanduíche no Exterior", "PDE - Pós-doutorado no Exterior",
                      "PDS - Pós-doutorado Sênior", "PDI - Pos-doutorado Empresarial", "SWP - Doutorado-Sanduiche no Pais",
                      "MPE - Mestrado Profissional no Exterior")

cnpq <- dplyr::bind_rows(cnpq_2017, cnpq_2018, cnpq_2019, cnpq_2020, cnpq_2021, cnpq_2022) |> 
  dplyr::filter(modalidade %in% modalidades_cnpq) |> 
  dplyr::filter(sigla_uf_origem == "PB" & sigla_uf_destino == "PB") |> 
  dplyr::select(ano_referencia, beneficiario, modalidade, sigla_instituicao_destino,
                sigla_uf_destino, area, subarea, grande_area, valor_pago) |> 
  dplyr::mutate(dplyr::across(
    dplyr::where(is.character),
    ~ stringr::str_to_upper(
      janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE)
    ))) |> 
  dplyr::mutate(
    sigla_instituicao_destino = ifelse(
      sigla_instituicao_destino == 'UFPB C GRANDE',
      "UFPB",
      sigla_instituicao_destino
    )
  ) |> 
  dplyr::rename(
    DS_NIVEL = modalidade,
    NM_DISCENTE = beneficiario,
    SG_ENTIDADE_ENSINO = sigla_instituicao_destino,
    VL_BOLSA_ANO = valor_pago,
    ANO = ano_referencia,
    AREA = area,
    SUBAREA = subarea,
    GRANDE_AREA = grande_area
  ) |> 
  dplyr::mutate(DS_NIVEL = case_when(
    DS_NIVEL %in% c("GD DOUTORADO", "GDE DOUTORADO NO EXTERIOR", 
                    "SWE DOUTORADO SANDUICHE NO EXTERIOR", 
                    "SWP DOUTORADO SANDUICHE NO PAIS") ~ "DOUTORADO",
    DS_NIVEL %in% c("GM MESTRADO", "MPE MESTRADO PROFISSIONAL NO EXTERIOR") ~ "MESTRADO",
    DS_NIVEL %in% c("PDJ POS DOUTORADO JUNIOR", "PDE POS DOUTORADO NO EXTERIOR", 
                    "PDS POS DOUTORADO SENIOR", "PDI POS DOUTORADO EMPRESARIAL") ~ "POS-DOUTORADO"
  )) |> 
  dplyr::mutate(TIPO_BOLSA = "CNPQ") |> 
  dplyr::select(
    ANO,
    NM_DISCENTE,
    TIPO_BOLSA,
    DS_NIVEL,
    SG_ENTIDADE_ENSINO,
    VL_BOLSA_ANO
  ) |> 
  dplyr::rename(DS_GRAU_ACADEMICO_DISCENTE = DS_NIVEL)


cnpq |>  write_rds("dados/tidy/bolsas_cnpq_pb.rds")







## Bolsas FAPESQ -----
fapesq_path <- "dados/bruto/fapesq/Informações editais de bolsas de mestrado, doutorado e pós-doutorado - FAPESQ.xlsx"
fapesq_sheets <- readxl::excel_sheets(fapesq_path)

###### Edital 0320216 ----

df1 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[1L]]) |>
  dplyr::rename(
    NM_DISCENTE = "NOME DO PESQUISADOR",
    DS_GRAU_ACADEMICO_DISCENTE = "MODALIDADE",
    SG_ENTIDADE_ENSINO = "INSTITUIÇÃO",
    NM_PROJETO = "Título do Projeto",
    INICIO_BOLSA = "INICIO DA VIGÊNCIA",
    FIM_BOLSA = "FIM DA VIGÊNCIA"
  ) |>
  dplyr::mutate(EDITAL = "Edital 03/2016",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |>
  dplyr::mutate(across(
    c(NM_DISCENTE, NM_PROJETO, SG_ENTIDADE_ENSINO),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(
    GENERO = case_match(GENERO,
                        "Female"  ~ "FEMININO",
                        "Male"   ~ "MASCULINO")
  ) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  ) 

###### Edital 072018 ----
df2 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[2L]]) |>
  dplyr::rename(
    NM_DISCENTE = "NOME DO PESQUISADOR",
    DS_GRAU_ACADEMICO_DISCENTE = "MODALIDADE",
    SG_ENTIDADE_ENSINO = "INSTITUIÇÃO",
    NM_PROJETO = "Título do Projeto",
    INICIO_BOLSA = "INICIO DA VIGÊNCIA",
    FIM_BOLSA = "FIM DA VIGÊNCIA",
    NM_PROGRAMA_IES = "PROGRAMA",
  ) |>
  dplyr::mutate(EDITAL = "Edital 07/2018",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |> 
  dplyr::mutate(GENERO = case_match(GENERO,
                                    "Female"  ~ "FEMININO",
                                    "Male"   ~ "MASCULINO")) |> 
  dplyr::mutate(across(
    c(NM_DISCENTE, NM_PROJETO, SG_ENTIDADE_ENSINO, NM_PROGRAMA_IES),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )



###### Edital 072021 ----

df3 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[3L]]) |>
  dplyr::rename(
    NM_DISCENTE = Bolsista,
    NM_SITUACAO_DISCENTE = Situacao,
    DS_GRAU_ACADEMICO_DISCENTE = "Modalidade/Nível",
    NR_DOCUMENTO_DISCENTE = "CPF Bolsista",
    SG_ENTIDADE_ENSINO = "Instituição",
    NM_PROJETO = "Projeto",
    INICIO_BOLSA = "Início Bolsa",
    FIM_BOLSA = "Término Bolsa",
    QT_BOLSA_FAPESQ = "Bolsas Concedidas",
    DT_NASCIMENTO_DISCENTE = "Data Nasc. Bolsista",
    EDITAL = Edital,
    VINCULO_INSTITUCIONAL = "Tem Vínculo Institucional",
    VINCULO_EMPREGATICIO = "Tem Vínculo Empregatício",
    COORDENADOR = "Coordenador"
  ) |> 
  dplyr::mutate(EDITAL = "Edital 07/2021",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |> 
  dplyr::mutate(GENERO = case_match(GENERO,
                                    "Female" ~ "FEMININO",
                                    "Male" ~ "MASCULINO")) |> 
  dplyr::mutate(across(
    c(NM_DISCENTE, SG_ENTIDADE_ENSINO, NM_PROJETO, COORDENADOR),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(DS_GRAU_ACADEMICO_DISCENTE = case_match(DS_GRAU_ACADEMICO_DISCENTE,
                                                        "BLD-DRP-Doutorado no país" ~ "DOUTORADO",
                                                        "BLD-MSP-Mestrado no País" ~ "MESTRADO",
                                                        "BLD-PDRP-Pós-Doutorado no País" ~ "POS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )|> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = as.character(NR_DOCUMENTO_DISCENTE))


###### Edital 16/2022 ----
df4 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[4L]]) |>
  dplyr::rename(
    NM_DISCENTE = Bolsista,
    NM_SITUACAO_DISCENTE = Situacao,
    DS_GRAU_ACADEMICO_DISCENTE = "Modalidade/Nível",
    NR_DOCUMENTO_DISCENTE = "CPF Bolsista",
    SG_ENTIDADE_ENSINO = "Instituição",
    INICIO_BOLSA = "Início Bolsa",
    FIM_BOLSA = "Término Bolsa",
    QT_BOLSA_FAPESQ = "Bolsas Concedidas",
    VL_BOLSA_MES = "Valor da Bolsa",
    DT_NASCIMENTO_DISCENTE = "Data Nasc. Bolsista",
    EDITAL = Edital,
    VINCULO_INSTITUCIONAL = "Tem Vínculo Institucional",
    VINCULO_EMPREGATICIO = "Tem Vínculo Empregatício",
    COORDENADOR = "Coordenador"
  ) |> 
  dplyr::mutate(EDITAL = "Edital 16/2022",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |> 
  dplyr::mutate(GENERO = case_match(GENERO,
                                    "Female" ~ "FEMININO",
                                    "Male" ~ "MASCULINO")) |> 
  dplyr::mutate(across(
    c(NM_DISCENTE, SG_ENTIDADE_ENSINO, COORDENADOR),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(DS_GRAU_ACADEMICO_DISCENTE = case_match(DS_GRAU_ACADEMICO_DISCENTE,
                                                        "BLD-DRP-Doutorado no país" ~ "DOUTORADO",
                                                        "BLD-MSP-Mestrado no País" ~ "MESTRADO",
                                                        "BLD-PDRP-Pós-Doutorado no País" ~ "POS-DOUTORADO")) |> 
  select(-`...15`) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )|> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = as.character(NR_DOCUMENTO_DISCENTE))


###### Edital 17/2022 ----
df5 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[5L]]) |>
  dplyr::rename(
    NM_DISCENTE = Bolsista,
    NM_SITUACAO_DISCENTE = Situacao,
    DS_GRAU_ACADEMICO_DISCENTE = "Modalidade/Nível",
    NR_DOCUMENTO_DISCENTE = "CPF Bolsista",
    SG_ENTIDADE_ENSINO = "Instituição",
    INICIO_BOLSA = "Início Bolsa",
    NM_PROJETO = "Projeto",
    FIM_BOLSA = "Término Bolsa",
    VL_BOLSA_MES = "Valor da Bolsa",
    DT_NASCIMENTO_DISCENTE = "Data Nasc. Bolsista",
    EDITAL = Edital,
    VINCULO_INSTITUCIONAL = "Tem Vínculo Institucional",
    VINCULO_EMPREGATICIO = "Tem Vínculo Empregatício",
    COORDENADOR = "Coordenador"
  ) |> 
  dplyr::mutate(EDITAL = "Edital 17/2022",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |> 
  dplyr::mutate(GENERO = case_match(GENERO,
                                    "Female" ~ "FEMININO",
                                    "Male" ~ "MASCULINO")) |> 
  dplyr::mutate(across(
    c(NM_DISCENTE, SG_ENTIDADE_ENSINO, COORDENADOR, NM_PROJETO),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(DS_GRAU_ACADEMICO_DISCENTE = case_match(DS_GRAU_ACADEMICO_DISCENTE,
                                                        "BLD-DRP-Doutorado no país" ~ "DOUTORADO",
                                                        "BLD-MSP-Mestrado no País" ~ "MESTRADO",
                                                        "BLD-PDRP-Pós-Doutorado no País" ~ "POS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )|> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = as.character(NR_DOCUMENTO_DISCENTE))


###### Edital 08/2023 ----
df6 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[6L]]) |>
  dplyr::rename(
    NM_DISCENTE = Bolsista,
    NM_SITUACAO_DISCENTE = Situacao,
    DS_GRAU_ACADEMICO_DISCENTE = "Modalidade/Nível",
    NR_DOCUMENTO_DISCENTE = "CPF Bolsista",
    SG_ENTIDADE_ENSINO = "Instituição",
    INICIO_BOLSA = "Início Bolsa",
    NM_PROJETO = "Projeto",
    FIM_BOLSA = "Término Bolsa",
    VL_BOLSA_MES = "Valor da Bolsa",
    QT_BOLSA_FAPESQ = "Bolsas Concedidas",
    DT_NASCIMENTO_DISCENTE = "Data Nasc. Bolsista",
    EDITAL = Edital,
    VINCULO_INSTITUCIONAL = "Tem Vínculo Institucional",
    VINCULO_EMPREGATICIO = "Tem Vínculo Empregatício",
    COORDENADOR = "Coordenador"
  ) |> 
  dplyr::mutate(EDITAL = "Edital 08/2023",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |> 
  dplyr::mutate(GENERO = case_match(GENERO,
                                    "Female" ~ "FEMININO",
                                    "Male" ~ "MASCULINO")) |> 
  dplyr::mutate(across(
    c(NM_DISCENTE, SG_ENTIDADE_ENSINO, COORDENADOR, NM_PROJETO),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(DS_GRAU_ACADEMICO_DISCENTE = case_match(DS_GRAU_ACADEMICO_DISCENTE,
                                                        "BLD-DRP-Doutorado no país" ~ "DOUTORADO",
                                                        "BLD-MSP-Mestrado no País" ~ "MESTRADO",
                                                        "BLD-PDRP-Pós-Doutorado no País" ~ "POS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )|> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = as.character(NR_DOCUMENTO_DISCENTE))



###### Edital 09/2023 ----
df7 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[7L]]) |>
  dplyr::rename(
    NM_DISCENTE = Bolsista,
    NM_SITUACAO_DISCENTE = Situacao,
    DS_GRAU_ACADEMICO_DISCENTE = "Modalidade/Nível",
    NR_DOCUMENTO_DISCENTE = "CPF Bolsista",
    SG_ENTIDADE_ENSINO = "Instituição",
    INICIO_BOLSA = "Início Bolsa",
    NM_PROJETO = "Projeto",
    FIM_BOLSA = "Término Bolsa",
    VL_BOLSA_MES = "Valor da Bolsa",
    QT_BOLSA_FAPESQ = "Bolsas Concedidas",
    DT_NASCIMENTO_DISCENTE = "Data Nasc. Bolsista",
    EDITAL = Edital,
    VINCULO_INSTITUCIONAL = "Tem Vínculo Institucional",
    VINCULO_EMPREGATICIO = "Tem Vínculo Empregatício",
    COORDENADOR = "Coordenador"
  ) |> 
  dplyr::mutate(EDITAL = "Edital 08/2023",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |> 
  dplyr::mutate(GENERO = case_match(GENERO,
                                    "Female" ~ "FEMININO",
                                    "Male" ~ "MASCULINO")) |> 
  dplyr::mutate(across(
    c(NM_DISCENTE, SG_ENTIDADE_ENSINO, COORDENADOR, NM_PROJETO),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(DS_GRAU_ACADEMICO_DISCENTE = case_match(DS_GRAU_ACADEMICO_DISCENTE,
                                                        "BLD-DRP-Doutorado no país" ~ "DOUTORADO",
                                                        "BLD-MSP-Mestrado no País" ~ "MESTRADO",
                                                        "BLD-PDRP-Pós-Doutorado no País" ~ "POS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )|> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = as.character(NR_DOCUMENTO_DISCENTE))


##### EDITAL 11/2023 ----------
df8 <-
  readxl::read_excel(fapesq_path, sheet = fapesq_sheets[[8L]]) |>
  dplyr::rename(
    NM_DISCENTE = Bolsista,
    NM_SITUACAO_DISCENTE = Situacao,
    DS_GRAU_ACADEMICO_DISCENTE = "Modalidade/Nível",
    NR_DOCUMENTO_DISCENTE = "CPF Bolsista",
    SG_ENTIDADE_ENSINO = "Instituição",
    INICIO_BOLSA = "Início Bolsa",
    FIM_BOLSA = "Término Bolsa",
    EDITAL = Edital,
    COORDENADOR = "Coordenador"
  ) |> 
  dplyr::mutate(EDITAL = "Edital 08/2023",
                GENERO = genderBR::get_gender(NM_DISCENTE)) |> 
  dplyr::mutate(GENERO = case_match(GENERO,
                                    "Female" ~ "FEMININO",
                                    "Male" ~ "MASCULINO")) |> 
  dplyr::mutate(across(
    c(NM_DISCENTE, SG_ENTIDADE_ENSINO, COORDENADOR),
    ~ janitor::make_clean_names(.x, case = "sentence", allow_dupes = TRUE, ascii = TRUE) |> toupper()
  )) |> 
  dplyr::mutate(DS_GRAU_ACADEMICO_DISCENTE = case_match(DS_GRAU_ACADEMICO_DISCENTE,
                                                        "BLD-DRP-Doutorado no país" ~ "DOUTORADO",
                                                        "BLD-MSP-Mestrado no País" ~ "MESTRADO",
                                                        "BLD-PDRP-Pós-Doutorado no País" ~ "POS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )|> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = as.character(NR_DOCUMENTO_DISCENTE))






# UNIR BASES ----------

editais_fapesq <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8) |> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = str_replace_all(NR_DOCUMENTO_DISCENTE, "[.]", "")) |> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = str_replace_all(NR_DOCUMENTO_DISCENTE, "-", "")) |> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = ifelse(is.na(NR_DOCUMENTO_DISCENTE), NA, 
                                               paste0("***.", 
                                                      substr(NR_DOCUMENTO_DISCENTE, 4, 6),
                                                      "." ,
                                                      substr(NR_DOCUMENTO_DISCENTE, 7, 9),
                                                      "-**"))) |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO_NM = str_extract(NM_DISCENTE, "^[^ ]+")) |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO_SEGUNDO_NM = str_extract(NM_DISCENTE, "^[^ ]+ [^ ]+")) |> 
  dplyr::mutate(SG_ENTIDADE_ENSINO = case_match(SG_ENTIDADE_ENSINO,
                                                "UFPB CAMPUS III" ~ "UFPB",
                                                "UFCG CSTR" ~ "UFCG",
                                                "UFCG CUITE" ~ "UFCG",
                                                "UFPB CCA" ~ "UFPB",
                                                "CCA" ~ "UFPB",
                                                .default = as.character(SG_ENTIDADE_ENSINO))) |> 
  dplyr::mutate(
    QT_BOLSA_FAPESQ = ifelse(is.na(QT_BOLSA_FAPESQ),
                             lubridate::interval(ymd(INICIO_BOLSA), ymd(FIM_BOLSA)) %/% months(1),
                             QT_BOLSA_FAPESQ)
  ) |> 
  dplyr::mutate(
    VL_BOLSA_MES = case_when(
      EDITAL == "Edital 07/2021" & DS_GRAU_ACADEMICO_DISCENTE == "MESTRADO" ~ 1500,
      EDITAL == "Edital 07/2021" & DS_GRAU_ACADEMICO_DISCENTE == "DOUTORADO" ~ 2200,
      EDITAL == "Edital 07/2021" & DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO" ~ 4100,
      EDITAL == "Edital 03/2016" & DS_GRAU_ACADEMICO_DISCENTE == "MESTRADO" ~ 400,
      EDITAL == "Edital 03/2016" & DS_GRAU_ACADEMICO_DISCENTE == "DOUTORADO" ~ 500,
      EDITAL == "Edital 03/2016" & DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO" ~ 4100,
      EDITAL == "Edital 07/2018" & DS_GRAU_ACADEMICO_DISCENTE == "MESTRADO" ~ 400,
      EDITAL == "Edital 07/2018" & DS_GRAU_ACADEMICO_DISCENTE == "DOUTORADO" ~ 500,
      EDITAL == "Edital 07/2018" & DS_GRAU_ACADEMICO_DISCENTE == "POS-DOUTORADO" ~ 4100,
      TRUE ~ VL_BOLSA_MES
    )
  )
  
##### Valor das bolsas fapesq 
# https://fapesq.rpp.br/editais/editais-encerrados/edital-07-2021-bolsas-fapesq-retificado-06-07-2021.pdf/view
# 

editais_fapesq |> readr::write_rds("dados/tidy/editais_fapesq.rds")
editais_fapesq |> readr::write_csv("dados/tidy/editais_fapesq.csv")

editais_fapesq |> count(lubridate::year(INICIO_BOLSA))

## Produção Intelectual da Paraíba ----

# producao_artigos_periodicos <- read_rds(paste0(PATH_DADOS, "producao_artigos_periodicos.rds"))
# autor_producao_periodicos <- read_rds(paste0(PATH_DADOS, "autor_producao_periodicos.rds"))

INSTITUICOES_PB <- c("UFPB/AREIA", "UFPB/J.P.", "IFPB", "UFPB/RT", "UEPB", "UFPB-JP", "UFPB-RT", "UFPB", "UFCG")

artigos_autor_pb <- producao_artigos_periodicos |> 
  dplyr::filter(SG_ENTIDADE_ENSINO %in% INSTITUICOES_PB) |> 
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
                              ID_PESSOA_PART_EXTERNO)) |> 
  dplyr::mutate(
    NM_AUTOR = stringr::str_to_upper(
      janitor::make_clean_names(NM_AUTOR, case = "sentence", allow_dupes = TRUE)
    )
  ) 

artigos_autor_pb |> write_rds("dados/tidy/artigos_autor_pb.rds")




