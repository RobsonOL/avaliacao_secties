# 1. PACKAGES -----------
rm(list = ls())
pacman::p_load(tidyverse, janitor, readr, tidyr, skimr, vroom, genderBR)

# FAPESQ ----
fapesq_path <- "dados/bruto/Informações editais de bolsas de mestrado, doutorado e pós-doutorado - FAPESQ.xlsx"
fapesq_sheets <- readxl::excel_sheets(fapesq_path)

# DADOS ----

## Edital 0320216 ----

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

## Edital 072018 ----
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

  
                
## Edital 072021 ----
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
    QT_MESES_BOLSA = "Bolsas Concedidas",
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
                                      "BLD-PDRP-Pós-Doutorado no País" ~ "PÓS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )


## Edital 16/2022 ----
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
    QT_MESES_BOLSA = "Bolsas Concedidas",
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
                                      "BLD-PDRP-Pós-Doutorado no País" ~ "PÓS-DOUTORADO")) |> 
  select(-`...15`) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )


## Edital 17/2022 ----
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
                                      "BLD-PDRP-Pós-Doutorado no País" ~ "PÓS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )


## Edital 08/2023 ----
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
    QT_MESES_BOLSA = "Bolsas Concedidas",
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
                                      "BLD-PDRP-Pós-Doutorado no País" ~ "PÓS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )



## Edital 09/2023 ----
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
    QT_MESES_BOLSA = "Bolsas Concedidas",
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
                                      "BLD-PDRP-Pós-Doutorado no País" ~ "PÓS-DOUTORADO")) |> 
  dplyr::mutate(
    TIPO_BOLSA_MAIS_COMUM = "FAPESQ - EDITAL"
  )




editais_fapesq <- bind_rows(df1, df2, df3, df4, df5, df6, df7) |> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = str_replace(NR_DOCUMENTO_DISCENTE, ".", "")) |> 
  dplyr::mutate(NR_DOCUMENTO_DISCENTE = ifelse(is.na(NR_DOCUMENTO_DISCENTE), NA, 
                                               paste0("***.", 
                                                      substr(NR_DOCUMENTO_DISCENTE, 4, 6),
                                                      "." ,
                                                      substr(NR_DOCUMENTO_DISCENTE, 7, 9),
                                                      "-**"))) |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO = str_extract(NM_DISCENTE, "^[^ ]+")) |> 
  dplyr::mutate(SG_ENTIDADE_ENSINO = case_match(SG_ENTIDADE_ENSINO,
                                                "UFPB CAMPUS III" ~ "UFPB",
                                                "UFCG CSTR" ~ "UFCG",
                                                "UFCG CUITE" ~ "UFCG",
                                                "UFPB CCA" ~ "UFPB",
                                                "CCA" ~ "UFPB",
                                                .default = as.character(SG_ENTIDADE_ENSINO)))
                                                



skimr::skim(editais_fapesq)



editais_fapesq |> readr::write_rds("dados/tidy/editais_fapesq.rds")
editais_fapesq |> readr::write_csv("dados/tidy/editais_fapesq.csv")


## Joining with the main dataset ---- 

df <- read_rds("dados/tidy/discentes_bolsa_tese_pub.rds") |> 
  dplyr::mutate(NM_DISCENTE_PRIMEIRO = str_extract(NM_DISCENTE, "^[^ ]+")) 


df |> 
  left_join(editais_fapesq, by = c("NM_DISCENTE", "SG_ENTIDADE_ENSINO", "DS_GRAU_ACADEMICO_DISCENTE")) |> 
  filter(NM_DISCENTE %in% c(editais_fapesq$NM_DISCENTE))
