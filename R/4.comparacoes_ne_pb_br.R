# Comparar PB, NE, BRASIL -------

# Carregar pacotes
pacman::p_load(tidyverse, janitor)

# DADOS ------
# PATH_DADOS <- "C:/Users/robso/OneDrive/Avaliação SECTIES/bruto/"
# PATH_DADOS <- "dados/bruto"

discentes <- readr::read_rds(paste0(PATH_DADOS, "discentes.rds"))

estados_nordeste <- c("PB", "PE", "AL", "SE", "BA", "RN", "CE", "PI", "MA")

discentes_brasil <- discentes |>
  mutate(across(
    c(AN_BASE, CD_AREA_AVALIACAO, AN_NASCIMENTO_DISCENTE,
      # AN_MATRICULA_DISCENTE, ME_MATRICULA_DISCENTE,
      ID_PESSOA),
    as.numeric
  )) |> 
  # filter(SG_UF_PROGRAMA == "PB") |>
  mutate(across(c(DT_MATRICULA_DISCENTE), parse_date_time, "%d%b%y:%H:%M:%S")) |>
  mutate(across(c(DT_SITUACAO_DISCENTE), parse_date_time, "%d%b%y:%H:%M:%S")) |>
  mutate(IDADE = AN_BASE - AN_NASCIMENTO_DISCENTE) |>
  mutate(across(where(is.character), ~ na_if(., "NI"))) |>
  rename(ANO = AN_BASE) |>
  relocate(IDADE, .after = AN_NASCIMENTO_DISCENTE) |>
  mutate(
    SG_ENTIDADE_ENSINO = case_match(
      SG_ENTIDADE_ENSINO,
      "UFPB/J.P." ~ "UFPB-JP",
      "UFPB/RT" ~ "UFPB-RT",
      "UFPB/AREIA" ~ "UFPB-AREIA",
      .default = SG_ENTIDADE_ENSINO
    )
  ) |>
  dplyr::mutate(NM_DISCENTE = stringr::str_to_upper(
    janitor::make_clean_names(NM_DISCENTE, case = "sentence", allow_dupes = TRUE)
  )) |> 
  dplyr::mutate(REGIAO = case_match(SG_UF_PROGRAMA,
    "PB" ~ "NE",
    "PE" ~ "NE",
    "AL" ~ "NE",
    "SE" ~ "NE",
    "BA" ~ "NE",
    "RN" ~ "NE",
    "CE" ~ "NE",
    "PI" ~ "NE",
    "MA" ~ "NE",
    "SP" ~ "SE",
    "RJ" ~ "SE",
    "MG" ~ "SE",
    "ES" ~ "SE",
    "RS" ~ "S",
    "SC" ~ "S",
    "PR" ~ "S",
    "MS" ~ "CO",
    "MT" ~ "CO",
    "GO" ~ "CO",
    "DF" ~ "CO",
    "AM" ~ "N",
    "RR" ~ "N",
    "AP" ~ "N",
    "PA" ~ "N",
    "TO" ~ "N",
    "AC" ~ "N",
    "RO" ~ "N",
    .default = "BRASIL"
  ))

discentes |> group_by(is.na(ID_PESSOA)) |> tally() 
