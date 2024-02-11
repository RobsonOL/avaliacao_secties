reprex::reprex({
  library(readr, dplyr)
  df_discentes |> write_rds("dados/tidy/df_discentes.rds")

  df_discentes |> 
    dplyr::group_by(GENERO) |> 
    dplyr::count()
  }, wd = ".")


