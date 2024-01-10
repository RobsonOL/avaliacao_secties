library(vroom)
library(knitr)
metadados <- read_csv2("dados/metadados.csv", locale = locale(encoding = 'latin5'))

metadados |> kable(format = "markdown")         
