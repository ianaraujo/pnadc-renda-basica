# install.packages("tidyverse")
# install.packages("PNADcIBGE")

library(tidyverse)
library(PNADcIBGE)

pnadc <- get_pnadc(year = 2020, quarter = 4, design = FALSE)

pnadc_rio <- pnadc %>%
  filter(UF == "Rio de Janeiro" & V1023 == "Capital")

# Médias de pessoas por domicílio 

pnadc_rio %>%
  summarise(media_p_domicilio = mean(V2001))

