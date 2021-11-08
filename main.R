# install.packages("tidyverse")
# install.packages("PNADcIBGE")
# install.packages("survey")
# install.packages("srvyr")

library(tidyverse)
library(PNADcIBGE)
library(survey)
library(srvyr)

# Downloads PNADc data from IBGE

pnadc <- get_pnadc(year = 2021, quarter = 2, 
                   interview = NULL, vars = NULL, labels = F, design = F)

pnadc_plano_amostral <- survey::svydesign(ids = ~ UPA, strata = ~ Estrato, 
                                          weights = ~ V1027, data = pnadc, nest = TRUE)

frequencias_pop <- data.frame(posest = unique(pnadc$posest), n = unique(pnadc$V1029))

pnadc_calibrado <- survey::postStratify(pnadc_plano_amostral, 
                                        strata = ~ posest, population = frequencias_pop)

pnadc_calibrado <- as_survey_design(pnadc_calibrado)

pnadc_calibrado <- update(pnadc_calibrado, 
                          VD4019n = ifelse(V2009 >= 14 & VD4015 == 1, VD4019, NA),
                          VD4020n = ifelse(V2009 >= 14 & VD4015 == 1, VD4020, NA))

pnadc_calibrado <- pnadc_calibrado %>%
  mutate(rb1 = VD4019n + 250) %>%
  mutate(rb2 = VD4019n + 500)

rend_tot<-svymean( ~ VD4019n + rb1 + rb2, 
                   subset(pnadc_calibrado, VD4002 %in% 1) ,
                   na.rm = TRUE ) 

x <- as.data.frame(rend_tot) %>%
  rownames_to_column("type")

ggplot(x, aes(x = type, y = mean)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width = .1)


rend_quantil <- svyquantile( ~ VD4020n, 
                             subset(pnadc_calibrado, VD4002 %in% 1), 
                             seq(.1, 0.9, .1), 
                             na.rm = TRUE )

y <- rend_quantil$VD4020n %>%
  as.data.frame() %>%
  rownames_to_column()

ggplot(y, aes(x = rowname, y = quantile)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = quantile - se, ymax = quantile + se), width = .5)


# ... 

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

pnadc_rio <- pnadc %>% 
  dplyr::filter(UF == 33 & V1023 == 1)

# Simulações ----

source(file = "simulador.R")

simulador(file = "pnadc2021.csv", valor = 400)




