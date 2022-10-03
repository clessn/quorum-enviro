# Packages ####
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Note: cleaning of some variables have been done in other files in this repo
####### apsa.R
####### popcorn_22-09-22.R
####### violence_pol.R


# Data ####
Data <- haven::read_sav("_SharedFolder_transition/data/ULA011-données.Sav") %>% 
  filter(PROV == 11) %>% 
  mutate(id = 1:nrow(.))


ForNadj <- data.frame(id = Data$id,
                      postal_code = Data$CP,
                      age = 2022-Data$Q7)

CleanData <- data.frame(id = Data$id,
                        postal_code = Data$CP,
                        age = 2022-Data$Q7)

# Gender ####
table(Data$SEXE)
ForNadj$male <- NA
ForNadj$male[Data$SEXE==1] <- 1
ForNadj$male[Data$SEXE!=1] <- 0
table(ForNadj$male)

# Educ ####
table(Data$Q8)
ForNadj$educBHS <- NA
ForNadj$educBHS[Data$Q8 <= 3] <- 1
ForNadj$educBHS[Data$Q8 > 3] <- 0
table(ForNadj$educBHS)

table(Data$Q8)
ForNadj$educCollege <- NA
ForNadj$educCollege[Data$Q8 %in% c(4,6)] <- 1
ForNadj$educCollege[!(Data$Q8 %in% c(4,6))] <- 0
table(ForNadj$educCollege)

# Income ####
table(Data$Q10)
ForNadj$incomeLow <- NA
ForNadj$incomeLow[Data$Q10 <= 2] <- 1
ForNadj$incomeLow[Data$Q10 > 2] <- 0
table(ForNadj$incomeLow)

table(Data$Q10)
ForNadj$incomeMid <- NA
ForNadj$incomeMid[Data$Q10 %in% 3:5] <- 1
ForNadj$incomeMid[!(Data$Q10 %in% 3:5)] <- 0
table(ForNadj$incomeMid)

table(Data$Q10)
ForNadj$incomeHigh <- NA
ForNadj$incomeHigh[Data$Q10 >= 6] <- 1
ForNadj$incomeHigh[Data$Q10 < 6] <- 0
table(ForNadj$incomeHigh)

# Langue ####
table(Data$Q2)
ForNadj$langFr <- NA
ForNadj$langFr[Data$Q2 == 1] <- 1
ForNadj$langFr[Data$Q2 != 1] <- 0
table(ForNadj$langFr)

# Immigrant ####
table(Data$Q4)
ForNadj$immigrant <- NA
ForNadj$immigrant[Data$Q4 == 1] <- 1
ForNadj$immigrant[Data$Q4 != 1] <- 0
table(ForNadj$immigrant)

# Propriétaire ####
table(Data$Q14)
ForNadj$proprio <- NA
ForNadj$proprio[Data$Q14 == 2] <- 1
ForNadj$proprio[Data$Q14 != 2] <- 0
table(ForNadj$proprio)

saveRDS(ForNadj, "_SharedFolder_quorum-enviro/data/cleanData/data.rds")


# Question 76 ####

#  Il y a beaucoup d’informations qui circulent à propos de l’origine du coronavirus, le 
#virus qui provoque la COVID-19. À votre avis, le coronavirus est:

table(Data$Q76A)
CleanData$covidOriginByChinaA[Data$Q76A == 1 | Data$Q76A == 3] <- 0 # N'est pas établie ou ne sait pas
CleanData$covidOriginByChinaA[Data$Q76A == 2] <- 1 # Chine
table(CleanData$covidOriginByChinaA)

table(Data$Q76A)
CleanData$covidOriginNotKnownA[Data$Q76A == 2 | Data$Q76A == 3] <- 0 #Chine ou ne sait pas
CleanData$covidOriginNotKnownA[Data$Q76A == 1] <- 1 # n'est pas établie
table(CleanData$covidOriginNotKnownA)


table(Data$Q76B)
CleanData$covidOriginByChinaB[Data$Q76B == 1] <- 0 # N'est pas établie
CleanData$covidOriginByChinaB[Data$Q76B == 2] <- 1 # Chine
table(CleanData$covidOriginByChinaB)

table(Data$Q76C)
CleanData$covidOriginByChinaC[Data$Q76C == 1] <- 1 # Très certainement vrai
CleanData$covidOriginByChinaC[Data$Q76C == 2] <- 0.67 # Probablement vrai
CleanData$covidOriginByChinaC[Data$Q76C == 3] <- 0.33 # Probablement faux
CleanData$covidOriginByChinaC[Data$Q76C == 4] <- 0 # Très certainement faux
table(CleanData$covidOriginByChinaC)

table(Data$Q76D)
CleanData$covidOriginByChinaD[Data$Q76D == 1] <- 1 # Très certainement vrai
CleanData$covidOriginByChinaD[Data$Q76D == 2] <- 0.75 # Probablement vrai
CleanData$covidOriginByChinaD[Data$Q76D == 5] <- 0.50 # Ne sait pas
CleanData$covidOriginByChinaD[Data$Q76D == 3] <- 0.25 # Probablement faux
CleanData$covidOriginByChinaD[Data$Q76D == 4] <- 0 # Très certainement faux
table(CleanData$covidOriginByChinaD)

