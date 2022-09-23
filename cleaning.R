# Packages ####
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Note: cleaning of some variables have been done in other files in this repo
####### apsa.R
####### popcorn_22-09-22.R
####### violence_pol.R


# Data ####
Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav") %>% 
  filter(PROV == 11) %>% 
  mutate(id = 1:nrow(.))


ForNadj <- data.frame(id = Data$id,
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
