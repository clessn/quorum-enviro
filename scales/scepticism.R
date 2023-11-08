###***************###
# Packages ####
###**************###
library(tidyverse)
library(GGally)

###**************###
# Functions ####
###**************###
source("functions.R", encoding = "UTF-8")

###**************###
# Load Data ####
###**************###
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

###**************###
# Last cleaning ####
###**************###
## 0 = pas sceptique, 1 = scepticisme 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleScep_ prefix

table(Data$gravity_whenWillHarmCanadians)
Data$scaleScep_whenWillHarmCanadians <- finverser(Data$gravity_whenWillHarmCanadians)
table(Data$scaleScep_whenWillHarmCanadians)

table(Data$gravity_crisisIsExaggerated)
Data$scaleScep_crisisIsExaggerated <- Data$gravity_crisisIsExaggerated
table(Data$scaleScep_crisisIsExaggerated)

table(Data$science_trustScientists)
Data$scaleScep_trustScientists <- finverser(Data$science_trustScientists)
table(Data$scaleScep_trustScientists)

table(Data$science_consensusClimateChange)
Data$scaleScep_consensusClimateChange <- finverser(Data$science_consensusClimateChange)
table(Data$scaleScep_consensusClimateChange)

table(Data$science_carContributeClimateChange)
Data$scaleScep_carContribute <- finverser(Data$science_carContributeClimateChange)
table(Data$scaleScep_carContribute)

table(Data$science_scientistsExaggerateClimateChangeEvidence)
Data$scaleScep_scientistsExaggerate <- Data$science_scientistsExaggerateClimateChangeEvidence
table(Data$scaleScep_scientistsExaggerate)

# human causal effect
table(Data$science_climateChangeMainCause_naturalProcesses)
table(Data$science_climateChangeMainCause_natureAndHumans)
table(Data$science_climateChangeMainCause_humanActivities)
Data$scaleScep_natureCauseClimateChange <- NA
Data$scaleScep_natureCauseClimateChange[Data$science_climateChangeMainCause_humanActivities == 1] <- 0
Data$scaleScep_natureCauseClimateChange[Data$science_climateChangeMainCause_naturalProcesses == 1] <- 1
Data$scaleScep_natureCauseClimateChange[Data$science_climateChangeMainCause_natureAndHumans == 1] <- 1
table(Data$scaleScep_natureCauseClimateChange)

###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- Data %>% 
  select(starts_with("scaleScep")) %>% 
  drop_na()

topdown_fa(FaData)
topdown_fa(FaData, nfactors = 2)

###**************###
## 1.2 Check distribution of scale and variables ####
###**************###

Data2 <- Data %>% 
  mutate(scaleScep = (Data %>%
                        select(starts_with("scaleScep_")) %>%
                        rowSums())/length(names(Data %>% select(starts_with("scaleScep_")))))

hist(Data2$scaleScep)
plot(cumsum(vector_prop_table(Data2$scaleScep)))
cumsum(vector_prop_table(Data2$scaleScep))
plot(vector_prop_table(Data2$scaleScep))
plot(table(Data2$scaleScep))

plot(vector_prop_table(log(Data2$scaleScep)))
hist(log(Data2$scaleScep))

names(FaData) <- gsub("scaleScep_", "", names(FaData))
ggpairs(FaData,
        diag = list(continuous=wrap("barDiag",
                                    binwidth=0.25,
                                    fill = "blue",
                                    color = "blue",
                                    alpha = 0.6)),
        lower = list(continuous=ggpairs_jitter_with_smooth))

###**************###
# Binariser chaque variable ####
###**************###
## Lorsque la prop cumulative de la var atteint 50%
##  on met 1

vector_prop_table(Data$scaleScep_whenWillHarmCanadians)
cumsum(vector_prop_table(Data$scaleScep_whenWillHarmCanadians))
Data$scaleScep2_whenWillHarmCanadians <- NA
Data$scaleScep2_whenWillHarmCanadians[Data$scaleScep_whenWillHarmCanadians <= 0.2] <- 0
Data$scaleScep2_whenWillHarmCanadians[Data$scaleScep_whenWillHarmCanadians > 0.2] <- 1
table(Data$scaleScep2_whenWillHarmCanadians)

cumsum(vector_prop_table(Data$scaleScep_crisisIsExaggerated))
Data$scaleScep2_crisisIsExaggerated <- NA
Data$scaleScep2_crisisIsExaggerated[Data$scaleScep_crisisIsExaggerated <= 0.25] <- 0
Data$scaleScep2_crisisIsExaggerated[Data$scaleScep_crisisIsExaggerated > 0.25] <- 1
table(Data$scaleScep2_crisisIsExaggerated)

cumsum(vector_prop_table(Data$scaleScep_trustScientists))
Data$scaleScep2_trustScientists <- NA
Data$scaleScep2_trustScientists[Data$scaleScep_trustScientists == 0] <- 0
Data$scaleScep2_trustScientists[Data$scaleScep_trustScientists > 0] <- 1
table(Data$scaleScep2_trustScientists)

cumsum(vector_prop_table(Data$scaleScep_consensusClimateChange))
Data$scaleScep2_consensusClimateChange <- NA
Data$scaleScep2_consensusClimateChange[Data$scaleScep_consensusClimateChange <= 0.25] <- 0
Data$scaleScep2_consensusClimateChange[Data$scaleScep_consensusClimateChange > 0.25] <- 1
table(Data$scaleScep2_consensusClimateChange)

cumsum(vector_prop_table(Data$scaleScep_carContribute))
Data$scaleScep2_carContribute <- NA
Data$scaleScep2_carContribute[Data$scaleScep_carContribute <= 0.25] <- 0
Data$scaleScep2_carContribute[Data$scaleScep_carContribute > 0.25] <- 1
table(Data$scaleScep2_carContribute)

cumsum(vector_prop_table(Data$scaleScep_scientistsExaggerate))
Data$scaleScep2_scientistsExaggerate <- NA
Data$scaleScep2_scientistsExaggerate[Data$scaleScep_scientistsExaggerate <= 0.25] <- 0
Data$scaleScep2_scientistsExaggerate[Data$scaleScep_scientistsExaggerate > 0.25] <- 1
table(Data$scaleScep2_scientistsExaggerate)

table(Data$scaleScep_natureCauseClimateChange)
Data$scaleScep2_natureCauseClimateChange <- Data$scaleScep_natureCauseClimateChange
table(Data$scaleScep2_natureCauseClimateChange)

###**************###
# 2. second try ####
###**************###


###**************###
## 2.1 Factor analysis ####
###**************###
FaData <- Data %>% 
  select(starts_with("scaleScep2")) %>% 
  drop_na()

topdown_fa(FaData)
topdown_fa(FaData, nfactors = 2)

###**************###
## 2.2 Check distribution of scale and variables ####
###**************###

Data2 <- Data2 %>% 
  mutate(scaleScep2 = (Data %>%
                        select(starts_with("scaleScep2_")) %>%
                        rowSums())/length(names(Data %>% select(starts_with("scaleScep2_")))))

hist(Data2$scaleScep2)
plot(cumsum(vector_prop_table(Data2$scaleScep2)))
plot(vector_prop_table(Data2$scaleScep2))

names(FaData) <- gsub("scaleScep2_", "", names(FaData))
ggpairs(FaData,
        diag = list(continuous=wrap("barDiag",
                                    binwidth=0.5,
                                    fill = "blue",
                                    color = "blue",
                                    alpha = 0.6)),
        lower = list(continuous=ggpairs_jitter_with_smooth))

###**************###
## 2.3 ACP  ####
###**************###
acp_plots(FaData)

### Rien qui vaut la peine d'être exploré.

###**************###
# Standardiser première échelle ####
###**************###

Data$scaleScep <- Data2$scaleScep
Data$scaleScep[Data$scaleScep>= 0.75] <- 0.75
Data$scaleScep <- minmaxNormalization(Data$scaleScep)
hist(Data$scaleScep)
plot(density(Data$scaleScep, na.rm = T))


