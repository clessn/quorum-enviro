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
CleanData <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")
names(CleanData)

###**************###
# Last cleaning ####
###**************###
## 0 = fait moins d'actions pour l'environnement, 1 = fait des actions pour l'environnement 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleBehaviour_ prefix

# responsability_effortsClimateChangeFamily
table(CleanData$responsability_effortsClimateChangeFamily)
CleanData$scaleBehaviour_effortsClimateChangeFamily <- CleanData$responsability_effortsClimateChangeFamily
table(CleanData$scaleBehaviour_effortsClimateChangeFamily)

# responsability_climateChangeDiscuss
table(CleanData$responsability_climateChangeDiscuss)
CleanData$scaleBehaviour_climateChangeDiscuss <- CleanData$responsability_climateChangeDiscuss
table(CleanData$scaleBehaviour_climateChangeDiscuss)

# electricVehicule
table(CleanData$electricVehicule)
CleanData$scaleBehaviour_electricVehicule <- CleanData$electricVehicule
table(CleanData$scaleBehaviour_electricVehicule)

# responsability_individualToProtect
table(CleanData$responsability_individualToProtect)
CleanData$scaleBehaviour_individualToProtect <- CleanData$responsability_individualToProtect
table(CleanData$scaleBehaviour_individualToProtect)

###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- CleanData %>% 
  select(starts_with("scaleBehaviour")) %>% 
  drop_na()

topdown_fa(FaData)

FaData2 <- FaData %>% 
  select(-scaleBehaviour_electricVehicule)

topdown_fa(FaData2)


###**************###
## 1.2 Check distribution of scale and variables ####
###**************###

Data2 <- Data %>% 
  mutate(scaleGravity = (Data %>%
                        select(starts_with("scaleGravity_")) %>%
                        rowSums())/length(names(Data %>% select(starts_with("scaleGravity_")))))

hist(Data2$scaleGravity)
#plot(cumsum(vector_prop_table(Data2$scaleGravity)))
#cumsum(vector_prop_table(Data2$scaleGravity))
#plot(vector_prop_table(Data2$scaleGravity))
#plot(table(Data2$scaleGravity))
#plot(table(log(Data2$scaleGravity)))

#plot(vector_prop_table(log(Data2$scaleGravity)))
#hist(log(Data2$scaleGravity))

names(FaData) <- gsub("scaleGravity_", "", names(FaData))
ggpairs(FaData,
        diag = list(continuous=wrap("barDiag",
                                    binwidth=0.25,
                                    fill = "blue",
                                    color = "blue",
                                    alpha = 0.6)),
        lower = list(continuous=ggpairs_jitter_with_smooth))

