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
names(Data)

###**************###
# Last cleaning ####
###**************###
## 0 = pas inquiet, situation pas grave, 1 = inquiet, situation grave 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleScep_ prefix

# climateChangePersonalMenace
table(Data$gravity_climateChangePersonalMenace)
Data$scaleGravity_climateChangePersonalMenace <- Data$gravity_climateChangePersonalMenace
table(Data$scaleGravity_climateChangePersonalMenace)

# worriedClimateChange
table(Data$gravity_worriedClimateChange)
Data$scaleGravity_worriedClimateChange <- Data$gravity_worriedClimateChange
table(Data$scaleGravity_worriedClimateChange)

# majorCatastrophe
table(Data$gravity_majorCatastrophe)
Data$scaleGravity_majorCatastrophe <- Data$gravity_majorCatastrophe
table(Data$scaleGravity_majorCatastrophe)

# climateChangeEndHumanity
table(Data$gravity_climateChangeEndHumanity)
Data$scaleGravity_climateChangeEndHumanity <- Data$gravity_climateChangeEndHumanity
table(Data$scaleGravity_climateChangeEndHumanity)

###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- Data %>% 
  select(starts_with("scaleGravity")) %>% 
  drop_na()

topdown_fa(FaData)

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

