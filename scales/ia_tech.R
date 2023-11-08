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
## 0 = non-technophile, 1 = technophile  
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleIA_ prefix

table(Data$science_AIToolAgainstClimateChange)
Data$scaleIA_AIToolAgainstClimateChange <- Data$science_AIToolAgainstClimateChange
table(Data$scaleIA_AIToolAgainstClimateChange)

table(Data$science_techClimateChangeSolution)
Data$scaleIA_techClimateChangeSolution <- Data$science_techClimateChangeSolution
table(Data$scaleIA_techClimateChangeSolution)

table(Data$science_climateChangeTechnoEffortsNull)
Data$scaleIA_climateChangeTechnoEffortsNull <- Data$science_climateChangeTechnoEffortsNull
table(Data$scaleIA_climateChangeTechnoEffortsNull)

table(Data$science_techGoodForHumanity)
Data$scaleIA_techGoodForHumanity <- Data$science_techGoodForHumanity
table(Data$scaleIA_techGoodForHumanity)

###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- Data %>% 
  select(starts_with("scaleIA")) %>% 
  drop_na()

topdown_fa(FaData)

FaData2 <- FaData %>% select(-scaleIA_climateChangeTechnoEffortsNull)

topdown_fa(FaData2)

###**************###
## 1.2 Check distribution of scale ####
###**************###

FaData2$scaleIA <- rowSums(FaData2) 
hist(FaData2$scaleIA)
FaData2$scaleIA <- minmaxNormalization(FaData2$scaleIA)
hist(FaData2$scaleIA)

