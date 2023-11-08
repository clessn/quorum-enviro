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

###**************###
# Last cleaning ####
###**************###
## 0 = no support, 1 = support
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleStick_ prefix

# stateInterv_makeGasMoreExpensive
table(CleanData$stateInterv_makeGasMoreExpensive)
CleanData$scaleStick_makeGasMoreExpensive <- CleanData$stateInterv_makeGasMoreExpensive
table(CleanData$scaleStick_makeGasMoreExpensive)

# stateInterv_moreRegulationsEnviro
table(CleanData$stateInterv_moreRegulationsEnviro)
CleanData$scaleStick_moreRegulationsEnviro <- CleanData$stateInterv_moreRegulationsEnviro
table(CleanData$scaleStick_moreRegulationsEnviro)

# stateInterv_continueIncreaseGESPrice
table(CleanData$stateInterv_continueIncreaseGESPrice)
CleanData$scaleStick_continueIncreaseGESPrice <- CleanData$stateInterv_continueIncreaseGESPrice
table(CleanData$scaleStick_continueIncreaseGESPrice)

# stateInterv_decreaseFossilProd
table(CleanData$stateInterv_decreaseFossilProd)
CleanData$scaleStick_decreaseFossilProd <- CleanData$stateInterv_decreaseFossilProd
table(CleanData$scaleStick_decreaseFossilProd)


###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- CleanData %>% 
  select(starts_with("scaleStick")) %>% 
  drop_na()

topdown_fa(FaData)

###**************###
## 1.2 Check distribution of scale ####
###**************###

FaData$scaleStick <- rowSums(FaData)/ncol(FaData)
hist(FaData$scaleStick)

