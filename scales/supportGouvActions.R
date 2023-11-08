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
### Add them as a new variable with the scaleGouvActions_ prefix

# stateInterv_moreBikePathPubTransport
table(CleanData$stateInterv_moreBikePathPubTransport)
CleanData$scaleGouvActions_moreBikePathPubTransport <- CleanData$stateInterv_moreBikePathPubTransport
table(CleanData$scaleGouvActions_moreBikePathPubTransport)

# stateInterv_electricCarTaxRefund
table(CleanData$stateInterv_electricCarTaxRefund)
CleanData$scaleGouvActions_electricCarTaxRefund <- CleanData$stateInterv_electricCarTaxRefund
table(CleanData$scaleGouvActions_electricCarTaxRefund)

# stateInterv_continueIncreaseGESPrice_FossilJobs
table(CleanData$stateInterv_continueIncreaseGESPrice_FossilJobs)
CleanData$scaleGouvActions_continueIncreaseGESPrice_FossilJobs <- CleanData$stateInterv_continueIncreaseGESPrice_FossilJobs
table(CleanData$scaleGouvActions_continueIncreaseGESPrice_FossilJobs)

# stateInterv_continueIncreaseGESPrice_GreenJobs
table(CleanData$stateInterv_continueIncreaseGESPrice_GreenJobs)
CleanData$scaleGouvActions_continueIncreaseGESPrice_GreenJobs <- CleanData$stateInterv_continueIncreaseGESPrice_GreenJobs
table(CleanData$scaleGouvActions_continueIncreaseGESPrice_GreenJobs)

# stateInterv_continueIncreaseGESPrice_RedistributedPop
table(CleanData$stateInterv_continueIncreaseGESPrice_RedistributedPop)
CleanData$scaleGouvActions_continueIncreaseGESPrice_RedistributedPop <- CleanData$stateInterv_continueIncreaseGESPrice_RedistributedPop
table(CleanData$scaleGouvActions_continueIncreaseGESPrice_RedistributedPop)

# stateInterv_decreaseFossilProd_GreenJobs
table(CleanData$stateInterv_decreaseFossilProd_GreenJobs)
CleanData$scaleGouvActions_decreaseFossilProd_GreenJobs <- CleanData$stateInterv_decreaseFossilProd_GreenJobs
table(CleanData$scaleGouvActions_decreaseFossilProd_GreenJobs)

# stateInterv_decreaseFossilProd_FossilJobs
table(CleanData$stateInterv_decreaseFossilProd_FossilJobs)
CleanData$scaleGouvActions_decreaseFossilProd_FossilJobs <- CleanData$stateInterv_decreaseFossilProd_FossilJobs
table(CleanData$scaleGouvActions_decreaseFossilProd_FossilJobs)

# stateInterv_makeGasMoreExpensive
table(CleanData$stateInterv_makeGasMoreExpensive)
CleanData$scaleGouvActions_makeGasMoreExpensive <- CleanData$stateInterv_makeGasMoreExpensive
table(CleanData$scaleGouvActions_makeGasMoreExpensive)

# stateInterv_moreRegulationsEnviro
table(CleanData$stateInterv_moreRegulationsEnviro)
CleanData$scaleGouvActions_moreRegulationsEnviro <- CleanData$stateInterv_moreRegulationsEnviro
table(CleanData$scaleGouvActions_moreRegulationsEnviro)

# stateInterv_continueIncreaseGESPrice
table(CleanData$stateInterv_continueIncreaseGESPrice)
CleanData$scaleGouvActions_continueIncreaseGESPrice <- CleanData$stateInterv_continueIncreaseGESPrice
table(CleanData$scaleGouvActions_continueIncreaseGESPrice)

# stateInterv_decreaseFossilProd
table(CleanData$stateInterv_decreaseFossilProd)
CleanData$scaleGouvActions_decreaseFossilProd <- CleanData$stateInterv_decreaseFossilProd
table(CleanData$scaleGouvActions_decreaseFossilProd)

# economy_governmentClimatePolicyHurtsEconomy
table(CleanData$economy_governmentClimatePolicyHurtsEconomy)
CleanData$scaleGouvActions_governmentClimatePolicyHurtsEconomy <- finverser(CleanData$economy_governmentClimatePolicyHurtsEconomy)
table(CleanData$scaleGouvActions_governmentClimatePolicyHurtsEconomy)

# responsability_GvntVsCitizens
table(CleanData$responsability_GvntVsCitizens)
CleanData$scaleGouvActions_responsabilityGvnt <- CleanData$responsability_GvntVsCitizens
table(CleanData$scaleGouvActions_responsabilityGvnt)

###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- CleanData %>% 
  select(starts_with("scaleGouvActions")) %>% 
  drop_na()

topdown_fa(FaData)

FaData2 <- FaData %>% select(-scaleGouvActions_responsabilityGvnt)

topdown_fa(FaData2)

###**************###
## 1.2 Check distribution of scale ####
###**************###

FaData2$scaleGouvActions <- rowSums(FaData2)/ncol(FaData2)
hist(FaData2$scaleGouvActions)


