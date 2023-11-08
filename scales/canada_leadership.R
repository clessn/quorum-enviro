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
## 0 = all countries equal, 1 = canada and rich countries have a bigger role 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleLeadership_ prefix

# international_richCountriesMustLead
table(CleanData$international_richCountriesMustLead)
CleanData$scaleLeadership_richCountriesMustLead <- CleanData$international_richCountriesMustLead
table(CleanData$scaleLeadership_richCountriesMustLead)

# international_climateChangeHarmCA MINUS international_climateChangeHarmDevelopment
table(CleanData$international_climateChangeHarmCA)
table(CleanData$international_climateChangeHarmDevelopment)
CleanData$scaleLeadership_HarmDevMoreThanCA <- CleanData$international_climateChangeHarmDevelopment - CleanData$international_climateChangeHarmCA
table(CleanData$scaleLeadership_HarmDevMoreThanCA)
hist(CleanData$scaleLeadership_HarmDevMoreThanCA)
CleanData$scaleLeadership_HarmDevMoreThanCA <- minmaxNormalization(CleanData$scaleLeadership_HarmDevMoreThanCA)
table(CleanData$scaleLeadership_HarmDevMoreThanCA)
hist(CleanData$scaleLeadership_HarmDevMoreThanCA)

# international_CanadaShouldAcceptRefugees
table(CleanData$international_CanadaShouldAcceptRefugees)
CleanData$scaleLeadership_CanadaShouldAcceptRefugees <- CleanData$international_CanadaShouldAcceptRefugees
table(CleanData$scaleLeadership_CanadaShouldAcceptRefugees)

# international_richCountriesHelpRefugees
table(CleanData$international_richCountriesHelpRefugees)
CleanData$scaleLeadership_richCountriesAcceptRefugees <- CleanData$international_richCountriesHelpRefugees
table(CleanData$scaleLeadership_richCountriesAcceptRefugees)


###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- CleanData %>% 
  select(starts_with("scaleLeadership")) %>% 
  drop_na()

topdown_fa(FaData)

FaData2 <- FaData %>% select(-scaleLeadership_HarmDevMoreThanCA)

topdown_fa(FaData2)

###**************###
## 1.2 Check distribution of scale ####
###**************###

FaData2$scaleLeadership <- rowSums(FaData2)/ncol(FaData2)
hist(FaData2$scaleLeadership)

