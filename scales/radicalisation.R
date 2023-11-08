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
# Explore variables ####
###**************###
FaData <- Data %>%
  select(starts_with("radicalisation"))
names(FaData) <- gsub("radicalisation_tolerate_", "", names(FaData))
names(FaData) <- gsub("radicalisation_", "", names(FaData))

ggpairs(FaData,
        diag = list(continuous=wrap("barDiag",
                                    binwidth=0.25,
                                    fill = "blue",
                                    color = "blue",
                                    alpha = 0.6)),
        lower = list(continuous=ggpairs_jitter_with_smooth))

###**************###
## 1.1 Factor analysis ####
###**************###
topdown_fa(FaData)

### Every variable except signPetition contributes to the scale

