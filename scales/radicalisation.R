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
  select(starts_with("radicalisation_tolerate_"))
names(FaData) <- gsub("radicalisation_tolerate_", "", names(FaData))

ggpairs(FaData,
        diag = list(continuous=wrap("barDiag",
                                    binwidth=0.25,
                                    fill = "blue",
                                    color = "blue",
                                    alpha = 0.6)),
        lower = list(continuous=ggpairs_jitter_with_smooth))


