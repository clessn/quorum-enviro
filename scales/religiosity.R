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
## 0 = not religious at all, 1 = religious and active 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleReligiosity_ prefix

# member of a religious community
table(CleanData$ses_relNone)
CleanData$scaleReligiosity_memberCommunity <- 1
CleanData$scaleReligiosity_memberCommunity[CleanData$ses_relNone == 1] <- 0
table(CleanData$scaleReligiosity_memberCommunity)

# 'strength' of religiosity
table(CleanData$ses_lifeReligion)
CleanData$scaleReligiosity_lifeReligion <- CleanData$ses_lifeReligion
table(CleanData$scaleReligiosity_lifeReligion)

# attends events
table(CleanData$ses_attendsReligiousEvents)
CleanData$scaleReligiosity_attendsReligiousEvents <- CleanData$ses_attendsReligiousEvents
table(CleanData$scaleReligiosity_attendsReligiousEvents)


###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- CleanData %>% 
  select(starts_with("scaleReligiosity")) %>% 
  drop_na()

topdown_fa(FaData)

###**************###
## 1.2 Check distribution of scale ####
###**************###

FaData$scaleReligiosity <- rowSums(FaData)/ncol(FaData)
hist(FaData$scaleReligiosity)



# Some models -------------------------------------------------------------
model <- lm(ses_lifeReligion ~ ses_prov_qc + ses_gender_female +
              ses_hetero + ses_ethn_white + ses_age_75p*ses_prov_qc +
              ses_age_75p,
            data = CleanData)
summary(model)

model <- glm(scaleReligiosity_memberCommunity ~ ses_prov_qc + ses_gender_female +
               ses_hetero + ses_ethn_white + ses_age_75p*ses_prov_qc +
               ses_age_75p,
             family = binomial(),
             data = CleanData)
summary(model)

model <- lm(ses_lifeReligion ~ ses_prov_qc + ses_gender_female +
              ses_hetero + ses_ethn_white + ses_age_75p*ses_prov_qc +
              ses_age_75p,
            data = CleanData)
summary(model)

model <- lm(scaleReligiosity_attendsReligiousEvents ~ ses_prov_qc + ses_gender_female +
               ses_hetero + ses_ethn_white + ses_age_75p*ses_prov_qc +
               ses_age_75p,
             #family = binomial(),
             data = CleanData)
summary(model)

CleanData <- CleanData %>% 
  mutate(scaleReligiosity = (CleanData %>%
                              select(starts_with("scaleReligiosity_")) %>%
                              rowSums())/length(names(CleanData %>% select(starts_with("scaleReligiosity_")))))
model <- lm(scaleReligiosity ~ ses_prov_qc + ses_gender_female +
              ses_hetero + ses_ethn_white + ses_age_75p*ses_prov_qc +
              ses_age_75p,
            #family = binomial(),
            data = CleanData)
summary(model)

CleanData$rel_pred <- predict(object = model,
                              newdata = CleanData)
hist(CleanData$rel_pred)
