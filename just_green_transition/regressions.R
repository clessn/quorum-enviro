# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data_with_clusters.rds") %>% 
  mutate(region = case_when(
    ses_prov %in% c("alberta", "sk") ~ "Alberta and Saskatchewan",
    ses_prov %in% c("cb") ~ "British Columbia",
    ses_prov %in% c("on") ~ "Ontario",
    ses_prov %in% c("qc") ~ "Quebec",
  ),
  region = ifelse(is.na(region), "other", region),
  region = factor(region)
  )


model <- lm(stateInterv_continueIncreaseGESPrice ~
              scale_gravity + region + ses_gender_male +
              ses_age_24m + ses_age_2534 + ses_age_5564 +
              ses_age_6574 + ses_age_75p + ses_bornCanada +
              ses_educUniv + ses_incomeLow + ses_incomeMid,
            data = data)
summary(model)

model2 <- lm(stateInterv_continueIncreaseGESPrice_RedistributedPop ~
              scale_gravity + region + ses_gender_male +
              ses_age_24m + ses_age_2534 + ses_age_5564 +
              ses_age_6574 + ses_age_75p + ses_bornCanada +
              ses_educUniv + ses_incomeLow + ses_incomeMid,
            data = data)
summary(model2)

model3 <- lm(stateInterv_continueIncreaseGESPrice_FossilJobs ~
               scale_gravity + region + ses_gender_male +
               ses_age_24m + ses_age_2534 + ses_age_5564 +
               ses_age_6574 + ses_age_75p + ses_bornCanada +
               ses_educUniv + ses_incomeLow + ses_incomeMid,
             data = data)
summary(model3)

model4 <- lm(stateInterv_continueIncreaseGESPrice_GreenJobs ~
               scale_gravity + region + ses_gender_male +
               ses_age_24m + ses_age_2534 + ses_age_5564 +
               ses_age_6574 + ses_age_75p + ses_bornCanada +
               ses_educUniv + ses_incomeLow + ses_incomeMid,
             data = data)
summary(model4)



