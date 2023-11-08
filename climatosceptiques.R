# Packages ----------------------------------------------------------------
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")


# Make scales -------------------------------------------------------------

## nier existence du problème

table(Data$science_climateChangeIsHappening)
table(Data$science_consensusClimateChange)
table(Data$science_scientistsExaggerateClimateChangeEvidence)
Data$exagerate_evidence <- finverser(Data$science_scientistsExaggerateClimateChangeEvidence)
table(Data$exagerate_evidence)
table(Data$gravity_crisisIsExaggerated)
Data$exagerate_crisis <- finverser(Data$gravity_crisisIsExaggerated)
table(Data$exagerate_crisis)

table(Data$science_climateChangeMainCause_humanActivities)
table(Data$science_climateChangeMainCause_natureAndHumans)
table(Data$science_climateChangeMainCause_naturalProcesses)

Data$scale1_deny <- (Data$science_climateChangeIsHappening +
                       Data$science_consensusClimateChange +
                       Data$exagerate_evidence +
                       Data$exagerate_crisis)/4
hist(Data$scale1_deny)
Data$scale1_deny <- finverser(Data$scale1_deny)
hist(Data$scale1_deny)

## nier le rôle de l’Homme
table(Data$science_carContributeClimateChange)
table(Data$science_climateChangeMainCause_humanActivities)
table(Data$science_climateChangeMainCause_naturalProcesses)
table(Data$science_climateChangeMainCause_natureAndHumans)

Data$humanRole <- NA
Data$humanRole[Data$science_climateChangeMainCause_naturalProcesses==1] <- 0
Data$humanRole[Data$science_climateChangeMainCause_natureAndHumans==1] <- 0.5
Data$humanRole[Data$science_climateChangeMainCause_humanActivities==1] <- 1
table(Data$humanRole)

Data$scale2_denyHuman <- (Data$science_carContributeClimateChange +
                            Data$humanRole)/2
hist(Data$scale2_denyHuman)
Data$scale2_denyHuman <- finverser(Data$scale2_denyHuman)
hist(Data$scale2_denyHuman)

## couterait trop cher à régler
table(Data$economy_worryTooMuchAboutEnvironment)
table(Data$economy_governmentClimatePolicyHurtsEconomy)
table(Data$stateInterv_makeGasMoreExpensive)

Data$scale3_expensive <- (Data$economy_worryTooMuchAboutEnvironment +
                            Data$economy_governmentClimatePolicyHurtsEconomy +
                            Data$stateInterv_makeGasMoreExpensive)/3
hist(Data$scale3_expensive)



# yob ---------------------------------------------------------------------
Data$yob <- 2022 - Data$ses_age
hist(Data$yob)
Data$yob_factor <- factor(Data$yob, ordered = F)

# Regressions -------------------------------------------------------------

## I want to see if there is a generational effect among the different scales given
### that climatoscepticism has evolved throughout the years.
### To do this, I will do predicted probabilities using the same IV for each scales and
### switching the yob (and the region for control purposes)

### Histograms
hist(Data$scale1_deny)
hist(Data$scale2_denyHuman)
hist(Data$scale3_expensive)

### Dichotomize the scales at the 90 centile mark
c90 <- quantile(Data$scale1_deny, 0.75)
Data$deny1 <- 0
Data$deny1[Data$scale1_deny >= c90] <- 1
table(Data$deny1)

c90 <- quantile(Data$scale2_denyHuman, 0.75, na.rm = T)
Data$human2 <- 0
Data$human2[Data$scale2_denyHuman >= c90] <- 1
table(Data$human2)

c90 <- quantile(Data$scale3_expensive, 0.75, na.rm = T)
Data$expensive3 <- 0
Data$expensive3[Data$scale3_expensive >= c90] <- 1
table(Data$expensive3)

## No climate attitudes ----------------------------------------------------
model1 <- glm(deny1 ~
               ses_region_mari + ses_region_ont + ses_region_west +
               ses_gender_female + ses_bornCanada + ses_ethn_white +
               ses_educBHS + ses_educUniv + ses_incomeLow + ses_incomeHigh +
               ses_isPrimaryIndustries + ses_proprio + yob_factor,
              family = binomial(),
             data = Data)
summary(model1)

model2 <- glm(human2 ~
                ses_region_mari + ses_region_ont + ses_region_west +
                ses_gender_female + ses_bornCanada + ses_ethn_white +
                ses_educBHS + ses_educUniv + ses_incomeLow + ses_incomeHigh +
                ses_isPrimaryIndustries + ses_proprio + yob_factor,
              family = binomial(),
              data = Data)
summary(model2)

model3 <- glm(expensive3 ~
                ses_region_mari + ses_region_ont + ses_region_west +
                ses_gender_female + ses_bornCanada + ses_ethn_white +
                ses_educBHS + ses_educUniv + ses_incomeLow + ses_incomeHigh +
                ses_isPrimaryIndustries + ses_proprio + yob_factor,
              family = binomial(),
              data = Data)
summary(model3)


### Predicted probabilities ------------------------------------------------------------------

### Create skeleton dataframe containing all SES
#### combinations
yob <- sort(unique(Data$yob))
vars <- c("ses_region_mari", "ses_region_ont", "ses_region_west",
           "ses_gender_female", "ses_bornCanada", "ses_ethn_white",
            "ses_educBHS", "ses_educUniv", "ses_incomeLow", "ses_incomeHigh",
            "ses_isPrimaryIndustries", "ses_proprio")
args <- paste0("unique(Data$", vars, ")", collapse = ", ")
AllSES <- eval(parse(text = paste0(paste0("expand.grid(", args), ", yob)"))) %>% 
  drop_na()
names(AllSES) <- c(vars, "yob")
AllSES <- AllSES %>%
  mutate(n_regions = ses_region_mari + ses_region_ont + ses_region_west,
         ses_region_qc = 1 - (ses_region_mari + ses_region_ont + ses_region_west),
         yob_factor = factor(yob, ordered = F)) %>% 
  filter(n_regions <= 1)

AllSES$pred_deny <- predict(model1, newdata = AllSES, type = "response")
AllSES$pred_human <- predict(model2, newdata = AllSES, type = "response")
AllSES$pred_expensive <- predict(model3, newdata = AllSES, type = "response")

Graph <- AllSES %>% 
  pivot_longer(., cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = "pred_var",
               values_to = "prob") %>% 
  pivot_longer(., cols = starts_with("ses_region"),
               names_prefix = "ses_region_",
               names_to = "region",
               values_to = "drop") %>% 
  filter(drop == 1 &
         region != "mari") %>% 
  select(-drop)

ggplot(Graph, aes(x = yob, y = prob)) +
  facet_wrap(~region) +
  geom_smooth(aes(group = pred_var, color = pred_var),
              span = 0.1) +
  scale_y_continuous(limits = c(0,0.6)) +
  scale_x_continuous(limits = c(1940,2000))


ggplot(Graph, aes(x = yob, y = prob)) +
  geom_smooth(aes(group = pred_var, color = pred_var),
              span = 0.1) +
  scale_y_continuous(limits = c(0,0.6)) +
  scale_x_continuous(limits = c(1940,2000))

