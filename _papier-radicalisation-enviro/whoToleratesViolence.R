# Packages ####
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Data ####
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds") %>% 
  mutate(tol_violentActions = (radicalisation_tolerate_fightPolice +
                                 radicalisation_tolerate_violatingPowerful)/2,
         tol_propertyDest = (radicalisation_tolerate_vandalismObjects +
                               radicalisation_tolerate_throwingObjectsInfrastructure +
                               radicalisation_tolerate_sabotagingInfrastructure)/3,
         educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5, educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1, educ_level),
         age_cat = get_age_category(ses_age)) %>% 
  fastDummies::dummy_columns(.,
                             select_columns = c("age_cat"))


sysfonts::font_add_google("Roboto")
showtext::showtext_auto()

## Quick histograms ####
Data %>% 
  select(starts_with("tol_")) %>% 
  pivot_longer(starts_with("tol_")) %>%
  mutate(name = case_when(
    name == "tol_propertyDest" ~ "Property destruction",
    name == "tol_violentActions" ~ "Violent actions"
  )) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.15, fill = "grey", color = "grey") +
  facet_wrap(~name) +
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("Tolerance")

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/whoToleratesViolence/distributions.png",
       width = 6, height = 3)


### Need to dummy the variables and do a logistic binomial regression
Data2 <- Data %>% 
  mutate(tol_propertyDest = ifelse(tol_propertyDest > 0, 1, 0),
         tol_violentActions = ifelse(tol_violentActions > 0, 1, 0))
table(Data2$tol_propertyDest)
table(Data2$tol_violentActions)

# Do 50000  regressions with random VI from a bank ####
controls <- c("scale_gravity",
              "age_cat_18",
              "age_cat_23",
              "age_cat_28",
              "educ_level",
              "ses_gender_male",
              "ses_incomeHigh",
              "ses_incomeLow")

vi_bank <- c("ses_couple", "ses_married", "ses_bornCanada",
             "ses_ethn_white", "ses_proprio", "ses_relCatho",
             "ses_relNone", "ses_hetero", "ses_gai",
             names(Data2)[77:80],
             names(Data2)[89:145],
             names(Data2)[160:161])


## Property destruction ####
vd_model <- "tol_propertyDest"

#GraphData <- data.frame(
#  model = as.numeric(),
#  vd = as.character(),
#  vi = as.character(),
#  coef = as.numeric(),
#  se = as.numeric(),
#  pval = as.numeric()
#)
#
#
#for (i in 1:50000){
#  vis <- vi_bank[sample(1:length(vi_bank), 5, replace = T)]
#  predictors <- c(controls, vis)
#  modeli <- glm(formula = as.formula(paste(vd_model, "~", paste(predictors, collapse = " + "))),
#                data = Data2,
#                family = binomial)
#  model <- rep(i, 5)
#  vd <- rep(vd_model, 5)
#  vi <- names(modeli$coefficients)
#  vi <- vi[vi %in% vis]
#  coef <- modeli$coefficients
#  coef <- coef[vi]
#  se <- summary(modeli)$coefficients[,2]
#  se <- se[vi]
#  pval <- round(summary(modeli)$coefficients[,4], 3)
#  pval <- pval[vi]
#  GraphDatai <- as.data.frame(cbind(model, vd, vi, coef, se, pval),
#                              row.names = F)
#  GraphData <- rbind(GraphData, GraphDatai)
#  GraphData$coef <- as.numeric(GraphData$coef)
#  GraphData$se <- as.numeric(GraphData$se)
#  GraphData$pval <- as.numeric(GraphData$pval)
#  if (i %% 1000 == 0){
#    print(i)
#  }
#}
#
#saveRDS(GraphData, "_SharedFolder_quorum-enviro/data/whoToleratesViolence_regressions_propertyDestruction.rds")


## Violence ####  
vd_model <- "tol_violentActions"

#GraphData <- data.frame(
#  model = as.numeric(),
#  vd = as.character(),
#  vi = as.character(),
#  coef = as.numeric(),
#  se = as.numeric(),
#  pval = as.numeric()
#)
#
#
#for (i in 1:10000){
#  vis <- vi_bank[sample(1:length(vi_bank), 5, replace = T)]
#  predictors <- c(controls, vis)
#  modeli <- glm(formula = as.formula(paste(vd_model, "~", paste(predictors, collapse = " + "))),
#                data = Data2,
#                family = binomial)
#  model <- rep(i, 5)
#  vd <- rep(vd_model, 5)
#  vi <- names(modeli$coefficients)
#  vi <- vi[vi %in% vis]
#  coef <- modeli$coefficients
#  coef <- coef[vi]
#  se <- summary(modeli)$coefficients[,2]
#  se <- se[vi]
#  pval <- round(summary(modeli)$coefficients[,4], 3)
#  pval <- pval[vi]
#  GraphDatai <- as.data.frame(cbind(model, vd, vi, coef, se, pval),
#                              row.names = F)
#  GraphData <- rbind(GraphData, GraphDatai)
#  GraphData$coef <- as.numeric(GraphData$coef)
#  GraphData$se <- as.numeric(GraphData$se)
#  GraphData$pval <- as.numeric(GraphData$pval)
#  if (i %% 100 == 0){
#    print(i)
#  }
#}
#
#saveRDS(GraphData, "_SharedFolder_quorum-enviro/data/whoToleratesViolence_regressions_violentActions.rds")

Pd <- readRDS("_SharedFolder_quorum-enviro/data/whoToleratesViolence_regressions_propertyDestruction.rds")

Agg <- Pd %>%
  mutate(sign = ifelse(pval <= 0.05, 1, 0)) %>% 
  group_by(vi) %>% 
  summarise(n = n(),
            prop_sign = sum(sign)/n,
            mean_coef = mean(coef))

Va <- readRDS("_SharedFolder_quorum-enviro/data/whoToleratesViolence_regressions_violentActions.rds")

Agg2 <- Va %>%
  mutate(sign = ifelse(pval <= 0.05, 1, 0)) %>% 
  group_by(vi) %>% 
  summarise(n = n(),
            prop_sign = sum(sign)/n,
            mean_coef = mean(coef))


Graph <- rbind(Pd, Va) %>% 
  mutate(sign = ifelse(pval <= 0.05, 1, 0)) %>% 
  group_by(vd, vi) %>% 
  summarise(n = n(),
            prop_sign = sum(sign)/n,
            mean_coef = mean(coef)) %>% 
  group_by(vd) %>% 
  mutate(y = runif(n = n())) %>% 
  filter(prop_sign >= 0.8,
         mean_coef < 0)

ggplot(Graph, aes(x = mean_coef, y = y)) +
#  ggrepel::geom_label_repel(aes(label = vi, alpha = prop_sign)) +
  #geom_text(aes(label = vi)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = vi)) +
  facet_wrap(~vd) +
#  scale_y_continuous(limits = c(0.8, 1)) +
  geom_vline(xintercept = 0) +
  clessnverse::theme_clean_light(base_size = 30)

ggsave("_SharedFolder_quorum-enviro/explo_tolVol.png",
       width = 10, height = 6)
