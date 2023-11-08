# Packages ----------------------------------------------------------------
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data_with_clusters.rds") %>% 
  mutate(tol_violentActions = (radicalisation_tolerate_fightPolice +
                                 radicalisation_tolerate_violatingPowerful)/2,
         tol_propertyDest = (radicalisation_tolerate_vandalismObjects +
                               radicalisation_tolerate_throwingObjectsInfrastructure +
                               radicalisation_tolerate_sabotagingInfrastructure)/3,
         educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5, educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1, educ_level),
         income = ifelse(ses_incomeLow == 1, 0, NA),
         income = ifelse(ses_incomeMid == 1, 0.5, income),
         income = ifelse(ses_incomeHigh == 1, 1, income),
         age_cat = get_age_category(ses_age)) %>% 
  fastDummies::dummy_columns(.,
                             select_columns = c("age_cat")) %>% 
  mutate(age_cat_33p = ifelse(ses_age >= 33, 1, 0),
         tol_propertyDest = ifelse(tol_propertyDest > 0, 1, 0),
         tol_violentActions = ifelse(tol_violentActions > 0, 1, 0),
         age_cat = ifelse(age_cat == 13, 18, age_cat),
         age_cat_model = ifelse(!(age_cat %in% c(18,23,28)), 33, age_cat),
         age_cat_model = factor(age_cat_model)) %>% 
  select(-scale_radicalisation, -scale_optimist, -scale_engaged,
         -starts_with("scale_locus"))

# Simple comparison -------------------------------------------------------
Agg <- Data %>% 
  group_by(k4) %>%
  summarise(nCluster = n(),
            nViolentActions = sum(tol_violentActions),
            nPropDest = sum(tol_propertyDest)) %>% 
  mutate(prop_violentActions = round(nViolentActions/nCluster*100),
         prop_propDest = round(nPropDest/nCluster*100))


# Predicted probabilities ------------------------------------------------------------------

### Create skeleton dataframe containing all SES
#### combinations
vds <- c("tol_propertyDest", "tol_violentActions")
attitudes <- names(Data %>% select(starts_with("scale")))
att_levels <- seq(from = 0, to = 1, by = 0.1)
vars <- c("age_cat_model", "ses_gender_male",
          "income", "educ_level")
args <- paste0("unique(Data$", vars, ")", collapse = ", ")
AllSES <- eval(parse(text = paste0(paste0("expand.grid(", args), ", att_levels)"))) %>% 
  drop_na()
names(AllSES) <- c(vars, "att_level")

### Create empty dataframe to drop results in
Graph <- data.frame(
  age_cat_model = as.numeric(),
  ses_gender_male = as.numeric(),
  income = as.numeric(),
  educ_level = as.numeric(),
  att_level = as.numeric(),
  att = as.character(),
  vd = as.character(),
  pred_vd = as.numeric()
)

for (i in 1:length(attitudes)){
  attitude <- attitudes[i]
  pd <- predProbs_propDest(Data, attitude, AllSES)
  va <- predProbs_violentActions(Data, attitude, AllSES)
  Graphi <- rbind(pd, va)
  Graph <- rbind(Graph, Graphi)
}

clean_age_cats <- c("18" = "18-22 ans",
                    "23" = "23-27 ans",
                    "28" = "28-32 ans",
                    "33" = "33 ans et +")

clean_att_names <- c("Scepticisme", "Préoccupé",
                     "Supporte innov.\ntechno et IA",
                     "Cpt écolo", "Supporte leadership\ninternational",
                     "Religiosité/spiritualité", "Supporte\nmesures 'carottes'",
                     "Supporte\nmesures 'bâton'", "Supporte\nmesures gouvernementales")
names(clean_att_names) <- attitudes

clean_vd_names <- c("tol_propertyDest" = "Destruction\nde propriété",
                    "tol_violentActions" = "Actions violentes")

Graph$age_cat_clean <- clean_age_cats[as.character(Graph$age_cat_model)]
Graph$attitude_clean <- clean_att_names[Graph$att]
Graph$vd_clean <- clean_vd_names[Graph$vd]

### Difference by age_cat ###
Graph %>% 
  ggplot(aes(x = att_level, y = pred_vd*100)) +
  geom_jitter(width = 0.12,
              height = 0.12,
              alpha = 0.05) +
  ylab("Probabilité de tolérer (%)") +
  xlab("") +
  scale_y_continuous(limits = c(0,100)) +
  facet_grid(cols = vars(age_cat_clean),
             rows = vars(vd_clean)) +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5),
        strip.placement.x = "left")

ggsave("_SharedFolder_quorum-enviro/graphs/whoToleratesViolence/age_cat.png",
       width = 10, height = 8)
###***********************************************************************###

Graph %>% 
  filter(vd == "tol_propertyDest") %>%
  mutate(att = gsub("scale_", "", att)) %>% 
  ggplot(aes(x = att_level, y = pred_vd*100,
             group = att, color = att)) +
  geom_jitter(width = 0.12,
              height = 0.12,
              alpha = 0.1,
              show.legend = F) +
  geom_smooth(method = "gam",
              se = F, alpha = 1,
              show.legend = F) +
  scale_y_continuous(limits = c(0,100)) +
  xlab("") +
  ylab("Probabilité de tolérer:\ndestruction de propriété") +
  facet_wrap(~attitude_clean) +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5))

ggsave("_SharedFolder_quorum-enviro/graphs/whoToleratesViolence/propertyDest.png",
       width = 7, height = 8)

Graph %>% 
  filter(vd == "tol_violentActions") %>%
  mutate(att = gsub("scale_", "", att)) %>% 
  ggplot(aes(x = att_level, y = pred_vd*100,
             group = att, color = att)) +
  geom_jitter(width = 0.12,
              height = 0.12,
              alpha = 0.1,
              show.legend = F) +
  geom_smooth(method = "gam",
              se = F, alpha = 1,
              show.legend = F) +
  scale_y_continuous(limits = c(0,100)) +
  xlab("") +
  ylab("Probabilité de tolérer:\nactions violentes") +
  facet_wrap(~attitude_clean) +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5))

ggsave("_SharedFolder_quorum-enviro/graphs/whoToleratesViolence/violentActions.png",
       width = 7, height = 8)
