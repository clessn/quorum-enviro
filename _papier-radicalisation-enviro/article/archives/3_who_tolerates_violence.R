# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

get_age_category <- function(ages){
  groups <- seq(from = 13,
                to = max(ages, na.rm = T),
                by = 5)
  age_cats <- c()
  for (i in 1:length(ages)){
    if (!is.na(ages[i])){
      age_cats[i] <- max(groups[groups<=ages[i]],
                         na.rm = T)
    } else (age_cats[i] <- NA)
  }
  return(age_cats)
}

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds") %>%
  mutate(educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5,
                             educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1,
                             educ_level),
         age_cat = get_age_category(ses_age),
         age_cat = ifelse(age_cat >= 33, "33+", age_cat),
         age_cat = ifelse(age_cat == "13", "18", age_cat),
         age_cat = factor(age_cat,
                          levels = c("18", "23", "28", "33+")),
         tolerate_fightPolice = ifelse(radicalisation_tolerate_fightPolice >= 0.33, 1, 0),
         tolerate_violatingPowerful = ifelse(radicalisation_tolerate_violatingPowerful >= 0.33, 1, 0),
         tolerate_vandalismObjects = ifelse(radicalisation_tolerate_vandalismObjects >= 0.33, 1, 0),
         tolerate_throwingObjectsInfrastructure = ifelse(radicalisation_tolerate_throwingObjectsInfrastructure >= 0.33, 1, 0),
         tolerate_sabotagingInfrastructure = ifelse(radicalisation_tolerate_sabotagingInfrastructure >= 0.33, 1, 0),
         tolerate_attachTreeVehicule = ifelse(radicalisation_tolerate_attachTreeVehicule >= 0.33, 1, 0),
         tolerate_occupyPublicSpace = ifelse(radicalisation_tolerate_occupyPublicSpace >= 0.33, 1, 0),
         tolerate_blockPipelineConstruction = ifelse(radicalisation_tolerate_blockPipelineConstruction >= 0.33, 1, 0),
         tolerate_blockBridgeRoad = ifelse(radicalisation_tolerate_blockBridgeRoad >= 0.33, 1, 0),
         tolerate_violentActions = ifelse((tolerate_fightPolice + tolerate_violatingPowerful) > 0, 1, 0),
         tolerate_propertyDest = ifelse((tolerate_vandalismObjects +
                                           tolerate_throwingObjectsInfrastructure +
                                           tolerate_sabotagingInfrastructure) > 0, 1, 0),
         tolerate_disruptive = ifelse((tolerate_attachTreeVehicule +
                                         tolerate_occupyPublicSpace +
                                         tolerate_blockPipelineConstruction +
                                         tolerate_blockBridgeRoad) > 0, 1, 0))

table(data$age_cat)
data$age_cat <- relevel(data$age_cat, ref = "33+")

table(data$tolerate_violentActions)
table(data$tolerate_propertyDest)
table(data$tolerate_disruptive)

data_long <- data %>% 
  tidyr::pivot_longer(.,
                      cols = c(tolerate_violentActions,
                               tolerate_propertyDest,
                               tolerate_disruptive),
                      names_to = "action_type",
                      values_to = "tolerance") %>% 
  mutate(action_type = case_when(
    action_type == "tolerate_violentActions" ~ "Violent",
    action_type == "tolerate_propertyDest" ~ "Property destruction",
    action_type == "tolerate_disruptive" ~ "Nonviolent,\ndisruptive"
  ),
  action_type = factor(action_type,
                       levels = c("Nonviolent,\ndisruptive",
                                  "Violent",
                                  "Property destruction"
                                  )))

# 2. models ---------------------------------------------------------------

model_data <- data %>% 
  mutate(ses_region = case_when(
    ses_region_qc == 1 ~ "Quebec",
    ses_region_ont == 1 ~ "Ontario",
    ses_region_west == 1 ~ "Western provinces",
    ses_region_mari == 1 ~ "Maritimes"
  ),
  ses_region = ifelse(is.na(ses_region), "Other", ses_region),
  ses_region = factor(ses_region),
  ses_educ = case_when(
    ses_educBHS == 1 ~ "High school",
    ses_educCollege == 1 ~ "College",
    ses_educUniv == 1 ~ "University"
  ),
  ses_educ = factor(ses_educ,
                    levels = c("High school", "College", "University")),
  ses_income = case_when(
    ses_incomeLow == 1 ~ "Low",
    ses_incomeMid == 1 ~ "Mid",
    ses_incomeHigh == 1 ~ "High"
  ),
  ses_income = factor(ses_income,
                      levels = c("Low", "Mid", "High")),
  ses_religion = case_when(
    ses_relCatho == 1 ~ "Catholic",
    ses_relProtest == 1 ~ "Protestant"
  ),
  ses_religion = ifelse(is.na(ses_religion), "Other", ses_religion),
  ses_religion = factor(ses_religion),
  ses_gender = ifelse(ses_gender_male == 1, "Male", "Female"),
  ses_gender = factor(ses_gender),
  ses_ethn = ifelse(ses_ethn_white == 1, "White", "Other"),
  ses_ethn = factor(ses_ethn),
  ses_lifeReligion = case_when(
    ses_lifeReligion == 0 ~ "Strongly Disagree",
    ses_lifeReligion == 0.25 ~ "Somewhat Disagree",
    ses_lifeReligion == 0.5 ~ "Neutral",
    ses_lifeReligion == 0.75 ~ "Somewhat Agree",
    ses_lifeReligion == 1 ~ "Strongly Agree"
  ),
  ses_lifeReligion = factor(ses_lifeReligion,
                            levels = c("Strongly Disagree",
                                       "Somewhat Disagree",
                                       "Neutral",
                                       "Somewhat Agree",
                                       "Strongly Agree")),
  ses_proprio = ifelse(ses_proprio == 1, "Owner", "Tenant/Other"),
  ses_proprio = factor(ses_proprio),
  ses_hetero = ifelse(ses_hetero == 1, "Hetero", "LGBTQ+"),
  ses_hetero = factor(ses_hetero),
  ) %>% 
  select(
    age_cat,
    ses_region,
    ses_gender,
    ses_educ,
    ses_ethn,
    ses_income,
    ses_religion,
    ses_lifeReligion,
    ses_proprio,
    ses_hetero,
    tolerate_propertyDest,
    tolerate_violentActions,
    tolerate_disruptive)

vds <- c("tolerate_propertyDest",
         "tolerate_violentActions",
         "tolerate_disruptive")
vds_labels <- c("tolerate_propertyDest" = "Property destruction",
                "tolerate_violentActions" = "Violent actions",
                "tolerate_disruptive" = "Nonviolent, disruptive actions")

for (i in vds){
    model_data$vd <- model_data[[i]]
    model <- glm(vd ~ age_cat + ses_region +
                   ses_gender + ses_educ +
                   ses_ethn + ses_income +
                   ses_religion + ses_lifeReligion +
                   ses_proprio + ses_hetero,
                 family = binomial(link = "logit"),
                 data = model_data)
    datagrid_age <- marginaleffects::datagrid(
      model = model,
      age_cat = levels(model_data$age_cat)
    )
    datagrid_region <- marginaleffects::datagrid(
      model = model,
      ses_region = levels(model_data$ses_region)
    )
    datagrid_educ <- marginaleffects::datagrid(
      model = model,
      ses_educ = levels(model_data$ses_educ)
    )
    ## age
    predsi_age <- marginaleffects::predictions(
      model = model,
      newdata = datagrid_age,
      type = "response") %>% 
      mutate(vd = vds_labels[i],
             vi = "Age",
             vi_value = as.character(age_cat)) %>% 
      select(vd, vi, vi_value, estimate, conf_low95 = conf.low, conf_high95 = conf.high)
    predsi_age99 <- marginaleffects::predictions(
      model = model,
      newdata = datagrid_age,
      type = "response",
      conf_level = 0.99) %>% 
      select(vi_value = age_cat, conf_low99 = conf.low, conf_high99 = conf.high)
    predsi_age <- left_join(predsi_age, predsi_age99, by = "vi_value")
    ## region
    predsi_region <- marginaleffects::predictions(
      model = model,
      newdata = datagrid_region,
      type = "response") %>% 
      mutate(vd = vds_labels[i],
             vi = "Region",
             vi_value = as.character(ses_region)) %>% 
      select(vd, vi, vi_value, estimate, conf_low95 = conf.low, conf_high95 = conf.high)
    predsi_region99 <- marginaleffects::predictions(
      model = model,
      newdata = datagrid_region,
      type = "response",
      conf_level = 0.99) %>% 
      select(vi_value = ses_region, conf_low99 = conf.low, conf_high99 = conf.high)
    predsi_region <- left_join(predsi_region, predsi_region99, by = "vi_value")
    # educ
    predsi_educ <- marginaleffects::predictions(
      model = model,
      newdata = datagrid_educ,
      type = "response") %>% 
      mutate(vd = vds_labels[i],
             vi = "Education level",
             vi_value = as.character(ses_educ)) %>% 
      select(vd, vi, vi_value, estimate, conf_low95 = conf.low, conf_high95 = conf.high)
    predsi_educ99 <- marginaleffects::predictions(
      model = model,
      newdata = datagrid_educ,
      type = "response",
      conf_level = 0.99) %>% 
      select(vi_value = ses_educ, conf_low99 = conf.low, conf_high99 = conf.high)
    predsi_educ <- left_join(predsi_educ, predsi_educ99, by = "vi_value")
    predsi <- rbind(predsi_age, predsi_region, predsi_educ)  
    if (i == vds[1]){
      preds_df <- predsi %>% 
        mutate(vd = factor(vd,
                           levels = c("Nonviolent, disruptive actions",
                                      "Property destruction",
                                      "Violent actions")))
    } else {
      preds_df <- predsi %>% 
        mutate(vd = factor(vd,
                           levels = c("Nonviolent, disruptive actions",
                                      "Property destruction",
                                      "Violent actions"))) %>%
        rbind(preds_df, .)
    }
    preds_df <- preds_df %>% 
      mutate(conf_low95 = ifelse(conf_low95 < 0, 0, conf_low95),
             conf_high95 = ifelse(conf_high95 > 1, 1, conf_high95),
             conf_low99 = ifelse(conf_low99 < 0, 0, conf_low99),
             conf_high99 = ifelse(conf_high99 > 1, 1, conf_high99))
    message(i)
}

# Graph -------------------------------------------------------------------

preds_df <- preds_df %>% 
  mutate(vi_value_order = case_when(
    vi_value %in% c("18", "High school") ~ 1,
    vi_value %in% c("23", "College") ~ 2,
    vi_value %in% c("28", "University") ~ 3,
    vi_value == "33+" ~ 4
  ),
  vi_value = ifelse(vi_value == "18", "18-22", vi_value),
  vi_value = ifelse(vi_value == "23", "23-27", vi_value),
  vi_value = ifelse(vi_value == "28", "28-32", vi_value)
  )

ggplot(preds_df, aes(x = estimate, y = reorder(vi_value, -vi_value_order))) +
  facet_grid(rows = vars(vi),
             cols = vars(vd),
             scales = "free_y",
             switch = "y") +
  scale_x_continuous(name = "\nPredicted probability\nof tolerating action (%)\n",
                     labels = c(0, 25, 50, 75, 100),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1)) +
  geom_point(size = 2) +
  geom_linerange(aes(xmin = conf_low95, xmax = conf_high95),
                 linewidth = 1.1) +
  geom_linerange(aes(xmin = conf_low99, xmax = conf_high99),
                 linewidth = 0.5) +
  ylab("") +
  labs(caption = "Lines around the points represent the 95% and 99% confidence intervals.") +
  clessnize::theme_clean_light() +
  theme(strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "grey85"))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/figure3_who_tolerates_violence.png",
       width = 9, height = 6)
