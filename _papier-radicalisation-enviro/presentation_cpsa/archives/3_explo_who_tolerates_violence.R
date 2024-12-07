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


vis <- names(model_data %>% select(age_cat, starts_with("ses_")))
vds <- c("tolerate_propertyDest",
         "tolerate_violentActions",
         "tolerate_disruptive")
vds_labels <- c("tolerate_propertyDest" = "Property destruction",
                "tolerate_violentActions" = "Violent actions",
                "tolerate_disruptive" = "Nonviolent, disruptive actions")

for (i in vis){
  model_data$vi <- model_data[[i]]
  message("")
  message(i)
  for (j in vds){
    model_data$vd <- model_data[[j]]
    model <- glm(vd ~ age_cat + ses_region +
                   ses_gender + ses_educ +
                   ses_ethn + ses_income +
                   ses_religion + ses_lifeReligion +
                   ses_proprio + ses_hetero,
                 family = binomial(link = "logit"),
                 data = model_data)
    datagrid <- marginaleffects::datagrid(
      model = model,
      vi = levels(model_data[[i]])
    )
    datagrid[[i]] <- datagrid$vi
    predsj <- marginaleffects::predictions(
      model = model,
      newdata = datagrid,
      type = "response") %>% 
      mutate(vd = vds_labels[j]) %>% 
      select(vi, estimate, conf.low, conf.high, vd)
    levels(predsj$vi) <- levels(model_data[[i]])
    predsj[[i]] <- predsj$vi
    predsj <- predsj %>% 
      select(vd, all_of(i), estimate, conf.low, conf.high) %>% 
      mutate(conf.low = ifelse(conf.low < 0, 0, conf.low),
             conf.high = ifelse(conf.high > 1, 1, conf.high))
    if (j == vds[1]){
      predsi <- predsj %>% 
        mutate(vd = factor(vd,
                           levels = c("Nonviolent, disruptive actions",
                                      "Property destruction",
                                      "Violent actions")))
    } else {
      predsi <- predsj %>% 
        mutate(vd = factor(vd,
                           levels = c("Nonviolent, disruptive actions",
                                      "Property destruction",
                                      "Violent actions"))) %>%
        rbind(predsi, .)
    }
    message(paste0("    ", j))
  }
  if (i == vis[1]){
    preds_list <- list(predsi)
    names(preds_list)[1] <- i
  } else {
    preds_list[[i]] <- predsi
  }
  message(i)
}

for (i in vis){
  d <- preds_list[[i]]
  d$vi <- d[[i]]
  ggplot(d, aes(x = vi, y = estimate)) +
    facet_wrap(~vd) +
    geom_point() +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
    xlab("") +
    ylab("Predicted probability\nof tolerating action (%)") +
    scale_y_continuous(limits = c(0, 1),
                       labels = c(0, 25, 50, 75, 100),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    clessnize::theme_clean_light(base_size = 15) +
    theme(axis.title.y = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/new/3_who_tolerates_violence/", i, ".png"),
         width = 10, height = 8)
  message(i)
}





### with age_cat * vi ####
vis <- names(model_data %>% select(starts_with("ses_")))
vds <- c("tolerate_propertyDest",
         "tolerate_violentActions",
         "tolerate_disruptive")

for (i in vis){
  model_data$vi <- model_data[[i]]
  message("")
  message(i)
  for (j in vds){
    model_data$vd <- model_data[[j]]
    model <- glm(vd ~ age_cat + ses_region +
                   ses_gender + ses_educ +
                   ses_ethn + ses_income +
                   ses_religion + ses_lifeReligion +
                   ses_proprio + ses_hetero,
                 family = binomial(link = "logit"),
                 data = model_data)
    new_formula <- update(current_formula, reformulate(c(".", paste0("age_cat * ", i))))
    model <- update(model, formula = new_formula)
    datagrid <- marginaleffects::datagrid(
      model = model,
      age_cat = levels(model_data$age_cat),
      vi = levels(model_data[[i]])
    )
    datagrid[[i]] <- datagrid$vi
    predsj <- marginaleffects::predictions(
      model = model,
      newdata = datagrid,
      type = "response") %>% 
      mutate(vd = j) %>% 
      select(age_cat, vi, estimate, conf.low, conf.high, vd)
    levels(predsj$vi) <- levels(model_data[[i]])
    predsj[[i]] <- predsj$vi
    predsj <- predsj %>% 
      select(vd, age_cat, all_of(i), estimate, conf.low, conf.high)
    if (j == vds[1]){
      predsi <- predsj
    } else {
      predsi <- rbind(predsi, predsj)
    }
    message(paste0("    ", j))
  }
  if (i == vis[1]){
    preds_list <- list(predsi)
    names(preds_list)[1] <- i
  } else {
    preds_list[[i]] <- predsi
  }
  message(i)
}


d <- preds_list[["ses_gender"]]

ggplot(d, aes(x = ses_gender, y = estimate)) +
  facet_wrap(~vd) +
  geom_point(aes(group = age_cat,
                 color = age_cat),
             position = position_dodge(width = 0.5))

