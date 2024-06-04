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
                                levels = c("18", "23", "28", "33+")))

table(data$age_cat)
data$age_cat <- relevel(data$age_cat, ref = "33+")


# Create models -----------------------------------------------------------
vds <- names(data %>% 
               select(starts_with("radicalisation_tolerate")))
names(data) <- gsub("radicalisation_tolerate_",
                    "",
                    names(data))
vds <- gsub("radicalisation_tolerate_",
            "",
            vds)

for (i in 1:length(vds)){
  vd <- vds[i]
  model <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity*age_cat +
                 ses_gender_male + educ_level +
                 ses_incomeHigh + ses_incomeLow +
                 ses_region_ont + ses_region_west + ses_region_mari +
                 ses_couple + ses_bornCanada,
                 data = data)")))
  preds_age <- marginaleffects::predictions(
    model = model,
    newdata = marginaleffects::datagrid(
      scale_gravity = c(0, 1),
      age_cat = levels(data$age_cat)
    )) %>% 
    mutate(item = vd) %>% 
    select(item, scale_gravity, age_cat, estimate, conf.low, conf.high)
  preds_noage <- marginaleffects::predictions(
    model = model,
    newdata = marginaleffects::datagrid(
      scale_gravity = c(0, 1)
    )) %>% 
    mutate(age_cat = "all",
           item = vd) %>% 
    select(item, scale_gravity, age_cat, estimate, conf.low, conf.high)
  predsi <- rbind(preds_age, preds_noage) %>% 
    mutate(facet = ifelse(age_cat == "all", "All ages", "By age group"))
  if (i == 1){
    preds <- predsi
  } else {
    preds <- rbind(preds, predsi)
  }
  message(vd)
}

# Graph predicted tolerance -----------------------------------------------------

groups <- c("Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive",
            "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive",
            "Property\ndestruction", "Property\ndestruction", "Property\ndestruction",
            "Violent", "Violent")
names(groups) <- c("signPetition", "boycott", "divest", "manifestation", "occupyPublicSpace",
                   "attachTreeVehicule", "blockBridgeRoad", "blockPipelineConstruction", "vandalismObjects",
                   "sabotagingInfrastructure", "throwingObjectsInfrastructure", "fightPolice", "violatingPowerful")

# Clean item names ####
clean_names <- c("Signing a petition", 
                 "Boycotting products\nand companies",
                 "Divesting investments",
                 "Participating in a manifestation",
                 "Temporarily occupying\na public space",
                 "Tying himself/herself to\na tree or a vehicle",
                 "Blocking a bridge or a road",
                 "Blocking the construction\nof a pipeline",
                 "Vandalizing objects",
                 "Sabotaging infrastructure,\nvehicles, etc.",
                 "Throwing an object at\ninfrastructure, vehicles, etc.",
                 "Confronting police officers\nin a demonstration",
                 "Violating individuals in\npositions of power")


names(clean_names) <- c("signPetition",
                        "boycott",
                        "divest",
                        "manifestation",
                        "occupyPublicSpace",
                        "attachTreeVehicule",
                        "blockBridgeRoad",
                        "blockPipelineConstruction",
                        "vandalismObjects",
                        "sabotagingInfrastructure",
                        "throwingObjectsInfrastructure",
                        "fightPolice", "violatingPowerful")

graph_data <- preds %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(estimate),
         group = groups[item],
         group = factor(group,
                        levels = c("Property\ndestruction",
                                   "Violent",
                                   "Nonviolent,\ndisruptive",
                                   "Nonviolent,\nnon-disruptive")),
         item = clean_names[item],
         item = factor(item),
         item = fct_reorder(.f = item,
                            .x = mean_item,
                            .desc = T)) %>% 
  mutate(age_cat = factor(age_cat,
                          levels = c("all", "18", "23", "28", "33+")))


ggplot(graph_data, aes(x = estimate, y = item,
                  color = factor(scale_gravity),
                  size = age_cat,
                  shape = age_cat,
                  group = age_cat)) +
  geom_rect(data = subset(graph_data, group == 'Property\ndestruction'),
            fill = "grey85", xmin = -2,xmax = 2,
            ymin = -10,ymax = 10) +
  geom_rect(data = subset(graph_data, group == 'Violent'),
            fill = "grey90", xmin = -1.5,xmax = 1.5,
            ymin = -10,ymax = 10) +
  geom_rect(data = subset(graph_data, group == 'Nonviolent,\ndisruptive'),
            fill = "grey95", xmin = -1.5,xmax = 1.5,
            ymin = -10,ymax = 10) +
  facet_grid(rows = vars(group),
             cols = vars(facet),
             scales = "free_y",
             switch = "y") +
  geom_linerange(alpha = 0.5,
                 aes(xmin = conf.low, xmax = conf.high),
                 position = ggstance::position_dodgev(height = 0.75),
                 linewidth = 0.5) +
  geom_point(position =     ggstance::position_dodgev(height = 0.75)) +
  scale_color_manual(values = c("0" = "grey70", "1" = "black"),
                     labels = c("0" = "Low", "1" = "High"),
                     name = "Environmental Preoccupation") +
  #scale_fill_manual(values = c("0" = "grey70", "1" = "black")) +
  scale_size_manual(values = c("all" = 2.5, "18" = 1.5, "23" = 1.5, "28" = 2, "33+" = 1.5)) +
  scale_shape_manual(values = c("all" = 16, "18" = 15, "23" = 17, "28" = 18, "33+" = 16),
                     labels = c("all" = "All", "18" = "18-22", "23" = "23-27", "28" = "28-32", "33+" = "33+"),
                     name = "Age group") +
  xlab("\nPredicted tolerance\n") +
  ylab("") +
  labs(caption = "Predicted tolerance by linear regression models when keeping other independent variables constant") +
  guides(size = "none") +
  clessnize::theme_clean_light() + 
  theme(legend.title = element_text(),
        legend.box="vertical",
        strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/new/2_env_preo.png",
       width = 10, height = 8)

