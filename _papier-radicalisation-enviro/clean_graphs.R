# Packages ####
library(tidyverse)
library(GGally)
library(ggridges)
library(fastDummies)

source("functions.R", encoding = "UTF-8")

# Data ####
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

# Conceptual groups ####

# d: non violent, non-disruptive actions
# c: non violent, disruptive actions
# b: property destruction
# a: violence

sysfonts::font_add_google("Roboto")
showtext::showtext_auto()

groups <- c("Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive",
            "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive",
            "Property\ndestruction", "Property\ndestruction", "Property\ndestruction",
            "Violent", "Violent")
names(groups) <- c("signPetition", "boycott", "divest", "manifestation", "occupyPublicSpace",
            "attachTreeVehicule", "blockBridgeRoad", "blockPipelineConstruction", "vandalismObjects",
            "sabotagingInfrastructure", "throwingObjectsInfrastructure", "fightPolice", "violatingPowerful")
groups


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

# Figure 1: overall distribution ####
density <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(tolerance),
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
                            .desc = T))

#c("Nonviolent,\nnon-disruptive",
#  "Nonviolent,\ndisruptive",
#  "Violent",
#  "Property\ndestruction")


ggplot(density, aes(x = tolerance,
                    y = reorder(item, -mean_item))) +
  geom_rect(data = subset(density, group == 'Property\ndestruction'),
            fill = "#D4D4D4", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(density, group == 'Violent'),
            fill = "#E4E4E4", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(density, group == 'Nonviolent,\ndisruptive'),
            fill = "#F3F3F3", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf) +
  geom_density_ridges(scale = 0.9, bandwidth = 0.15,
                      color = NA, fill = "black",
                      vline_color = "black",
                      alpha = 0.7, quantile_lines=T,
                      quantile_fun=function(x,...)mean(x)) +
  facet_wrap(~group, ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(lineheight = 0.3),
        axis.text.y = element_text(lineheight = 0.3),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  scale_x_continuous(limits = c(-0.4, 1.4),
                     breaks = c(-0.25, 1.25),
                     labels = c("Not tolerated", "Tolerated")) +
  ylab("") +
  xlab("")

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/fig1_overall_distribution.png",
       width = 6, height = 7)


# Figure 2: gravity selon age ####


# Figure 3: complete models ####

#### Dans models.rmd

# Figure 4: graphique de regression ####

## Prepare independent variables
get_age_category <- function(ages){
  groups <- seq(from = 13,
                to = max(Data$ses_age, na.rm = T),
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

Data2 <- Data %>% 
  mutate(educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5, educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1, educ_level),
         responsability = responsability_citizensVsGvnt,
         over35yo = ifelse(ses_age >= 35, 1, 0),
         age_cat = get_age_category(ses_age),
         age_cat_norm = minmaxNormalization(age_cat),
         ses_age = minmaxNormalization(ses_age)) %>% 
  fastDummies::dummy_columns(.,
                             select_columns = c("age_cat"))

Data2 %>% 
  filter(age_cat != 13) %>% 
  group_by(age_cat) %>% 
  summarise(n = n(),
            mean_gravity = mean(scale_gravity))

vds <- names(Data2 %>% 
               select(starts_with("radicalisation_tolerate")))
names(Data2) <- gsub("radicalisation_tolerate_",
                     "",
                     names(Data2))
vds <- gsub("radicalisation_tolerate_",
            "",
            vds)


GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)

for (i in 1:length(vds)){
  #i <- 1
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity*age_cat_18 + scale_gravity*age_cat_23 +
                 scale_gravity*age_cat_28 + ses_gender_male + educ_level +
                 ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

clean_vis <- c("Environmental\npreoccupation (EP)",
               "EP * 18-22 yo",
               "EP * 23-27 yo",
               "EP * 28-32 yo")
names(clean_vis) <- c("scale_gravity",
                      "scale_gravity:age_cat_18",
                      "scale_gravity:age_cat_23",
                      "scale_gravity:age_cat_28")

Graph <- GraphData %>% 
  filter(vi %in% c("scale_gravity",
                   "scale_gravity:age_cat_18",
                   "scale_gravity:age_cat_23",
                   "scale_gravity:age_cat_28")) %>% 
  mutate(group = groups[item],
         group = factor(group,
                        levels = c("Nonviolent,\nnon-disruptive",
                                   "Nonviolent,\ndisruptive",
                                   "Violent",
                                   "Property\ndestruction")),
         item = clean_names[item],
         item = factor(item),
         item = fct_reorder(.f = item,
                            .x = mean_tol,
                            .desc = T),
         sign = as.character(ifelse(pval <= 0.05, "Significative", "Non-significative")),
         vi = clean_vis[vi])

#c("Property\ndestruction",
#  "Violent",
#  "Nonviolent,\ndisruptive",
#  "Nonviolent,\nnon-disruptive")


ggplot(data = Graph,
       aes(x = coef,
           y = reorder(item, -mean_tol),
           alpha = sign)) +
  geom_rect(data = subset(Graph, group == 'Property\ndestruction'),
            fill = "#C9C9C9", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf, alpha = 0.4) +
  geom_rect(data = subset(Graph, group == 'Violent'),
            fill = "#C9C9C9", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_rect(data = subset(Graph, group == 'Nonviolent,\ndisruptive'),
            fill = "#C9C9C9", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf, alpha = 0.05) +
  facet_grid(rows = vars(group),
             cols = vars(vi),
             scales = "free_y",
             switch = "y") +
  scale_x_continuous(limits = c(-0.75, 0.75),
                     breaks = c(-0.5, 0, 0.5)) +
  ylab("") +
  xlab("\nRegression coefficient") +
#  facet_wrap(~vi, nrow = 1) +
  geom_point(  color = "#595959", size = 1.5) +
  geom_segment(color = "#595959", size = 1, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#595959", size = 1, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  clessnverse::theme_clean_light(base_size = 30) +
  #clessnverse::theme_clean_light() +
  theme(strip.background.x = element_rect(fill = "#F2F2F2", color = NA),
        strip.background.y = element_blank(),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2),
        axis.text.y = element_text(lineheight = 0.3),
        #strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(lineheight = 0.3),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

#ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/fig4_coefficientsInteractions.png",
#       width = 10, height = 7)

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/fig4_coefficientsInteractions.png",
       width = 6.5, height = 7)

