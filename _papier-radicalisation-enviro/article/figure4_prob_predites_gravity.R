# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("_papier-radicalisation-enviro/article/environment.R")

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds") %>%
  filter(ses_age >= 18) %>% 
  mutate(educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5,
                             educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1,
                             educ_level),
         age_cat = get_age_category2(ses_age),
         age_cat = factor(age_cat, levels = c(18, 28, 38, 48, 58, 68, 78, 88)),
         ses_region = case_when(
           ses_region_qc == 1 ~ "Quebec",
           ses_region_ont == 1 ~ "Ontario",
           ses_region_west == 1 ~ "Western provinces",
           ses_region_mari == 1 ~ "Maritimes"
         ),
         ses_region = factor(ses_region))

table(data$age_cat)

# Models -------------------------------------------------------------------

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
  model <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity + age_cat +
                 ses_gender_male + educ_level +
                 ses_incomeHigh + ses_incomeLow + ses_region + ses_bornCanada,
                 data = data)")))
  predsi <- marginaleffects::predictions(
    model = model,
    newdata = marginaleffects::datagrid(
      scale_gravity = c(0, 0.5, 1)
    )) %>% 
    mutate(item = vd) %>% 
    select(item, scale_gravity, estimate, conf.low, conf.high)
  if (i == 1){
    preds <- predsi
  } else {
    preds <- rbind(preds, predsi)
  }
  message(vd)
}

# Graph -------------------------------------------------------------------
graph_data <- preds %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(estimate),
         action = item,
         group = groups[item],
         group = factor(group,
                        levels = rev(c("Violent",
                                       "Property\ndestruction",
                                       "Nonviolent,\ndisruptive",
                                       "Nonviolent,\nnon-disruptive"))),
         item = clean_names[item],
         item = factor(item),
         item = forcats::fct_reorder(.f = item,
                                     .x = mean_item,
                                     .desc = T)) %>% 
  mutate(action_group = groups[action],
         scale_gravity = case_when(
           scale_gravity == 0 ~ "Low",
           scale_gravity == 0.5 ~ "Medium",
           scale_gravity == 1 ~ "High"
         ),
         scale_gravity = factor(scale_gravity,
                                levels = c("Low", "Medium", "High")),
         estimate = ifelse(estimate < 0, 0, estimate),
         estimate = ifelse(estimate > 1, 1, estimate),
         conf.low = ifelse(conf.low < 0, 0, conf.low),
         conf.high = ifelse(conf.high > 1, 1, conf.high))


ggplot(graph_data, aes(x = estimate, y = item,
                       group = scale_gravity)) +
  facet_wrap(~group, ncol = 1, scales = "free_y",
             strip.position = "left") +
  geom_rect(data = subset(graph_data, group == 'Violent'),
            fill = "grey85", xmin = -1.5,xmax = 1.5,
            ymin = -15,ymax = 15) +
  geom_rect(data = subset(graph_data, group == 'Property\ndestruction'),
            fill = "grey90", xmin = -2,xmax = 2,
            ymin = -15,ymax = 15) +
  geom_rect(data = subset(graph_data, group == 'Nonviolent,\ndisruptive'),
            fill = "grey95", xmin = -1.5,xmax = 1.5,
            ymin = -15,ymax = 15) +
  geom_rect(data = subset(graph_data, group == 'Nonviolent,\nnon-disruptive'),
            fill = "white", xmin = -1.5,xmax = 1.5,
            ymin = -15,ymax = 15) +
  geom_linerange(alpha = 1,
                 aes(xmin = conf.low, xmax = conf.high,
                     color = scale_gravity),
                 linewidth = 0.5,
                 position = ggstance::position_dodge2v(height = 0.4)) +
  geom_point(aes(color = scale_gravity),
             position = ggstance::position_dodge2v(height = 0.4)) +
  scale_color_manual(name = "Environmental Preoccupation",
                     values = c("Low" = "grey70", "Medium" = "grey35", "High" = "black"),
                     labels = c("Low" = "Low", "Medium" = "Medium", "High" = "High")) +
  xlab("\nPredicted tolerance\n") +
  scale_x_continuous(breaks = c(0, 0.33, 0.67, 1),
                     limits = c(0, 1),
                     labels = c("None", "Low", "Medium", "High"),
                     expand = c(0.01, 0.01)) +
  ylab("") +
  labs(caption = "Predicted tolerance by linear regression models when keeping other independent variables constant") +
  clessnize::theme_clean_light() + 
  theme(legend.title = element_text(),
        legend.box="vertical",
        strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/figure4_prob_predites_gravity.png",
       width = 10, height = 8)



