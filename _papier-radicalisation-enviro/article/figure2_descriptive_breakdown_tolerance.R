# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("_papier-radicalisation-enviro/article/environment.R")

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds") %>% 
  filter(ses_age >= 18) %>% 
  mutate(age_cat = get_age_category(ses_age),
         ses_region = case_when(
           ses_region_qc == 1 ~ "Quebec",
           ses_region_ont == 1 ~ "Ontario",
           ses_region_west == 1 ~ "Western provinces",
           ses_region_mari == 1 ~ "Maritimes"
         )) %>% 
  select(age_cat, starts_with("ses_educ"), ses_region,
         starts_with("radicalisation_tolerate")) %>% 
  tidyr::pivot_longer(., cols = c(starts_with("radicalisation_tolerate")),
                      names_to = "action",
                      names_prefix = "radicalisation_tolerate_",
                      values_to = "tolerance") %>% 
  mutate(action_group = groups[action])

# Age --------------------------------

data_group_age <- data %>% 
  filter(age_cat < 70) %>% 
  group_by(age_cat, action_group) %>%
  summarise(mean_tolerance = mean(tolerance, na.rm = T),
            sd = sd(tolerance, na.rm = T),
            n = n()) %>% 
  mutate(me = margin_error(mean_tolerance, sd, n),
         conf_low = mean_tolerance - me,
         conf_high = mean_tolerance + me,
         action_group = factor(action_group, levels = c("Nonviolent,\nnon-disruptive", "Nonviolent,\ndisruptive", "Violence and\nmaterial destruction")))

plot_age <- ggplot(data_group_age, aes(x = mean_tolerance, y = age_cat * -1)) +
  facet_wrap(~action_group, ncol = 1,
             strip.position = "left") +
  geom_rect(data = subset(data_group_age, action_group == 'Violence and\nmaterial destruction'),
            fill = "grey80", xmin = -2,xmax = 2,
            ymin = -100,ymax = 100) +
  geom_rect(data = subset(data_group_age, action_group == 'Nonviolent,\ndisruptive'),
            fill = "grey90", xmin = -1.5,xmax = 1.5,
            ymin = -100,ymax = 100) +
  geom_rect(data = subset(data_group_age, action_group == 'Nonviolent,\nnon-disruptive'),
            fill = "white", xmin = -1.5,xmax = 1.5,
            ymin = -100,ymax = 100) +
  geom_point() +
  geom_linerange(aes(xmin = conf_low, xmax = conf_high)) +
  scale_y_continuous(name = "\nAge (grouped by 5 years)\n",
                     labels = c(abs(seq(from = -18, to = -70, by = -5))),
                     breaks = seq(from = -18, to = -70, by = -5),
                     expand = c(0.1, 0.1)) +
  scale_x_continuous(breaks = c(0, 0.33, 0.67, 1),
                     limits = c(0, 1),
                     labels = c("None", "Low", "Medium", "High"),
                     name = "Mean tolerance") +
  clessnize::theme_clean_light(base_size = 15) +
  theme(strip.background.x = element_rect(fill = "#F2F2F2", color = NA),
        strip.background.y = element_blank(),
        strip.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 7),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 7.5))

# Educ --------------------------------------------------------------------

data_group_educ <- data %>% 
  mutate(educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5,
                             educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1,
                             educ_level)) %>% 
  group_by(educ_level, action_group) %>%
  summarise(mean_tolerance = mean(tolerance, na.rm = T),
            sd = sd(tolerance, na.rm = T),
            n = n()) %>% 
  mutate(me = margin_error(mean_tolerance, sd, n),
         conf_low = mean_tolerance - me,
         conf_high = mean_tolerance + me,
         action_group = factor(action_group, levels = c("Nonviolent,\nnon-disruptive", "Nonviolent,\ndisruptive", "Violence and\nmaterial destruction")))

plot_educ <- ggplot(data_group_educ, aes(x = mean_tolerance, y = educ_level)) +
  facet_wrap(~action_group, ncol = 1,
             strip.position = "right") +
  geom_rect(data = subset(data_group_educ, action_group == 'Violence and\nmaterial destruction'),
            fill = "grey80", xmin = -2,xmax = 2,
            ymin = -100,ymax = 100) +
  geom_rect(data = subset(data_group_educ, action_group == 'Nonviolent,\ndisruptive'),
            fill = "grey90", xmin = -1.5,xmax = 1.5,
            ymin = -100,ymax = 100) +
  geom_rect(data = subset(data_group_educ, action_group == 'Nonviolent,\nnon-disruptive'),
            fill = "white", xmin = -1.5,xmax = 1.5,
            ymin = -100,ymax = 100) +
  geom_point() +
  geom_linerange(aes(xmin = conf_low, xmax = conf_high)) +
  scale_y_continuous(name = "\nEducation level\n",
                     labels = c("High-school", "College", "University"),
                     breaks = c(0, 0.5, 1),
                     expand = c(0.1, 0.1)) +
  scale_x_continuous(breaks = c(0, 0.33, 0.67, 1),
                     limits = c(0, 1),
                     labels = c("None", "Low", "Medium", "High"),
                     name = "Mean tolerance") +
  #labs(caption = "The error bars represent the 95% confidence interval.\nDue to large error margins, respondents over 70 years old were removed from the visualisation.") +
  clessnize::theme_clean_light(base_size = 15) +
  theme(strip.background.x = element_rect(fill = "#F2F2F2", color = NA),
        strip.background.y = element_blank(),
        strip.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 7.5))

# Region ------------------------------------------------------------------

data_group_region <- data %>% 
  group_by(ses_region, action_group) %>%
  summarise(mean_tolerance = mean(tolerance, na.rm = T),
            sd = sd(tolerance, na.rm = T),
            n = n()) %>% 
  mutate(me = margin_error(mean_tolerance, sd, n),
         conf_low = mean_tolerance - me,
         conf_high = mean_tolerance + me,
         action_group = factor(action_group, levels = c("Nonviolent,\nnon-disruptive", "Nonviolent,\ndisruptive", "Violence and\nmaterial destruction")))

plot_region <- ggplot(data_group_region, aes(x = mean_tolerance, y = ses_region)) +
  facet_wrap(~action_group, ncol = 1,
             strip.position = "left") +
  geom_rect(data = subset(data_group_region, action_group == 'Violence and\nmaterial destruction'),
            fill = "grey80", xmin = -2,xmax = 2,
            ymin = -100,ymax = 100) +
  geom_rect(data = subset(data_group_region, action_group == 'Nonviolent,\ndisruptive'),
            fill = "grey90", xmin = -1.5,xmax = 1.5,
            ymin = -100,ymax = 100) +
  geom_rect(data = subset(data_group_region, action_group == 'Nonviolent,\nnon-disruptive'),
            fill = "white", xmin = -1.5,xmax = 1.5,
            ymin = -100,ymax = 100) +
  geom_point() +
  geom_linerange(aes(xmin = conf_low, xmax = conf_high)) +
  scale_y_discrete(name = "\nRegion\n",
                     expand = c(0.1, 0.1)) +
  scale_x_continuous(breaks = c(0, 0.33, 0.67, 1),
                     limits = c(0, 1),
                     labels = c("None", "Low", "Medium", "High"),
                     name = "\nMean tolerance") +
  #labs(caption = "The error bars represent the 95% confidence interval.\nDue to large error margins, respondents over 70 years old were removed from the visualisation.") +
  clessnize::theme_clean_light(base_size = 15) +
  theme(strip.background.x = element_rect(fill = "#F2F2F2", color = NA),
        strip.background.y = element_blank(),
        strip.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 7.5))


# Merge graphs ------------------------------------------------------------

top_row <- cowplot::plot_grid(plot_age, plot_educ, ncol = 2)
bottom_row <- cowplot::plot_grid(NULL, plot_region, NULL, ncol = 3, rel_widths = c(1, 2.5, 1),
                                 axis = "tblr")

# Combinez les rangées en une seule figure
combined_plot <- cowplot::plot_grid(top_row, NULL, bottom_row,
                                    ncol = 1, rel_heights = c(1, 0.07, 1))

plot <- cowplot::ggdraw() +
  cowplot::draw_plot(combined_plot, 0, 0.1, 1, 0.9) +
  cowplot::draw_label("Mean tolerance",
             x = 0.5, y = 0.075, hjust = 0.5,
             size = 15) +
  cowplot::draw_label("The error bars represent the 95% confidence interval.\nDue to large error margins, respondents over 70 years old were removed from the age visualisation.", 
             x = 0.05, y = 0.025, hjust = 0, size = 9, fontface = "italic") +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave(plot, filename = "_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/figure2_descriptive_breakdown_tolerance.png",
       width = 10, height = 9)

