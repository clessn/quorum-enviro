# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("_papier-radicalisation-enviro/article/environment.R")

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds") %>%
  filter(ses_age >= 18) %>% 
  tidyr::pivot_longer(cols = starts_with("radicalisation_tolerate"),
                      names_to = "item", values_to = "tolerance",
                      names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(tolerance),
         group = groups[item],
         group = factor(group,
                        levels = rev(c("Violent,\ndisruptive",
                                       "Nonviolent,\ndisruptive",
                                       "Nonviolent,\nnon-disruptive"))),
         item = clean_names[item],
         item = factor(item),
         item = forcats::fct_reorder(.f = item,
                                     .x = mean_item,
                                     .desc = T))

# Graph -------------------------------------------------------------------

ggplot(data, aes(x = tolerance,
                    y = reorder(item, mean_item))) +
  facet_wrap(~group, ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  geom_rect(data = subset(data, group == "Violent,\ndisruptive"),
            fill = "grey90", xmin = -1.5,xmax = 1.5,
            ymin = -15,ymax = 15) +
  geom_rect(data = subset(data, group == 'Nonviolent,\ndisruptive'),
            fill = "grey95", xmin = -1.5,xmax = 1.5,
            ymin = -15,ymax = 15) +
  geom_rect(data = subset(data, group == 'Nonviolent,\nnon-disruptive'),
            fill = "white", xmin = -1.5,xmax = 1.5,
            ymin = -15,ymax = 15) +
  ggridges::geom_density_ridges(scale = 0.9, bandwidth = 0.15,
                                color = NA, fill = "black",
                                vline_color = "black",
                                alpha = 0.7, quantile_lines=T,
                                quantile_fun=function(x,...)mean(x)) +
  clessnize::theme_clean_light() +
  ylab("") +
  xlab("\nTolerance level") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  scale_x_continuous(limits = c(-0.32, 1.32),
                     breaks = c(0, 0.33, 0.67, 1),
                     labels = c("None", "Low", "Medium", "High"))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/figure1_densities_all.png",
       width = 8, height = 7, dpi = 300)

