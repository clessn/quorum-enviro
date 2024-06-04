# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("_papier-radicalisation-enviro/article/environment.R")

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds") %>% 
  filter(ses_age >= 18) %>% 
  mutate(ses_region = case_when(
    ses_region_qc == 1 ~ "Quebec",
    ses_region_ont == 1 ~ "Ontario",
    ses_region_west == 1 ~ "Western provinces",
    ses_region_mari == 1 ~ "Maritimes"
    ),
    ses_region = factor(ses_region),
    age_cat = get_age_category2(ses_age),
    educ_level = ifelse(ses_educBHS == 1, 0, NA),
    educ_level = ifelse(ses_educCollege == 1, 0.5,
                        educ_level),
    educ_level = ifelse(ses_educUniv == 1, 1,
                        educ_level),
    responsibility_climateChange_Govt = (responsability_climateChangeFedGovt + responsability_climateChangeProvGovt) / 2) %>% 
  select(age_cat, ses_region, ses_gender_male,
         educ_level, ses_incomeHigh, ses_incomeLow, ses_bornCanada,
         responsibility_climateChange_Govt, responsability_climateChangeEnterprise,
         responsability_climateChangeCitizens,
         starts_with("radicalisation_tolerate"))


# Model -------------------------------------------------------------------

vds <- names(data %>% 
               select(starts_with("radicalisation_tolerate")))
z_value = qnorm(0.975)

for (i in 1:length(vds)){
  vd <- vds[i]
  model <- eval(parse(text = paste0("lm(", vd, " ~ age_cat +
                 ses_gender_male + educ_level +
                 ses_incomeHigh + ses_incomeLow + ses_region + ses_bornCanada +
                 responsibility_climateChange_Govt + responsability_climateChangeEnterprise +
                 responsability_climateChangeCitizens,
                 data = data)")))
  model_coefficients <- data.frame(summary(model)$coefficients[,1:2]) %>% 
    mutate(vi = rownames(.),
           item = vd,
           me = z_value * Std..Error,
           conf_low = Estimate - me,
           conf_high = Estimate + me) %>% 
    filter(vi %in% c("responsibility_climateChange_Govt",
                     "responsability_climateChangeEnterprise",
                     "responsability_climateChangeCitizens")) %>% 
    select(item, vi, estimate = Estimate, conf_low, conf_high)
  rownames(model_coefficients) <- NULL
  if (i == 1){
    model_coefs <- model_coefficients
  } else {
    model_coefs <- rbind(model_coefs, model_coefficients)
  }
  message(vd)
}

model_coefs$item <- gsub("radicalisation_tolerate_", "", model_coefs$item)

# Graph -------------------------------------------------------------------

items_mean <- data %>% 
  tidyr::pivot_longer(
    cols = starts_with("radicalisation_tolerate"),
    names_to = "item"
  ) %>% 
  mutate(item = gsub("radicalisation_tolerate_", "", item),
         item = clean_names[item]) %>% 
  group_by(item) %>%
  summarise(mean_item = mean(value))
  
graph_data <- model_coefs %>%
  mutate(group = groups[item],
         group = factor(group,
                        levels = rev(c("Violent",
                                       "Property\ndestruction",
                                       "Nonviolent,\ndisruptive",
                                       "Nonviolent,\nnon-disruptive"))),
         item = clean_names[item],
         vi = clean_vis[vi],
         vi = factor(vi,
                     levels = c("Citizens",
                                "Enterprises",
                                "Governments")),
         significative = ifelse(sign(estimate) == sign(conf_low) &
                                  sign(estimate) == sign(conf_high),
                                "Significant",
                                "Not significant")) %>% 
  left_join(items_mean, by = "item")

ggplot(data = graph_data,
       aes(x = estimate,
           y = reorder(item, -mean_item))) +
  facet_grid(rows = vars(group),
             cols = vars(vi),
             scales = "free_y",
             switch = "y") +
  geom_rect(data = subset(graph_data, group == 'Violent'),
            fill = "grey85", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(graph_data, group == 'Property\ndestruction'),
            fill = "grey90", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(graph_data, group == 'Nonviolent,\ndisruptive'),
            fill = "grey95", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(graph_data, group == 'Nonviolent,\nnon-disruptive'),
            fill = "white", xmin = -1.5,xmax = 1.5,
            ymin = -Inf,ymax = Inf) +
  scale_x_continuous(limits = c(-0.75, 0.75),
                     breaks = c(-0.5, 0, 0.5)) +
  ylab("") +
  xlab("\nRegression Coefficient\n(Tolerance as DV)") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  geom_point(aes(color = significative), size = 1.5) +
  geom_linerange(aes(xmin = conf_low, xmax = conf_high,
                     color = significative),
                 size = 0.5) +
  clessnize::theme_clean_light() +
  scale_color_manual(values = c("Significant" = "black",
                                "Not significant" = "grey75"),
                     breaks = c("Significant", "Not significant"),
                     name = "Significance at 95% confidence interval") +
  theme(strip.background.x = element_rect(fill = "#F2F2F2", color = NA),
        strip.background.y = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 7),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 7.5))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/dans_article/figure5_locus_responsibility.png",
       width = 7.5, height = 6, dpi = 300)
