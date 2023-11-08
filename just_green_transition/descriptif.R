# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data_with_clusters.rds")

# Carbon dioxide ---------------------------------------------------------------

## wrangling ---------------------------------------------------------------

carbon <- Data %>% 
  rename(stateInterv_continueIncreaseGESPrice_base = stateInterv_continueIncreaseGESPrice) %>% 
  pivot_longer(., cols = starts_with("stateInterv_continueIncreaseGESPrice"),
               names_to = "distribution",
               values_to = "scale",
               names_prefix = "stateInterv_continueIncreaseGESPrice_") %>% 
  group_by(distribution, scale) %>% 
  summarise(n = n()) %>% 
  group_by(distribution) %>% 
  mutate(distribution = case_when(
           distribution == "base" ~ "Like the existing\nsystem in Canada",
           distribution == "RedistributedPop" ~ "Funds distributed\nback to the population",
           distribution == "GreenJobs" ~ "Funds used to create\njobs in green energy",
           distribution == "FossilJobs" ~ "Funds used to support\nfossil fuels workers"
         ),
         ndist = sum(n),
         prop = n/ndist,
         force = case_when(
           scale %in% c(0, 1) ~ "Strongly",
           scale %in% c(0.25, 0.75) ~ "Somewhat",
           scale == 0.5 ~ "Neutral"
         ),
         force = factor(force, levels = c("Somewhat",
                                          "Neutral",
                                          "Strongly")),
         accord = case_when(
           scale %in% c(0, 0.25) ~ "disagree",
           scale %in% c(0.5) ~ "Neutral",
           scale %in% c(0.75, 1) ~ "agree"
         ),
         accord = factor(accord, levels = c("disagree", "Neutral", "agree")),
         label = paste0(force, " ", accord),
         label = ifelse(label == "Neutral Neutral", "Neutral", label),
         label = factor(label, levels = c("Strongly disagree", "Somewhat disagree", 
                                          "Neutral", "Somewhat agree", "Strongly agree"))) %>% 
  arrange(distribution, -scale) %>% 
  group_by(distribution) %>% 
  mutate(cumsum_prop = cumsum(prop),
         scale = factor(scale, levels = c(0, 0.25, 0.5, 0.75, 1)))


ggplot(carbon, aes(x = distribution, y = prop*100, fill = label, alpha = label)) +
  geom_bar(stat = "identity", position = "stack", aes(group = scale)) +
  geom_text(aes(label = round(prop*100), group = scale),
            position = position_stack(vjust = 0.5),
            alpha = 1,
            size = 3) +
  scale_alpha_manual(values = c("Strongly disagree" = 1, 
                                "Somewhat disagree" = 0.5,
                                "Neutral" = 0.5, 
                                "Somewhat agree" = 0.5, 
                                "Strongly agree" = 1)) +
  scale_fill_manual(values = c("Strongly disagree" = "#D4342E", 
                               "Somewhat disagree" = "#D4342E",
                               "Neutral" = "grey", 
                               "Somewhat agree" = "#2E8B57", 
                               "Strongly agree" = "#2E8B57")) +
  ylab("Proportion of the sample (%)") +
  xlab("") +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light()

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/explo/carb_all.png",
       width = 9, height = 7)



#### second try

carbon %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = label,
             color = label, alpha = label)) +
  geom_point(aes(size = prop),
             shape = 15, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(prop*100), "%")),
            alpha = 1,
            size = 3, color = "black") +
  scale_size_continuous(range = c(7.5, 40)) +
  scale_alpha_manual(values = c("Strongly disagree" = 1, 
                                "Somewhat disagree" = 0.5,
                                "Neutral" = 0.5, 
                                "Somewhat agree" = 0.5, 
                                "Strongly agree" = 1)) +
  scale_color_manual(values = c("Strongly disagree" = "#D4342E", 
                               "Somewhat disagree" = "#D4342E",
                               "Neutral" = "grey", 
                               "Somewhat agree" = "#2E8B57", 
                               "Strongly agree" = "#2E8B57")) +
  ylab("") +
  xlab("") +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light()

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/explo/carb_all2.png",
       width = 9, height = 7)


# By province -------------------------------------------------------------

carbon <- Data %>% 
  mutate(region = case_when(
    ses_prov %in% c("alberta", "sk") ~ "Alberta and Saskatchewan",
    ses_prov %in% c("cb") ~ "British Columbia",
    ses_prov %in% c("on") ~ "Ontario",
    ses_prov %in% c("qc") ~ "Quebec",
  )) %>% 
  drop_na(region) %>% 
  rename(stateInterv_continueIncreaseGESPrice_base = stateInterv_continueIncreaseGESPrice) %>% 
  pivot_longer(., cols = starts_with("stateInterv_continueIncreaseGESPrice"),
               names_to = "distribution",
               values_to = "scale",
               names_prefix = "stateInterv_continueIncreaseGESPrice_") %>% 
  group_by(distribution, region, scale) %>% 
  summarise(n = n()) %>% 
  group_by(distribution, region) %>% 
  mutate(distribution = case_when(
    distribution == "base" ~ "Like the existing\nsystem in Canada",
    distribution == "RedistributedPop" ~ "Funds distributed\nback to the population",
    distribution == "GreenJobs" ~ "Funds used to create\njobs in green energy",
    distribution == "FossilJobs" ~ "Funds used to support\nfossil fuels workers"
  ),
  ndist = sum(n),
  prop = n/ndist,
  force = case_when(
    scale %in% c(0, 1) ~ "Strongly",
    scale %in% c(0.25, 0.75) ~ "Somewhat",
    scale == 0.5 ~ "Neutral"
  ),
  force = factor(force, levels = c("Somewhat",
                                   "Neutral",
                                   "Strongly")),
  accord = case_when(
    scale %in% c(0, 0.25) ~ "disagree",
    scale %in% c(0.5) ~ "Neutral",
    scale %in% c(0.75, 1) ~ "agree"
  ),
  accord = factor(accord, levels = c("disagree", "Neutral", "agree")),
  label = paste0(force, " ", accord),
  label = ifelse(label == "Neutral Neutral", "Neutral", label),
  label = factor(label, levels = c("Strongly disagree", "Somewhat disagree", 
                                   "Neutral", "Somewhat agree", "Strongly agree"))) %>% 
  arrange(distribution, region, -scale) %>% 
  group_by(distribution, region) %>% 
  mutate(cumsum_prop = cumsum(prop),
         scale = factor(scale, levels = c(0, 0.25, 0.5, 0.75, 1)))


carbon %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = label,
             color = label, alpha = label)) +
  facet_wrap(~region) +
  geom_point(aes(size = prop),
             shape = 15, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(prop*100), "%")),
            alpha = 1,
            size = 2.25, color = "black") +
  scale_size_continuous(range = c(7, 30)) +
  scale_alpha_manual(values = c("Strongly disagree" = 1, 
                                "Somewhat disagree" = 0.5,
                                "Neutral" = 0.5, 
                                "Somewhat agree" = 0.5, 
                                "Strongly agree" = 1)) +
  scale_color_manual(values = c("Strongly disagree" = "#D4342E", 
                                "Somewhat disagree" = "#D4342E",
                                "Neutral" = "grey", 
                                "Somewhat agree" = "#2E8B57", 
                                "Strongly agree" = "#2E8B57")) +
  ylab("") +
  xlab("") +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light()

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/explo/carb_prov2.png",
       width = 12, height = 10)

