# Packages ----------------------------------------------------------------
library(tidyverse)
library(ggpattern)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data_with_clusters.rds")

# Carbon dioxide ---------------------------------------------------------------

## All ---------------------------------------------------------------------

### wrangling ---------------------------------------------------------------

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
  distribution = factor(distribution,
                        levels = c("Like the existing\nsystem in Canada",
                                   "Funds distributed\nback to the population",
                                   "Funds used to create\njobs in green energy",
                                   "Funds used to support\nfossil fuels workers")),
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
    scale %in% c(0, 0.25) ~ "in disfavor",
    scale %in% c(0.5) ~ "Neutral",
    scale %in% c(0.75, 1) ~ "in favor"
  ),
  accord = factor(accord, levels = c("in disfavor", "Neutral", "in favor")),
  accord_label = case_when(
    accord == "in disfavor" ~ "In disfavor",
    accord == "Neutral" ~ "Neutral",
    accord == "in favor" ~ "In favor"
  ),
  accord_label = factor(accord_label, levels = c("In disfavor", "Neutral", "In favor")),
  label = paste0(force, " ", accord),
  label = ifelse(label == "Neutral Neutral", "Neutral", label),
  label = factor(label, levels = c("Strongly in disfavor", "Somewhat in disfavor", 
                                   "Neutral", "Somewhat in favor", "Strongly in favor"))) %>% 
  arrange(distribution, -scale) %>% 
  group_by(distribution) %>% 
  mutate(cumsum_prop = cumsum(prop),
         scale = factor(scale, levels = c(0, 0.25, 0.5, 0.75, 1)))

## data for dotted lines
dotted <- carbon %>% 
  filter(distribution == "Like the existing\nsystem in Canada") %>% 
  arrange(accord, desc(force)) %>% 
  group_by(accord) %>% 
  mutate(yintercept = cumsum(prop))


### Graph -------------------------------------------------------------------

#### color
carbon %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = prop*100,
             fill = label)) +
  facet_wrap(~accord_label, ncol = 1,
             strip.position = "right") +
  geom_bar(stat = "identity", color = NA,
           aes(group = force)) +
  geom_hline(data = dotted,
             aes(yintercept = yintercept*100),
             linetype = "dotted",
             linewidth = 0.6,
             color = "black",
             alpha = 0.75) +
  geom_label(aes(label = round(prop*100), group = force),
             position = position_stack(vjust = 0.5),
             size = 3, label.size = 0,
             label.padding = unit(0.1, "cm"),
             show.legend = FALSE) +
  labs(title = "Proportion of the sample in favor or in disfavor of the following question\ndepending on how the funds are used",
       subtitle = "Are you in favor of continuing to increase the price of carbon dioxide emissions?",
       caption = "n = 1500. Data collected from a survey conducted in August 2022 across Canada for all ages by the firm Synopsis.") +
  scale_fill_manual(values = c("Strongly in disfavor" = "#D4342E", 
                               "Somewhat in disfavor" = "#e99996",
                               "Neutral" = "lightgrey", 
                               "Somewhat in favor" = "#96c5ab", 
                               "Strongly in favor" = "#2E8B57")) +
  scale_color_manual(values = c("Strongly in disfavor" = "#D4342E", 
                                "Somewhat in disfavor" = "#e99996",
                                "Neutral" = "lightgrey", 
                                "Somewhat in favor" = "#96c5ab", 
                                "Strongly in favor" = "#2E8B57")) +
  ylab("Proportion of the sample (%)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5),
        panel.border = element_rect(fill = "NA", color = "lightgrey"),
        panel.spacing = unit(0.45, "cm"),
        plot.title = element_text(size = 13))

#ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/carb_all.png",
#       width = 8, height = 7)

## Regions -----------------------------------------------------------------

### wrangling ---------------------------------------------------------------

carbonregion <- Data %>% 
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
  distribution = factor(distribution,
                        levels = c("Like the existing\nsystem in Canada",
                                   "Funds distributed\nback to the population",
                                   "Funds used to create\njobs in green energy",
                                   "Funds used to support\nfossil fuels workers")),
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
    scale %in% c(0, 0.25) ~ "in disfavor",
    scale %in% c(0.5) ~ "Neutral",
    scale %in% c(0.75, 1) ~ "in favor"
  ),
  accord = factor(accord, levels = c("in disfavor", "Neutral", "in favor")),
  accord_label = case_when(
    accord == "in disfavor" ~ "In disfavor",
    accord == "Neutral" ~ "Neutral",
    accord == "in favor" ~ "In favor"
  ),
  accord_label = factor(accord_label, levels = c("In disfavor", "Neutral", "In favor")),
  label = paste0(force, " ", accord),
  label = ifelse(label == "Neutral Neutral", "Neutral", label),
  label = factor(label, levels = c("Strongly in disfavor", "Somewhat in disfavor", 
                                   "Neutral", "Somewhat in favor", "Strongly in favor"))) %>% 
  arrange(distribution, region, -scale) %>% 
  group_by(distribution, region) %>% 
  mutate(cumsum_prop = cumsum(prop),
         scale = factor(scale, levels = c(0, 0.25, 0.5, 0.75, 1)),
         region_label = paste0(region, "\nn = ", ndist))

## data for dotted lines
dottedregions <- carbonregion %>% 
  filter(distribution == "Like the existing\nsystem in Canada") %>% 
  arrange(region_label, accord, desc(force)) %>% 
  group_by(region_label, accord) %>% 
  mutate(yintercept = cumsum(prop))


### Graph -----------------------------------------------------------

## color
carbonregion %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = prop*100,
             fill = label)) +
  facet_grid(rows = vars(accord_label),
             cols = vars(region_label)) + 
  geom_bar(stat = "identity", color = NA,
           aes(group = force)) +
  geom_hline(data = dottedregions,
             aes(yintercept = yintercept*100),
             linetype = "dotted",
             linewidth = 0.75,
             color = "black",
             alpha = 0.75) +
  geom_label(aes(label = round(prop*100), group = force),
             position = position_stack(vjust = 0.5),
             size = 2.25, label.size = 0,
             label.padding = unit(0.05, "cm"),
             show.legend = FALSE) +
  scale_fill_manual(values = c("Strongly in disfavor" = "stripe", 
                               "Somewhat in disfavor" = "#e99996",
                               "Neutral" = "grey", 
                               "Somewhat in favor" = "#96c5ab", 
                               "Strongly in favor" = "#2E8B57")) +
  scale_color_manual(values = c("Strongly in disfavor" = "#D4342E", 
                               "Somewhat in disfavor" = "#e99996",
                               "Neutral" = "grey", 
                               "Somewhat in favor" = "#96c5ab", 
                               "Strongly in favor" = "#2E8B57")) +
  ylab("Proportion of the sample (%)") +
  xlab("") +
  labs(title = "Proportion of the sample in favor or in disfavor of the following question\ndepending on how the funds are used",
       subtitle = "Are you in favor of continuing to increase the price of carbon dioxide emissions?",
       caption = "n = 1500. Data collected from a survey conducted in August 2022 across Canada for all ages by the firm Synopsis.") +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        panel.border = element_rect(fill = "NA", color = "lightgrey"),
        panel.spacing = unit(0.25, "cm"),
        plot.title = element_text(size = 11))


### bw
carbonregion %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = prop*100)) +
  facet_grid(rows = vars(accord_label),
             cols = vars(region_label),
             switch = "y") + 
  geom_bar(stat = "identity", color = NA,
           fill = "#666666",
           aes(group = force,
               alpha = force)) +
  geom_hline(data = dottedregions,
             aes(yintercept = yintercept*100),
             linetype = "dotted",
             linewidth = 0.75,
             color = "black",
             alpha = 0.75) +
  geom_label(aes(label = round(prop*100),
                 group = force, fill = force),
             color = "white",
             position = position_stack(vjust = 0.5),
             size = 2.25, label.size = 0,
             label.padding = unit(0.05, "cm"),
             show.legend = FALSE) +
  scale_alpha_manual(values = c("Strongly" = 1, "Somewhat" = 0.6)) +
  scale_fill_manual(values = c("Strongly" = "#666666", "Neutral" = "#666666", "Somewhat" = "#a3a3a3")) +
  ylab("Proportion of the sample (%)") +
  xlab("") +
  labs(#title = "Proportion of the sample in favor or in disfavor of the following question\ndepending on how the funds are used",
       subtitle = "Are you in favor of continuing to increase the price of carbon dioxide emissions?",
       caption = "n = 1500. Data collected from a survey conducted in August 2022 across Canada for all ages by the firm Synopsis.") +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        panel.border = element_rect(fill = "NA", color = "lightgrey"),
        panel.spacing = unit(0.25, "cm"),
        plot.title = element_text(size = 11),
        strip.placement = "outside")


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/carb_regions.png",
       width = 11, height = 9)


# Fossil fuels ---------------------------------------------------------------

## All ---------------------------------------------------------------------

### wrangling ---------------------------------------------------------------

fossil <- Data %>% 
  rename(stateInterv_decreaseFossilProd_base = stateInterv_decreaseFossilProd) %>% 
  pivot_longer(., cols = starts_with("stateInterv_decreaseFossilProd"),
               names_to = "distribution",
               values_to = "scale",
               names_prefix = "stateInterv_decreaseFossilProd_") %>% 
  group_by(distribution, scale) %>% 
  summarise(n = n()) %>% 
  group_by(distribution) %>% 
  mutate(distribution = case_when(
    distribution == "base" ~ "Like the existing\nsystem in Canada",
    distribution == "GreenJobs" ~ "Funds used to create\njobs in green energy",
    distribution == "FossilJobs" ~ "Funds used to support\nfossil fuels workers"
  ),
  distribution = factor(distribution,
                        levels = c("Like the existing\nsystem in Canada",
                                   "Funds used to create\njobs in green energy",
                                   "Funds used to support\nfossil fuels workers")),
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
    scale %in% c(0, 0.25) ~ "in disfavor",
    scale %in% c(0.5) ~ "Neutral",
    scale %in% c(0.75, 1) ~ "in favor"
  ),
  accord = factor(accord, levels = c("in disfavor", "Neutral", "in favor")),
  accord_label = case_when(
    accord == "in disfavor" ~ "In disfavor",
    accord == "Neutral" ~ "Neutral",
    accord == "in favor" ~ "In favor"
  ),
  accord_label = factor(accord_label, levels = c("In disfavor", "Neutral", "In favor")),
  label = paste0(force, " ", accord),
  label = ifelse(label == "Neutral Neutral", "Neutral", label),
  label = factor(label, levels = c("Strongly in disfavor", "Somewhat in disfavor", 
                                   "Neutral", "Somewhat in favor", "Strongly in favor"))) %>% 
  arrange(distribution, -scale) %>% 
  group_by(distribution) %>% 
  mutate(cumsum_prop = cumsum(prop),
         scale = factor(scale, levels = c(0, 0.25, 0.5, 0.75, 1)))

## data for dotted lines
dottedfossil <- fossil %>% 
  filter(distribution == "Like the existing\nsystem in Canada") %>% 
  arrange(accord, desc(force)) %>% 
  group_by(accord) %>% 
  mutate(yintercept = cumsum(prop))


### Graph -------------------------------------------------------------------
fossil %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = prop*100,
             fill = label)) +
  facet_wrap(~accord_label, ncol = 1,
             strip.position = "right") +
  geom_bar(stat = "identity", color = NA,
           aes(group = force)) +
  geom_hline(data = dottedfossil,
             aes(yintercept = yintercept*100),
             linetype = "dotted",
             linewidth = 0.6,
             color = "black",
             alpha = 0.75) +
  geom_label(aes(label = round(prop*100), group = force),
             position = position_stack(vjust = 0.5),
             size = 2.4, label.size = 0,
             label.padding = unit(0.07, "cm"),
             show.legend = FALSE) +
  labs(title = "Proportion of the sample in favor or in disfavor of the\nfollowing question depending on how the funds are used",
       subtitle = "Are you in favor of progressively decreasing fossil fuels production in Canada?",
       caption = "n = 1500. Data collected from a survey conducted in August 2022 across Canada for all ages by the firm Synopsis.") +
  scale_fill_manual(values = c("Strongly in disfavor" = "#D4342E", 
                               "Somewhat in disfavor" = "#e99996",
                               "Neutral" = "lightgrey", 
                               "Somewhat in favor" = "#96c5ab", 
                               "Strongly in favor" = "#2E8B57")) +
  scale_color_manual(values = c("Strongly in disfavor" = "#D4342E", 
                                "Somewhat in disfavor" = "#e99996",
                                "Neutral" = "lightgrey", 
                                "Somewhat in favor" = "#96c5ab", 
                                "Strongly in favor" = "#2E8B57")) +
  ylab("Proportion of the sample (%)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5),
        panel.border = element_rect(fill = "NA", color = "lightgrey"),
        panel.spacing = unit(0.45, "cm"))

#ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/fossil_all.png",
#       width = 7, height = 7)

## Regions -----------------------------------------------------------------

### wrangling ---------------------------------------------------------------

fossilregion <- Data %>% 
  mutate(region = case_when(
    ses_prov %in% c("alberta", "sk") ~ "Alberta and Saskatchewan",
    ses_prov %in% c("cb") ~ "British Columbia",
    ses_prov %in% c("on") ~ "Ontario",
    ses_prov %in% c("qc") ~ "Quebec",
  )) %>% 
  drop_na(region) %>% 
  rename(stateInterv_decreaseFossilProd_base = stateInterv_decreaseFossilProd) %>% 
  pivot_longer(., cols = starts_with("stateInterv_decreaseFossilProd"),
               names_to = "distribution",
               values_to = "scale",
               names_prefix = "stateInterv_decreaseFossilProd_") %>% 
  group_by(distribution, region, scale) %>% 
  summarise(n = n()) %>% 
  group_by(distribution, region) %>% 
  mutate(distribution = case_when(
    distribution == "base" ~ "Like the existing\nsystem in Canada",
    distribution == "GreenJobs" ~ "Funds used to create\njobs in green energy",
    distribution == "FossilJobs" ~ "Funds used to support\nfossil fuels workers"
  ),
  distribution = factor(distribution,
                        levels = c("Like the existing\nsystem in Canada",
                                   "Funds used to create\njobs in green energy",
                                   "Funds used to support\nfossil fuels workers")),
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
    scale %in% c(0, 0.25) ~ "in disfavor",
    scale %in% c(0.5) ~ "Neutral",
    scale %in% c(0.75, 1) ~ "in favor"
  ),
  accord = factor(accord, levels = c("in disfavor", "Neutral", "in favor")),
  accord_label = case_when(
    accord == "in disfavor" ~ "In disfavor",
    accord == "Neutral" ~ "Neutral",
    accord == "in favor" ~ "In favor"
  ),
  accord_label = factor(accord_label, levels = c("In disfavor", "Neutral", "In favor")),
  label = paste0(force, " ", accord),
  label = ifelse(label == "Neutral Neutral", "Neutral", label),
  label = factor(label, levels = c("Strongly in disfavor", "Somewhat in disfavor", 
                                   "Neutral", "Somewhat in favor", "Strongly in favor"))) %>% 
  arrange(distribution, region, -scale) %>% 
  group_by(distribution, region) %>% 
  mutate(cumsum_prop = cumsum(prop),
         scale = factor(scale, levels = c(0, 0.25, 0.5, 0.75, 1)),
         region_label = paste0(region, "\nn = ", ndist))

## data for dotted lines
dottedfossilregions <- fossilregion %>% 
  filter(distribution == "Like the existing\nsystem in Canada") %>% 
  arrange(region_label, accord, desc(force)) %>% 
  group_by(region_label, accord) %>% 
  mutate(yintercept = cumsum(prop))


### Graph -----------------------------------------------------------

### color
fossilregion %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = prop*100,
             fill = label)) +
  facet_grid(rows = vars(accord_label),
             cols = vars(region_label)) + 
  geom_bar(stat = "identity", color = NA,
           aes(group = force)) +
  geom_hline(data = dottedfossilregions,
             aes(yintercept = yintercept*100),
             linetype = "dotted",
             linewidth = 0.75,
             color = "black",
             alpha = 0.75) +
  geom_label(aes(label = round(prop*100), group = force),
             position = position_stack(vjust = 0.5),
             size = 2.25, label.size = 0,
             label.padding = unit(0.05, "cm"),
             show.legend = FALSE) +
  scale_fill_manual(values = c("Strongly in disfavor" = "#D4342E", 
                               "Somewhat in disfavor" = "#e99996",
                               "Neutral" = "grey", 
                               "Somewhat in favor" = "#96c5ab", 
                               "Strongly in favor" = "#2E8B57")) +
  scale_color_manual(values = c("Strongly in disfavor" = "#D4342E", 
                                "Somewhat in disfavor" = "#e99996",
                                "Neutral" = "grey", 
                                "Somewhat in favor" = "#96c5ab", 
                                "Strongly in favor" = "#2E8B57")) +
  ylab("Proportion of the sample (%)") +
  xlab("") +
  labs(title = "Proportion of the sample in favor or in disfavor of the following question\ndepending on how the funds are used",
       subtitle = "Are you in favor of progressively decreasing fossil fuels production in Canada?",
       caption = "n = 1500. Data collected from a survey conducted in August 2022 across Canada for all ages by the firm Synopsis.") +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        panel.border = element_rect(fill = "NA", color = "lightgrey"),
        panel.spacing = unit(0.25, "cm"))


#### bw
fossilregion %>%
  mutate(label = factor(label, levels = rev(levels(label)))) %>% 
  ggplot(aes(x = distribution, y = prop*100,
             fill = label)) +
  facet_grid(rows = vars(accord_label),
             cols = vars(region_label),
             switch = "y") + 
  geom_bar(stat = "identity", color = NA,
           fill = "#666666",
           aes(group = force,
               alpha = force)) +
  geom_hline(data = dottedfossilregions,
             aes(yintercept = yintercept*100),
             linetype = "dotted",
             linewidth = 0.75,
             color = "black",
             alpha = 0.75) +
  geom_label(aes(label = round(prop*100),
                 group = force, fill = force),
             color = "white",
             position = position_stack(vjust = 0.5),
             size = 2.25, label.size = 0,
             label.padding = unit(0.05, "cm"),
             show.legend = FALSE) +
  scale_alpha_manual(values = c("Strongly" = 1, "Somewhat" = 0.6)) +
  scale_fill_manual(values = c("Strongly" = "#666666", "Neutral" = "#666666", "Somewhat" = "#a3a3a3")) +
  ylab("Proportion of the sample (%)") +
  xlab("") +
  labs(#title = "Proportion of the sample in favor or in disfavor of the following question\ndepending on how the funds are used",
       subtitle = "Are you in favor of progressively decreasing fossil fuels production in Canada?",
       caption = "n = 1500. Data collected from a survey conducted in August 2022 across Canada for all ages by the firm Synopsis.") +
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.5, 0.5, 0.5, 1)))) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        panel.border = element_rect(fill = "NA", color = "lightgrey"),
        panel.spacing = unit(0.25, "cm"),
        strip.placement = "outside")

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/fossil_regions.png",
       width = 11, height = 9)


# Scale gravity * support mesure base ------------------------------------------

graph <- Data %>% 
  pivot_longer(., cols = starts_with(c("stateInterv_continueIncreaseGESPrice", "stateInterv_decreaseFossilProd")),
               names_to = "politique",
               values_to = "accord") %>% 
  mutate(mesure = gsub("stateInterv_continueIncreaseGESPrice_", "", politique),
         mesure = gsub("stateInterv_decreaseFossilProd_", "", mesure),
         politique = substr(politique, 13, 20),
         mesure = gsub("stateInterv_continueIncreaseGESPrice", "base", mesure),
         mesure = gsub("stateInterv_decreaseFossilProd", "base", mesure),
         region = case_when(
           ses_prov %in% c("alberta", "sk") ~ "Alberta and\nSaskatchewan",
           ses_prov %in% c("cb") ~ "British Columbia",
           ses_prov %in% c("on") ~ "Ontario",
           ses_prov %in% c("qc") ~ "Quebec",
         ),
         region = ifelse(is.na(region), "Other region", region),
         region = factor(region, levels = c("Quebec", "Ontario",
                                            "British Columbia",
                                            "Alberta and\nSaskatchewan",
                                            "Other region")),
         region2 = case_when(
           region == "Quebec" ~ "Quebec",
           region == "Alberta and\nSaskatchewan" ~ "Alberta and\nSaskatchewan",
           !(region %in% c("Quebec", "Alberta and\nSaskatchewan")) ~ "ROC" 
         ),
         region2 = factor(region2, levels = c("ROC", "Quebec", "Alberta and\nSaskatchewan")),
         qc = ifelse(ses_prov_qc == 1, "Quebec", "ROC"),
         qc = factor(qc, levels = c("ROC", "Quebec")),
         mesure = case_when(
           mesure == "base" ~ "Like the existing\nsystem in Canada",
           mesure == "RedistributedPop" ~ "Funds distributed\nback to the population",
           mesure == "GreenJobs" ~ "Funds used to create\njobs in green energy",
           mesure == "FossilJobs" ~ "Funds used to support\nfossil fuels workers"
         ),
         mesure = factor(mesure,
                               levels = c("Like the existing\nsystem in Canada",
                                          "Funds used to create\njobs in green energy",
                                          "Funds used to support\nfossil fuels workers",
                                          "Funds distributed\nback to the population")),
         politique = ifelse(politique == "continue", "Continue to increase\nprice of CO2 emissions", "Decrease fossil\nfuel production"))

ggplot(graph, aes(x = scale_gravity, y = accord)) +
  facet_grid(cols = vars(mesure),
             rows = vars(politique),
             switch = "y") +
  geom_jitter(alpha = 0.025, size = 1) +
  geom_smooth(aes(group = qc, linetype = qc),
              method = "lm",
              color = "black",
              linewidth = 0.75) +
  ylab("") +
  xlab("\nConcern About Climate Change Scale\n") +
  #labs(title = "Relation between concern for climate change\nand support for different policies") +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c("Not concerned", "Concerned")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Strongly in\ndisfavor",
                                "Somewhat in\ndisfavor",
                                "Neutral",
                                "Somewhat in\nfavor",
                                "Strongly in\nfavor")) +
  clessnverse::theme_clean_light() +
  theme(strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 90, size = 7,
                                   hjust = 0.5, vjust = 0),
        strip.text.y = element_text(size = 13),
        panel.border = element_rect(color = "lightgrey", fill = NA))

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/gravityXaccord.png",
       width = 10, height = 8)


## with alb-sk
ggplot(graph, aes(x = scale_gravity, y = accord)) +
  facet_grid(cols = vars(mesure),
             rows = vars(politique),
             switch = "y") +
  geom_jitter(alpha = 0.025, size = 1) +
  geom_smooth(aes(group = region2, linetype = region2),
              method = "lm",
              color = "black",
              linewidth = 0.75) +
  ylab("") +
  xlab("\nConcern About Climate Change Scale\n") +
  labs(title = "Relation between concern for climate change\nand support for different policies") +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c("Not concerned", "Concerned")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Strongly in\ndisfavor",
                                "Somewhat in\ndisfavor",
                                "Neutral",
                                "Somewhat in\nfavor",
                                "Strongly in\nfavor")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  clessnverse::theme_clean_light() +
  theme(strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 90, size = 7,
                                   hjust = 0.5, vjust = 0),
        strip.text.y = element_text(size = 13))

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/gravityXaccord2.png",
       width = 10, height = 8)
