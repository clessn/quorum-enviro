# Packages ----------------------------------------------------------------
library(tidyverse)
library(marginaleffects)

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data_with_clusters.rds") %>% 
  mutate(region = case_when(
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
  party_id = factor(politics_idFederal)
  )

data$party_id <- relevel(data$party_id, ref = "PLC")

# Carbon ------------------------------------------------------------------

carbon <- data %>% 
  rename(stateInterv_continueIncreaseGESPrice = stateInterv_continueIncreaseGESPrice) %>% 
  pivot_longer(., cols = starts_with("stateInterv_continueIncreaseGESPrice_"),
               names_to = "distribution",
               values_to = "scale",
               names_prefix = "stateInterv_continueIncreaseGESPrice_") %>% 
  mutate(effect = scale - stateInterv_continueIncreaseGESPrice) %>% 
  pivot_wider(., id_cols = names(.)[!(names(.) %in% c("distribution", "scale", "effect"))],
              names_from = "distribution",
              values_from = "effect")



## Models -------------------------------------------------------

m_redispop <- lm(RedistributedPop ~
                   scale_gravity * region + ses_gender_male +
                   ses_age_24m + ses_age_2534 + ses_age_5564 +
                   ses_age_6574 + ses_age_75p + ses_bornCanada +
                   ses_educUniv + ses_incomeLow + ses_incomeMid + party_id,
                 data = carbon)
summary(m_redispop)  

m_greenjobs <- lm(GreenJobs ~
                   scale_gravity * region + ses_gender_male +
                   ses_age_24m + ses_age_2534 + ses_age_5564 +
                   ses_age_6574 + ses_age_75p + ses_bornCanada +
                   ses_educUniv + ses_incomeLow + ses_incomeMid + party_id,
                 data = carbon)
summary(m_greenjobs)  

m_fossiljobs <- lm(FossilJobs ~
                    scale_gravity * region + ses_gender_male +
                    ses_age_24m + ses_age_2534 + ses_age_5564 +
                    ses_age_6574 + ses_age_75p + ses_bornCanada +
                    ses_educUniv + ses_incomeLow + ses_incomeMid + party_id,
                  data = carbon)
summary(m_fossiljobs)


### regression table for annex

modelsummary::modelplot(models = list("Redistribute to population" = m_redispop,
                                      "Green jobs" = m_greenjobs,
                                      "Fossil workers" = m_fossiljobs),
                        coef_rename = c(
                          "scale_gravity" = "Concern for climate change",
                          "regionOntario" = "Region: Ontario",
                          "regionBritish Columbia" = "Region: British Columbia",
                          "regionAlberta and\nSaskatchewan" = "Region: Alberta and Saskatchewan",
                          "regionOther region" = "Region: Other",
                          "ses_gender_male" = "Gender: Male",
                          "ses_age_24m" = "Age Under 24",
                          "ses_age_2534" = "Age 25-34",
                          "ses_age_5564" = "Age 55-64",
                          "ses_age_6574" = "Age 65-74",
                          "ses_age_75p" = "Age 75+",
                          "party_idPCC" = "Party ID: CPC",
                          "party_idNPD" = "Party ID: NDP",
                          "party_idBQ" = "Party ID: BQ",
                          "party_idPVC" = "Party ID: Green",
                          "party_idPPC" = "Party ID: PPC",
                          "party_idnoId" = "Party ID: No ID",
                          "ses_bornCanada" = "Born in Canada",
                          "ses_educUniv" = "University Education",
                          "ses_incomeLow" = "Low Income",
                          "ses_incomeMid" = "Middle Income",
                          "scale_gravity:regionOntario" = "Interaction: Climate Concern\nand Region Ontario",
                          "scale_gravity:regionBritish Columbia" = "Interaction: Climate Concern\nand Region British Columbia",
                          "scale_gravity:regionAlberta and\nSaskatchewan" = "Interaction: Climate Concern and\nRegion Alberta and Saskatchewan",
                          "scale_gravity:regionOther region" = "Interaction: Climate Concern and\nRegion Other"
                        )) +
  scale_color_grey(name = "Difference between existing system and") +
  geom_vline(xintercept = 0,
             linetype = "dotted") +
  clessnverse::theme_clean_light() +
  theme(legend.title = element_text(size = 10),
        axis.title.x = element_text(hjust = 0.5),
        legend.position = "right")
  
  
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/annex_regplot_carbon.png",
       width = 10, height = 9)


## Prob predites -----------------------------------------------------------

## scale_gravity X region --------------------------------------------------

d <- marginaleffects::datagrid(model = m_redispop,
                               scale_gravity = c(seq(0, 1, by = 0.1)),
                               region = unique(data$region))

preds <- rbind(
  marginaleffects::predictions(m_redispop,
                               newdata = d) %>%
    select(-RedistributedPop) %>%
    mutate(model = "Distributed back\nto population"),
  marginaleffects::predictions(m_greenjobs,
                               newdata = d) %>%
    select(-GreenJobs,-RedistributedPop) %>%
    mutate(model = "Create jobs in\ngreen energy"),
  marginaleffects::predictions(m_fossiljobs,
                               newdata = d) %>%
    select(-FossilJobs,-RedistributedPop) %>%
    mutate(model = "Support fossil\nfuel workers")
) %>% 
  mutate(model = factor(model, levels = c(
    "Distributed back\nto population",
    "Create jobs in\ngreen energy",
    "Support fossil\nfuel workers"
  ))) %>% 
  filter(region != "Other region")

texts <- preds %>% 
  filter(scale_gravity %in% c(0, 0.5, 1))

ggplot(preds, aes(x = scale_gravity, y = estimate)) +
  facet_grid(cols = vars(region),
             rows = vars(model),
             switch = "y") +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "darkgrey") +
  geom_ribbon(aes(group = region, ymin = conf.low, ymax = conf.high),
              alpha = 0.3) +
  geom_line(aes(group = region),
            linetype = "dotted", linewidth = 1) +
  geom_text(data = texts %>% filter(scale_gravity == 0),
            aes(y = conf.high + 0.025, label = sprintf("%.2f", estimate)),
            size = 2, hjust = 0) +
  geom_text(data = texts %>% filter(scale_gravity == 0.5),
            aes(y = conf.high + 0.025, label = sprintf("%.2f", estimate)),
            size = 2, hjust = 0) +
  geom_text(data = texts %>% filter(scale_gravity == 1),
            aes(y = conf.low - 0.025, label = sprintf("%.2f", estimate)),
            size = 2, hjust = 1) +
  clessnverse::theme_clean_light() +
  labs(#title = "Predicted Difference in Support for\nCarbon Dioxide Emission Price Increase",
       subtitle = "Existing System in Canada vs. Just Measure") +
  ylab("\nPredicted Support Difference\n(Regression Model Units)\n") +
  xlab("\nConcern About Climate Change Scale\n") +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c("Not concerned", "Concerned")) +
  theme(strip.placement = "outside",
        panel.border = element_rect(color = "lightgrey", fill = NA),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5))


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/ppred_carbon.png",
       width = 9, height = 7)


## region X model, x gravity ----------------------------------------------------------------

#d <- marginaleffects::datagrid(model = m_redispop,
#                               scale_gravity = c(0.1, 0.9),
#                               region = unique(data$region))
#
#preds <- rbind(
#  marginaleffects::predictions(m_redispop,
#                               newdata = d) %>%
#    select(-RedistributedPop) %>%
#    mutate(model = "redistributepop"),
#  marginaleffects::predictions(m_greenjobs,
#                               newdata = d) %>%
#    select(-GreenJobs,-RedistributedPop) %>%
#    mutate(model = "greenjobs"),
#  marginaleffects::predictions(m_fossiljobs,
#                               newdata = d) %>%
#    select(-FossilJobs,-RedistributedPop) %>%
#    mutate(model = "fossiljobs")
#) %>% 
#  group_by(region, model) %>% 
#  mutate(max = max(estimate)-0.0225,
#         min = min(estimate)+0.0225)
#
#ggplot(preds, aes(x = model, y = estimate)) +
#  facet_wrap(~region) +
#  geom_hline(yintercept = 0,
#             linetype = "dotted",
#             color = "darkgrey") +
#  geom_point(aes(alpha = factor(scale_gravity),
#                 fill = region),
#             size = 8, shape = 21,
#             stroke = 0.5, color = "black") +
#  geom_segment(aes(xend = model, yend = max, y = min,
#                   color = region),
#               linewidth = 1.5, alpha = 0.3) +
#  scale_alpha_manual(values = c(0.45, 1),
#                     labels = c("Not preoccupied", "Preoccupied")) +
#  clessnverse::theme_clean_light() +
#  ylab("predicted difference between support for\nexisting system VS just measure") +
#  theme(legend.title = element_text()) +
#  guides(color = "none",
#         fill = "none")
#
#
#ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/explo/ppred_carbon2.png",
#       width = 10, height = 8)
#



# Fossil ------------------------------------------------------------------

fossil <- data %>% 
  pivot_longer(., cols = starts_with("stateInterv_decreaseFossilProd_"),
               names_to = "distribution",
               values_to = "scale",
               names_prefix = "stateInterv_decreaseFossilProd_") %>% 
  mutate(effect = scale - stateInterv_decreaseFossilProd) %>% 
  pivot_wider(., id_cols = names(.)[!(names(.) %in% c("distribution", "scale", "effect"))],
              names_from = "distribution",
              values_from = "effect")


## Models -------------------------------------------------------

m_greenjobs <- lm(GreenJobs ~
                    scale_gravity * region + ses_gender_male +
                    ses_age_24m + ses_age_2534 + ses_age_5564 +
                    ses_age_6574 + ses_age_75p + ses_bornCanada +
                    ses_educUniv + ses_incomeLow + ses_incomeMid + party_id,
                  data = fossil)
summary(m_greenjobs)  

m_fossiljobs <- lm(FossilJobs ~
                     scale_gravity * region + ses_gender_male +
                     ses_age_24m + ses_age_2534 + ses_age_5564 +
                     ses_age_6574 + ses_age_75p + ses_bornCanada +
                     ses_educUniv + ses_incomeLow + ses_incomeMid + party_id,
                   data = fossil)
summary(m_fossiljobs)

### regression table for annex
modelsummary::modelplot(models = list("Green jobs" = m_greenjobs,
                                      "Fossil workers" = m_fossiljobs),
                        coef_rename = c(
                          "scale_gravity" = "Concern for climate change",
                          "regionOntario" = "Region: Ontario",
                          "regionBritish Columbia" = "Region: British Columbia",
                          "regionAlberta and\nSaskatchewan" = "Region: Alberta and Saskatchewan",
                          "regionOther region" = "Region: Other",
                          "ses_gender_male" = "Gender: Male",
                          "ses_age_24m" = "Age Under 24",
                          "ses_age_2534" = "Age 25-34",
                          "ses_age_5564" = "Age 55-64",
                          "ses_age_6574" = "Age 65-74",
                          "ses_age_75p" = "Age 75+",
                          "party_idPCC" = "Party ID: CPC",
                          "party_idNPD" = "Party ID: NDP",
                          "party_idBQ" = "Party ID: BQ",
                          "party_idPVC" = "Party ID: Green",
                          "party_idPPC" = "Party ID: PPC",
                          "party_idnoId" = "Party ID: No ID",
                          "ses_bornCanada" = "Born in Canada",
                          "ses_educUniv" = "University Education",
                          "ses_incomeLow" = "Low Income",
                          "ses_incomeMid" = "Middle Income",
                          "scale_gravity:regionOntario" = "Interaction: Climate Concern\nand Region Ontario",
                          "scale_gravity:regionBritish Columbia" = "Interaction: Climate Concern\nand Region British Columbia",
                          "scale_gravity:regionAlberta and\nSaskatchewan" = "Interaction: Climate Concern and\nRegion Alberta and Saskatchewan",
                          "scale_gravity:regionOther region" = "Interaction: Climate Concern and\nRegion Other"
                        )) +
  scale_color_grey(name = "Difference between existing system and") +
  geom_vline(xintercept = 0,
             linetype = "dotted") +
  clessnverse::theme_clean_light() +
  theme(legend.title = element_text(size = 10),
        axis.title.x = element_text(hjust = 0.5),
        legend.position = "right")


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/annex_regplot_fossil.png",
       width = 10, height = 9)



## Prob predites -----------------------------------------------------------

## scale_gravity X region --------------------------------------------------

d <- marginaleffects::datagrid(model = m_greenjobs,
                               scale_gravity = c(seq(0, 1, by = 0.1)),
                               region = unique(data$region))

preds <- rbind(
  marginaleffects::predictions(m_greenjobs,
                               newdata = d) %>%
    select(-GreenJobs) %>%
    mutate(model = "Create jobs in\ngreen energy"),
  marginaleffects::predictions(m_fossiljobs,
                               newdata = d) %>%
    select(-FossilJobs,-GreenJobs) %>%
    mutate(model = "Support fossil\nfuel workers")
) %>% 
  mutate(model = factor(model, levels = c(
    "Create jobs in\ngreen energy",
    "Support fossil\nfuel workers"
  ))) %>% 
  filter(region != "Other region")

texts <- preds %>% 
  filter(scale_gravity %in% c(0, 0.5, 1))

ggplot(preds, aes(x = scale_gravity, y = estimate)) +
  facet_grid(cols = vars(region),
             rows = vars(model),
             switch = "y") +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "darkgrey") +
  geom_ribbon(aes(group = region, ymin = conf.low, ymax = conf.high),
              alpha = 0.3) +
  geom_line(aes(group = region),
            linetype = "dotted", linewidth = 1) +
  geom_text(data = texts %>% filter(scale_gravity == 0),
            aes(y = conf.high + 0.025, label = sprintf("%.2f", estimate)),
            size = 2, hjust = 0) +
  geom_text(data = texts %>% filter(scale_gravity == 0.5),
            aes(y = conf.high + 0.025, label = sprintf("%.2f", estimate)),
            size = 2, hjust = 0) +
  geom_text(data = texts %>% filter(scale_gravity == 1),
            aes(y = conf.low - 0.025, label = sprintf("%.2f", estimate)),
            size = 2, hjust = 1) +
  clessnverse::theme_clean_light() +
  labs(#title = "Predicted Difference in Support for\nFossil Fuels Production Decrease",
       subtitle = "Existing System in Canada vs. Just Measure") +
  ylab("\nPredicted Support Difference\n(Regression Model Units)\n") +
  xlab("\nConcern About Climate Change Scale\n") +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c("Not concerned", "Concerned")) +
  theme(strip.placement = "outside",
        panel.border = element_rect(color = "lightgrey", fill = NA),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5))


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/ppred_fossil.png",
       width = 9, height = 7)



