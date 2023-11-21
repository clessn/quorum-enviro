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
                                     "Other region"))
  )

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
                   ses_educUniv + ses_incomeLow + ses_incomeMid,
                 data = carbon)
summary(m_redispop)  

m_greenjobs <- lm(GreenJobs ~
                   scale_gravity * region + ses_gender_male +
                   ses_age_24m + ses_age_2534 + ses_age_5564 +
                   ses_age_6574 + ses_age_75p + ses_bornCanada +
                   ses_educUniv + ses_incomeLow + ses_incomeMid,
                 data = carbon)
summary(m_greenjobs)  

m_fossiljobs <- lm(FossilJobs ~
                    scale_gravity * region + ses_gender_male +
                    ses_age_24m + ses_age_2534 + ses_age_5564 +
                    ses_age_6574 + ses_age_75p + ses_bornCanada +
                    ses_educUniv + ses_incomeLow + ses_incomeMid,
                  data = carbon)
summary(m_fossiljobs)


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
  )))

ggplot(preds, aes(x = scale_gravity, y = estimate)) +
  facet_grid(cols = vars(model),
             rows = vars(region),
             switch = "y") +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "darkgrey") +
  geom_ribbon(aes(group = region, ymin = conf.low, ymax = conf.high),
              alpha = 0.3) +
  geom_line(aes(group = region),
            linetype = "dotted", linewidth = 1) +
  clessnverse::theme_clean_light() +
  labs(title = "Predicted Difference in Support for\nCarbon Dioxide Emission Price Increase",
       subtitle = "Current Canadian System vs. Just Measure") +
  ylab("\nPredicted Support Difference\n(Regression Model Units)\n") +
  xlab("\nConcern About Climate Change Scale\n") +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c("Not concerned", "Concerned")) +
  theme(strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/ppred_carbon.png",
       width = 7, height = 9)


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
                    ses_educUniv + ses_incomeLow + ses_incomeMid,
                  data = fossil)
summary(m_greenjobs)  

m_fossiljobs <- lm(FossilJobs ~
                     scale_gravity * region + ses_gender_male +
                     ses_age_24m + ses_age_2534 + ses_age_5564 +
                     ses_age_6574 + ses_age_75p + ses_bornCanada +
                     ses_educUniv + ses_incomeLow + ses_incomeMid,
                   data = fossil)
summary(m_fossiljobs)


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
  )))

ggplot(preds, aes(x = scale_gravity, y = estimate)) +
  facet_grid(cols = vars(model),
             rows = vars(region),
             switch = "y") +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "darkgrey") +
  geom_ribbon(aes(group = region, ymin = conf.low, ymax = conf.high),
              alpha = 0.3) +
  geom_line(aes(group = region),
            linetype = "dotted", linewidth = 1) +
  clessnverse::theme_clean_light() +
  labs(title = "Predicted Difference in Support for\nFossil Fuels Production Decrease",
       subtitle = "Current Canadian System vs. Just Measure") +
  ylab("\nPredicted Support Difference\n(Regression Model Units)\n") +
  xlab("\nConcern About Climate Change Scale\n") +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c("Not concerned", "Concerned")) +
  theme(strip.placement = "outside",
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/ppred_fossil.png",
       width = 7, height = 9)



