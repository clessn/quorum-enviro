# Packages ####
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Data ####
Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav") %>% 
  filter(PROV == 11) %>% 
  mutate(id = 1:nrow(.))

# Clean Data ####

# Q42 When will climate change harm Canadians
Data$when_harm_can <- NA
Data$when_harm_can[Data$Q42 <= 6] <- Data$Q42[Data$Q42 <= 6] - 1
Data$when_harm_can <- Data$when_harm_can/5
table(Data$when_harm_can)

# Eco anxiety scale ####
ScaleData <- Data %>%
  mutate(indi_resp = (Q32-1)/2,
         will_harm_can = (Q38-1)/3,
         menace_for_me = (Q40-1)/4,
         worried = (Q41-1)/3,
         major_catastrophe = (Q43_A1-1)/4,
         exagerated = (Q43_A2-1)/4,
         end_humanity = (Q72_A3-1)/4,
         exagerated = finverser(exagerated)) %>% 
  select(id, indi_resp, will_harm_can, menace_for_me, when_harm_can,
         worried, major_catastrophe, exagerated, end_humanity) %>% 
  na.omit()

ids_scale <- ScaleData$id
ScaleData <- ScaleData[,-1]

## Factor analysis ####
sysfonts::font_add_google("Roboto")
showtext::showtext_auto()

topdown_fa(ScaleData) +
  theme(axis.title.x = element_text(lineheight = 0.35))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/fa.png",
       width = 5, height = 4)

# Good, let's make a scale
ScaleData2 <- ScaleData %>% 
  mutate(scale_ecoanxiety = rowSums(.)/8)

ggplot(ScaleData2, aes(x = scale_ecoanxiety)) +
  geom_histogram(fill = "#2E8B57", color = "#2E8B57", alpha = 0.7) +
  clessnverse::theme_clean_dark() +
  xlab("") + ylab("\nNombre de répondants\n") +
  ggtitle("Distribution de l'échelle d'éco-anxiété\n") +
  scale_x_continuous(breaks = c(0.15, 0.85),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/hist.png",
       width = 4, height = 2.5)

# Link with age just for fun
Data$scale_ecoanxiety <- NA
Data$scale_ecoanxiety[ids_scale] <- ScaleData2$scale_ecoanxiety

ggplot(Data, aes(x = scale_ecoanxiety, y = factor(AGE))) +
  ggridges::geom_density_ridges(fill = "#2E8B57",
                                color = "#2E8B57",
                                alpha = 0.7,
                                quantile_lines = c(0.25, 0.5, 0.75)) +
  clessnverse::theme_clean_dark() +
  xlab("\nÉchelle d'éco-anxiété") +
  ylab("") +
  ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_x_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  scale_y_discrete(labels = c("18-24", "25-34", "35-44",
                              "45-54", "55-64", "65-74",
                              "75 et +"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/age.png",
       width = 4, height = 2.5)


# Link with religiosity

# Link with political violence

# Link with lifestyle
  # Meat consumption
  # my own lifestyle has contributed to
  # electric vehicule

# Link with political id


