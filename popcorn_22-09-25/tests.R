# Packages ####
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Data ####
Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav") %>% 
  filter(PROV == 11) %>% 
  mutate(id = 1:nrow(.),
         age = 2022-Q7)

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

# Link with age just for fun ####
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


ggplot(Data, aes(x = age, y = scale_ecoanxiety)) +
  geom_jitter(color = "#2E8B57") +
  geom_smooth() +
  clessnverse::theme_clean_dark() +
  ylab("\nÉchelle d'éco-anxiété") +
  xlab("Âge") +
  ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_y_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  theme(plot.title = element_text(lineheight = 0.35))


ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/age.png",
       width = 4, height = 2.5)

# Redo the scale with more extreme variables ####
Scale2 <- ScaleData %>% 
  select(menace_for_me, major_catastrophe, end_humanity) %>% 
  # Recode the variables to have a better discrimination
  mutate(menace_for_me = ifelse(menace_for_me>=0.75,1,0),
         major_catastrophe = ifelse(major_catastrophe>=0.75,1,0),
         end_humanity = ifelse(end_humanity>=0.75,1,0))

topdown_fa(Scale2) +
  theme(axis.title.x = element_text(lineheight = 0.35))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/fa2.png",
       width = 5, height = 4)

# Good, let's make a scale
Scale2Data2 <- Scale2 %>% 
  mutate(scale_ecoanxiety = rowSums(.)/3)

ggplot(Scale2Data2, aes(x = scale_ecoanxiety)) +
  geom_histogram(fill = "#2E8B57", color = "#2E8B57", alpha = 0.7) +
  clessnverse::theme_clean_dark() +
  xlab("") + ylab("\nNombre de répondants\n") +
  ggtitle("Distribution de l'échelle d'éco-anxiété\n") +
  scale_x_continuous(breaks = c(0.15, 0.85),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/hist2.png",
       width = 4, height = 2.5)

# Link with age just for fun ####
Data$scale_ecoanxiety <- NA
Data$scale_ecoanxiety[ids_scale] <- Scale2Data2$scale_ecoanxiety

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


ggplot(Data, aes(x = age, y = scale_ecoanxiety)) +
  geom_jitter(color = "#2E8B57") +
  geom_smooth() +
  clessnverse::theme_clean_dark() +
  ylab("\nÉchelle d'éco-anxiété") +
  xlab("Âge") +
  ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_y_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  theme(plot.title = element_text(lineheight = 0.35))


ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/age2.png",
       width = 4, height = 2.5)

# Changing the 0 manually ####

Data$scale_ecoanxiety2 <- Data$scale_ecoanxiety * 12
Data$scale_ecoanxiety2[Data$scale_ecoanxiety2 <= 5] <- 5
Data$scale_ecoanxiety2 <- (Data$scale_ecoanxiety2-5)/7
hist(Data$scale_ecoanxiety2)

ggplot(Data, aes(x = scale_ecoanxiety2)) +
  geom_histogram(fill = "#2E8B57", color = "#2E8B57", alpha = 0.7) +
  clessnverse::theme_clean_dark() +
  xlab("") + ylab("\nNombre de répondants\n") +
  ggtitle("Distribution de l'échelle d'éco-anxiété\n") +
  scale_x_continuous(breaks = c(0.15, 0.85),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux"))

ggplot(Data, aes(x = scale_ecoanxiety2, y = factor(AGE))) +
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


ggplot(Data, aes(x = age, y = scale_ecoanxiety2)) +
  geom_jitter(color = "#2E8B57") +
  geom_smooth() +
  clessnverse::theme_clean_dark() +
  ylab("\nÉchelle d'éco-anxiété") +
  xlab("Âge") +
  ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_y_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  theme(plot.title = element_text(lineheight = 0.35))


# Link with religiosity ####
Data %>% 
  filter(Q16 %in% c(2,9)) %>% 
  ggplot(aes(x = scale_ecoanxiety2, y = factor(Q16))) +
  ggridges::geom_density_ridges(fill = "#2E8B57",
                                color = "#2E8B57",
                                alpha = 0.7,
                                quantile_lines = c(0.25, 0.5, 0.75)) +
  clessnverse::theme_clean_dark() +
  xlab("\nÉchelle d'éco-anxiété") +
  ylab("") +
  #ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_x_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  scale_y_discrete(labels = c("Catholiques", "Membre d'aucune\ncommunauté religieuse"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/religion1.png",
       width = 4, height = 2.5)

Data %>% 
  group_by(Q17) %>% 
  summarise(ecoanxiety = mean(scale_ecoanxiety2, na.rm=T)) %>% 
  ggplot(aes(x = Q17, y = ecoanxiety)) +
  geom_bar(stat = "identity", fill = "#2E8B57",
           color = "#2E8B57",
           alpha = 0.7)
  

Data %>% 
  filter(Q19!=5) %>% 
  ggplot(aes(x = scale_ecoanxiety2, y = factor(Q19))) +
  ggridges::geom_density_ridges(fill = "#2E8B57",
                                color = "#2E8B57",
                                alpha = 0.7,
                                scale = 0.95,
                                quantile_lines = c(0.33, 0.67)) +
  clessnverse::theme_clean_dark() +
  xlab("\nÉchelle d'éco-anxiété") +
  ylab("") +
  #ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_x_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  scale_y_discrete(breaks = 1:4,
                   labels = c("Dieu personnel",
                              "Esprit ou force vitale",
                              "Ne sait pas",
                              "Pas de Dieu spirituel"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/religion2.png",
       width = 4, height = 2.5)


# Link with political violence ####
ggplot(Data, aes(x = Q62, y = scale_ecoanxiety)) +
  geom_jitter(color = "#2E8B57") +
  geom_smooth() +
  clessnverse::theme_clean_dark() +
  ylab("\nÉchelle d'éco-anxiété") +
  xlab("Violence") +
  #ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_y_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  theme(plot.title = element_text(lineheight = 0.35))



# Link with lifestyle ####
  # Meat consumption
  # my own lifestyle has contributed to
  # electric vehicule

# Link with political id ####
ggplot(Data, aes(y = factor(as.character(Q71)), x = scale_ecoanxiety2)) +
  ggridges::geom_density_ridges(color = NA,
                                fill = "white",
                                alpha = 0.2,
                                scale = 0.75,
                                panel_scaling = F,
                                bandwidth = 0.05) +
  geom_jitter(aes(color = factor(as.character(Q71))),
              show.legend = F, alpha = 0.6,
              size = 5) +
  clessnverse::theme_clean_dark(base_size = 35) +
  xlab("\nÉchelle d'éco-anxiété") +
  ylab("") +
  ggtitle("Éco-anxiété chez les bases partisanes\n") +
  labs(subtitle = "Selon l'identification partisane\n") +
  scale_x_continuous(breaks = c(0.05, 0.95),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  theme(plot.title = element_text(lineheight = 0.35),
        plot.subtitle = element_text(lineheight = 0.35)) +
  scale_color_manual(values = c("3" = "#ED8528",
                                "4" = "#0070C0",
                                "2" = "#E61B2E",
                                "5" = "#3D5889",
                                "6" = "grey",
                                "1" = "#00B0F0")) +
  scale_y_discrete(labels = c("CAQ", "PLQ", "QS",
                            "PQ", "PCQ",
                            "Aucune"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/id_partisane.png",
       width = 8, height = 6)




