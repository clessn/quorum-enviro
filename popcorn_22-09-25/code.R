# Packages ####
library(tidyverse)
source("functions.R", encoding = "UTF-8")
sysfonts::font_add_google("Roboto")
showtext::showtext_auto()

# Data ####
Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav") %>% 
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
  mutate(menace_for_me = (Q40-1)/4,
         major_catastrophe = (Q43_A1-1)/4,
         end_humanity = (Q72_A3-1)/4,
         menace_for_me = ifelse(menace_for_me>=0.75,1,0),
         major_catastrophe = ifelse(major_catastrophe>=0.75,1,0),
         end_humanity = ifelse(end_humanity>=0.75,1,0)) %>% 
  select(id, menace_for_me, major_catastrophe, end_humanity) %>% 
  na.omit()

ids_scale <- ScaleData$id
ScaleData <- ScaleData[,-1]

topdown_fa(ScaleData) +
  theme(axis.title.x = element_text(lineheight = 0.35))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/fa.png",
       width = 5, height = 4)

# Good, let's make a scale
ScaleData <- ScaleData %>% 
  mutate(scale_ecoanxiety = rowSums(.)/3)

ScaleData %>% 
  group_by(scale_ecoanxiety) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = scale_ecoanxiety, y = n)) +
  geom_bar(stat = "identity", fill = "#2E8B57",
           color = "#2E8B57", alpha = 0.7) +
  clessnverse::theme_clean_dark(base_size = 30) +
  xlab("") + ylab("\nNombre de répondants\n") +
  labs(title = "\nDes Canadiens généralement éco-anxieux?\n",
       subtitle = "Distribution de l'échelle d'éco-anxiété au Canada\n",
       caption = "Source: Sondage CLESSN août 2022 (n = 1500). \nNote: Échelle d'éco-anxiété construite à partir de questions sur les perceptions de l'impact futur des changements climatiques. \nPour plus d'informations: info@clessn.com") +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("Pas éco-anxieux",
                                "Très éco-anxieux")) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(lineheight = 0.15),
        plot.title.position = "panel",
        plot.subtitle = element_text(lineheight = 0.35),
        plot.caption = element_text(lineheight = 0.35),
        plot.caption.position = "panel")

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/hist.png",
       width = 8.5, height = 5)


Data$scale_ecoanxiety <- NA
Data$scale_ecoanxiety[ids_scale] <- ScaleData$scale_ecoanxiety

# Province ####
provinces <- c("1" = "Alberta", "9" = "Ontario", "11" = "Québec")

HistProvinces <- Data %>%
  filter(PROV %in% c(11, 9, 1)) %>% 
  mutate(PROV = provinces[as.character(PROV)]) %>% 
  group_by(PROV, scale_ecoanxiety) %>% 
  summarise(n = n()) %>%
  mutate(n_group = sum(n),
         prop = n/n_group*100,
         label = paste0(PROV, " (n = ", n_group, ")"))

ggplot(HistProvinces, aes(x = scale_ecoanxiety, y = prop)) +
  geom_bar(stat = "identity", fill = "#2E8B57",
           color = "#2E8B57", alpha = 0.7) +
  facet_wrap(~label) +
  clessnverse::theme_clean_dark(base_size = 30) +
  xlab("") + ylab("\nProportion (%)\n") +
  labs(title = "\nLe niveau d'écoanxiété diffère selon la province\n",
       subtitle = "Distribution de l'échelle d'écoanxiété dans trois provinces canadiennes\n",
       caption = "Source: Sondage CLESSN août 2022 (n = 1500). \nNote: Échelle d'écoanxiété construite à partir de questions sur les perceptions de l'impact des changements climatiques. \nPour plus d'informations: info@clessn.com") +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("Pas\nécoanxieux",
                                "Très\nécoanxieux")) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(lineheight = 0.15),
        plot.title.position = "panel",
        plot.subtitle = element_text(lineheight = 0.35),
        axis.title.y = element_text(lineheight = 0.35),
        axis.text.x = element_text(lineheight = 0.35),
        plot.caption = element_text(lineheight = 0.35),
        plot.caption.position = "panel",
        plot.margin = margin(15,15,15,15),
        strip.background = element_rect(color = "#828282"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/provinces.png",
       width = 8.5, height = 5)
writexl::write_xlsx(HistProvinces, "_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/provinces.xlsx")

# Âge ####
Data %>% 
  filter(PROV==11) %>% 
  group_by(AGE) %>% 
  summarise(mean = mean(scale_ecoanxiety)) %>% 
  ggplot(aes(y = mean, x = factor(AGE))) +
  geom_bar(stat = "identity", fill = "#2E8B57",
                              color = "#2E8B57",
                              alpha = 0.7) +
  clessnverse::theme_clean_dark(base_size = 30) +
  ylab("\nÉchelle d'éco-anxiété") +
  xlab("") +
  ggtitle("Les jeunes, plus éco-anxieux?\n") +
  scale_y_discrete(labels = c("18-24", "25-34", "35-44",
                              "45-54", "55-64", "65-74",
                              "75 et +"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/age.png",
       width = 8.5, height = 5)

# id partisane ####
partys <- c("CAQ", "PLQ", "QS", "PQ", "PCQ", "Aucune")

IdPart <- Data %>% 
  filter(PROV==11 &
           Q71 != 6) %>% 
  mutate(party = partys[Q71]) %>% 
  group_by(party, scale_ecoanxiety) %>% 
  summarise(n = n()) %>% 
  mutate(n_group = sum(n),
         prop = n/n_group*100)

ggplot(IdPart, aes(y = prop, x = scale_ecoanxiety)) +
  geom_bar(stat = "identity",
           aes(fill = party, color = party),
           alpha = 0.7, show.legend = F) +
  clessnverse::theme_clean_dark(base_size = 35) +
  xlab("") +
  ylab("Proportion (%)\n") +
  facet_wrap(~party) +
  ggtitle("L'écoanxiété n'est pas partagée par tous les groupes partisans\n") +
  labs(subtitle = "Écoanxiété chez les bases partisanes des principaux partis provinciaux québécois\n\n",
       caption = "Source: Sondage CLESSN août 2022 (n = 1500). \nNote: Échelle d'écoanxiété construite à partir de questions sur les perceptions de l'impact des changements climatiques. \nPour plus d'informations: info@clessn.com") +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("Pas\nécoanxieux",
                                "Très\nécoanxieux")) +
  theme(plot.title = element_text(lineheight = 0.15),
        plot.title.position = "panel",
        plot.subtitle = element_text(lineheight = 0.35),
        axis.title.y = element_text(lineheight = 0.15),
        axis.text.x = element_text(lineheight = 0.35),
        plot.caption = element_text(lineheight = 0.35),
        plot.caption.position = "panel",
        plot.margin = margin(15,15,15,15),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_rect(color = "#828282")) +
  scale_y_continuous(limits = c(0, 75)) +
  scale_color_manual(values = c("QS" = "#ED8528",
                                "PQ" = "#0070C0",
                                "PLQ" = "#E61B2E",
                                "PCQ" = "#3D5889",
                                "CAQ" = "#00B0F0")) +
  scale_fill_manual(values = c("QS" = "#ED8528",
                                "PQ" = "#0070C0",
                                "PLQ" = "#E61B2E",
                                "PCQ" = "#3D5889",
                                "CAQ" = "#00B0F0"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/id_partisane.png",
       width = 8, height = 6)
writexl::write_xlsx(IdPart, "_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/id_partisane.xlsx")


# Violence politique ####
partys <- c("CAQ", "PLQ", "QS", "PQ", "PCQ", "Aucune")

Violence <- Data %>% 
  filter(PROV==11 &
           Q71 != 6) %>%
  mutate(party = partys[Q71]) %>%
  group_by(party, Q63_A4) %>%
  summarise(n = n()) %>%
  mutate(n_group = sum(n),
         prop = n/n_group*100)

ggplot(Violence, aes(y = prop, x = Q63_A4)) +
  geom_bar(stat = "identity",
           aes(fill = party, color = party),
           alpha = 0.7, show.legend = F) +
  clessnverse::theme_clean_dark(base_size = 35) +
  xlab("") +
  ylab("Proportion (%)\n") +
  facet_wrap(~party) +
  ggtitle("La tolérance de la marche pour le climat\n") +
  labs(subtitle = "À quel point seriez-vous prêt à tolérer l'action politique suivante\npour l'avancement de la cause climatique: participer à une manifestation?\n",
       caption = "Source: Sondage CLESSN août 2022 (n = 1500). \nPour plus d'informations: info@clessn.com\n") +
  scale_x_continuous(breaks = c(1, 4),
                     labels = c("Aucune\ntolérance",
                                "Tolérance\nélevée")) +
  scale_y_continuous(limits = c(0, 75)) +
  theme(plot.title = element_text(lineheight = 0.15),
        plot.title.position = "panel",
        plot.subtitle = element_text(lineheight = 0.35),
        axis.title.y = element_text(lineheight = 0.15),
        axis.text.x = element_text(lineheight = 0.35),
        plot.caption = element_text(lineheight = 0.35),
        plot.caption.position = "panel",
        plot.margin = margin(15,15,15,15),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_rect(color = "#828282")) +
  scale_color_manual(values = c("QS" = "#ED8528",
                                "PQ" = "#0070C0",
                                "PLQ" = "#E61B2E",
                                "PCQ" = "#3D5889",
                                "CAQ" = "#00B0F0")) +
  scale_fill_manual(values = c("QS" = "#ED8528",
                               "PQ" = "#0070C0",
                               "PLQ" = "#E61B2E",
                               "PCQ" = "#3D5889",
                               "CAQ" = "#00B0F0"))

ggsave("_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/manifs.png",
       width = 8, height = 6)
writexl::write_xlsx(Violence, "_SharedFolder_quorum-enviro/popcorn_2022-09-22/to_share/manifs.xlsx")



# Carte ####