# Library ##

library(tidyverse)
library(ggridges)
library(ggthemes)

# Functions ##

source("functions.R", encoding = "UTF-8")

# Data ##

Data <- haven::read_sav("_SharedFolder_transition/data/ULA011-données.Sav")

GraphData <- Data %>%
  dplyr:: select(QUEST, Q22_A3) %>%
  group_by(Q22_A3) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))

GraphData2 <- Data %>%
  dplyr:: select(QUEST, Q22_A3, Q22_A5) %>%
  pivot_longer(starts_with("Q22"))# %>%
  #summarize(n = n()) %>%
  #mutate(prop = n/sum(n))

# Plots ##

ggplot(GraphData, aes(x = Q22_A3, y = prop)) +
  geom_bar(color = "#2E8B57", fill = "#2E8B57", alpha = 0.7, stat = "identity") +
  labs(title = "Êtes-vous en accord ou en désaccord avec les énoncés suivants :", subtitle =
         "Nous devrions développer les pistes cyclables et les transports publics, \nmême si cela implique de réduire l'espace pour les voitures",
       x = "", y = "\nproportion (%)\n") +
  theme_classic() +
  scale_x_continuous(labels = c("Fortement\n en désaccord", "Plutôt en désaccord", "Neutre", "Plutôt en accord", "Fortement\n en accord"))

ggsave("_SharedFolder_transition/graphs/test3.png")

ggplot(GraphData2, aes(x = value, y = name)) +
  geom_bar(color = "#69b5ac", fill = "#69b5ac", alpha = 0.7, stat = "identity")



# Échelle ##

minmaxNormalization <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

Data2 <- data.frame(id = 1:nrow(Data),
                        postal_code = Data$CP,
                        ses_age = Data$AGE)

table(Data$Q22_A3)
Data2$carSpace <- NA
Data2$carSpace[Data$Q22_A3 == 1] <- 1
Data2$carSpace[Data$Q22_A3 == 2] <- 2
Data2$carSpace[Data$Q22_A3 == 3] <- 3
Data2$carSpace[Data$Q22_A3 == 4] <- 4
Data2$carSpace[Data$Q22_A3 == 5] <- 5
Data2$carSpace <- minmaxNormalization(Data2$carSpace)
table(Data2$carSpace)


table(Data$Q22_A5)
Data2$fuelCost <- NA
Data2$fuelCost[Data$Q22_A5 == 1] <- 5
Data2$fuelCost[Data$Q22_A5 == 2] <- 4
Data2$fuelCost[Data$Q22_A5 == 3] <- 3
Data2$fuelCost[Data$Q22_A5 == 4] <- 2
Data2$fuelCost[Data$Q22_A5 == 5] <- 1
Data2$fuelCost <- minmaxNormalization(Data2$fuelCost)
table(Data2$fuelCost)


table(Data$Q43_A3)
Data2$carResponsibility <- NA
Data2$carResponsibility[Data$Q43_A3 == 1] <- 1
Data2$carResponsibility[Data$Q43_A3 == 2] <- 2
Data2$carResponsibility[Data$Q43_A3 == 3] <- 3
Data2$carResponsibility[Data$Q43_A3 == 4] <- 4
Data2$carResponsibility[Data$Q43_A3 == 5] <- 5
Data2$carResponsibility <- minmaxNormalization(Data2$carResponsibility)
table(Data2$carResponsibility)

GraphData3 <- Data2 %>%
  select(id, carSpace, carResponsibility, fuelCost) %>%
  pivot_longer(cols = c(carSpace, carResponsibility, fuelCost))

ggplot(GraphData3, aes(x = value, y = name)) +
  geom_density_ridges(color = "#69b5ac", fill = "#69b5ac", alpha = 0.5, scale = 0.95) +
  theme_hc() +
  labs(title = "Êtes-vous en accord ou en désaccord avec les énoncés suivants :", x = "", y = "") +
  scale_y_discrete(labels = c("Nous devrions rendre l'essence et le diesel\n plus chers pour nous inciter à moins utiliser la voiture",
                               "Nous devrions développer les pistes cyclables\n et les transports publics, même si cela implique de réduire\n l'espace pour les voitures",
                               "Les émissions des voitures contribuent\n aux changements climatiques causés par l'être humain")) +
  scale_x_continuous(labels = c("Fortement\n en désaccord", "Plutôt\n en désaccord", "Neutre", "Plutôt\n en accord", "Fortement\n en accord"),
                     breaks = c(0, 0.25, 0.50, 0.75, 1))












