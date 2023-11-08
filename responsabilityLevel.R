# Packages ####
library(tidyverse)
source("functions.R", encoding = "UTF-8")

get_age_category <- function(ages){
  groups <- seq(from = 13,
                to = max(Data$ses_age, na.rm = T),
                by = 5)
  age_cats <- c()
  for (i in 1:length(ages)){
    if (!is.na(ages[i])){
      age_cats[i] <- max(groups[groups<=ages[i]],
                         na.rm = T)
    } else (age_cats[i] <- NA)
  }
  return(age_cats)
}


# Data ####
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

print("Citoyens")
table(Data$responsability_climateChangeCitizens)
print("Municipalités")
table(Data$responsability_climateChangeMuniGovt)
print("Gouv provincial")
table(Data$responsability_climateChangeProvGovt)
print("Gouv fédéral")
table(Data$responsability_climateChangeFedGovt)
print("Entreprises")
table(Data$responsability_climateChangeEnterprise)
print("OBNL")
table(Data$responsability_climateChangeOBNL)

names(Data %>% select(starts_with("radicalisation")))

Graph <- Data %>% 
  select(-responsability_climateChangeDiscuss) %>% 
  mutate(global_tolerance = minmaxNormalization(rowSums(select(., starts_with("radicalisation_tolerate"))))) %>% 
  pivot_longer(.,
               cols = starts_with("responsability_climateChange"),
               names_to = "actor", values_to = "responsability",
               names_prefix = "responsability_climateChange") %>% 
  select(id, actor, responsability, global_tolerance) %>% 
  group_by(actor, responsability) %>% 
  summarise(n = n(),
            tolerance = mean(global_tolerance)) %>% 
  mutate(responsability = case_when(
    responsability == 0 ~ "Pas du tout\nresponsable",
    responsability == 0.33 ~ "Un peu\nresponsable",
    responsability == 0.67 ~ "Partiellement\nresponsable",
    responsability == 1 ~ "Pleinement\nresponsable"
  ))

ggplot(Graph, aes(x = factor(responsability,
                             levels = c("Pas du tout\nresponsable",
                                        "Un peu\nresponsable",
                                        "Partiellement\nresponsable",
                                        "Pleinement\nresponsable")),
                  y = tolerance)) +
  geom_bar(stat = "identity",
           aes(alpha = n)) +
  geom_text(angle = 90,
            aes(y = tolerance - 0.1, label = paste0(n, " répondants")),
                size = 3.5, vjust = 2.25) +
  #geom_text() +
  xlab("") +
  ylab("Tolérance globale") +
  facet_wrap(~actor) +
  clessnverse::theme_clean_light() +
  theme(axis.text.x = element_text(size = 7))
        #axis.line.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #strip.background.x = element_blank())

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/respo_tolerance2.png",
       width = 8, height = 6)



#### Testing logistic regs on vege and 




## Qui sont ceux qui tolèrent les actions violentes?? ####