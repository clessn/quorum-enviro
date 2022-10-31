library(tidyverse)

Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

names(Data)

Data %>% 
  filter(politics_idProvincial!="noId") %>% 
  group_by(politics_idProvincial, radicalisation_tolerate_vandalismObjects) %>% 
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100,
         is_tolerant = ifelse(radicalisation_tolerate_vandalismObjects==0, 0, 1),
         tol_level = ifelse(radicalisation_tolerate_vandalismObjects==0, 1, radicalisation_tolerate_vandalismObjects),
         tol_level = factor((tol_level*3), levels = c(3,2,1)),
         rad = as.character(radicalisation_tolerate_vandalismObjects),
         party = factor(politics_idProvincial,
                        levels = c("CAQ", "PQ", "PCQ", "PLQ", "QS"))) %>% 
  ggplot(aes(x = interaction(is_tolerant, party), y = prop)) +
  geom_bar(stat = "identity",
           aes(alpha = tol_level))



# +
#  facet_wrap(~politics_idProvincial) +
#  scale_x_discrete(labels = c("Aucune tolérance", "Tolérance faible", "Tolérance"))

