library(tidyverse)

Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

Data %>% 
  mutate(willHarmMoreDev = international_climateChangeHarmDevelopment - international_climateChangeHarmCA) %>% 
  ggplot(aes(x = willHarmMoreDev)) +
  geom_hist()
