# Packages ----------------------------------------------------------------
library(tidyverse)
source("functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

Data %>% 
  pivot_longer(starts_with("scale_locus"),
               names_prefix = "scale_locus_") %>% 
  ggplot(aes(x = value, y = factor(name))) +
  ggridges::geom_density_ridges(scale = 1.1,
                                bandwidth = 0.1,
                                quantile_lines = T,
                                quantiles = 3) +
  ylab("") +
  xlab("Responsabilité RELATIVE\naux autres acteurs")
ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/respoRelative.png",
       width = 9, height = 6)