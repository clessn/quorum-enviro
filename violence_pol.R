library(tidyverse)

Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav")


# Q62, les Q63
Graph <- Data %>% 
  select(starts_with("Q63")) %>% 
  pivot_longer(., everything()) %>% 
  mutate(clean_name = case_when(name == "Q63_A1" ~ "Signer une pétition",
                                name == "Q63_A2" ~ "Boycotter produits et compagnies",
                                name == "Q63_A3" ~ "Désinvestir des placements",
                                name == "Q63_A4" ~ "Participer à une manifestation",
                                name == "Q63_A5" ~ "Occuper temporairement un espace public",
                                name == "Q63_A6" ~ "S'attacher à un arbre ou un véhicule",
                                name == "Q63_A7" ~ "Bloquer un pont ou une route",
                                name == "Q63_A8" ~ "Bloquer la construction d'un oléoduc",
                                name == "Q63_A9" ~ "Faire du vandalisme sur les objets",
                                name == "Q63_A10" ~ "Saboter infrastructures, véhicules, etc.",
                                name == "Q63_A11" ~ "Tirer un objet sur une infra, un véhicule, etc.",
                                name == "Q63_A12" ~ "Affronter des policiers dans une manifestation",
                                name == "Q63_A13" ~ "Violenter des individus en position de pouvoir"))

ggplot(Graph, aes(x = value, y = factor(clean_name))) +
  ggridges::geom_density_ridges(color = "#2E8B57", fill = "#2E8B57", alpha = 0.7, scale = 0.95) +
  ggridges::theme_ridges() +
  scale_x_continuous(labels = c("Aucune\ntolérance", "Tolérance\nfaible", "Tolérance\nmoyenne", "Tolérance\nélevée"),
                     breaks = 1:4) +
  xlab("") +
  ylab("") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("_SharedFolder_quorum-enviro/test2.png",
       width = 8, height = 10)  

