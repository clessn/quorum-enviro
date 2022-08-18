library(tidyverse)

Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav")

# Codebook ####
var_name <- c()
q_label <- c()
answers_labels <- c()
answers_codes <- c()
for (i in 1:length(names(Data))){
  #i <- 15
  col <- names(Data)[i]
  var_name[i] <- col
  q_label[i] <- attributes(Data[[col]])$label
  answers_labels[i] <- paste0(names(attributes(Data[[col]])$labels), collapse = ", ")
  answers_codes[i] <- paste0(attributes(Data[[col]])$labels, collapse = ", ")
}

codebook <- as.data.frame(cbind(var_name, q_label, answers_labels, answers_codes))

writexl::write_xlsx(codebook, "_SharedFolder_quorum-enviro/codebook.xlsx")


# Q57_A1, Q57_A2, Q57_A3, Q60, Q61, Q55_A1, Q55_A2

Graph <- Data %>% 
  select(c(Q55_A1, Q55_A2, Q57_A1, Q57_A2, Q57_A3, Q60, Q61)) %>% 
  pivot_longer(., everything())


ggplot(Graph, aes(x = value, y = name)) +
  ggridges::geom_density_ridges()


Graph <- Data %>% 
  select(c(Q55_A1, Q57_A1, Q57_A2, Q57_A3)) %>% 
  pivot_longer(., everything()) %>% 
  mutate(value2 = ifelse(value > 3, 4, value),
         value2 = ifelse(value < 3, 2, value2)) %>% 
  filter(value2 == 2)


ggplot(Graph, aes(x = value2, y = name)) +
  geom_histogram() +
  facet_wrap(~name)
  ggridges::stat_binline(scale = 0.9, ) +
  scale_x_discrete(breaks = c(2,3,4))

  ggplot(Graph, aes(x = value2)) +
    geom_bar() +
    facet_wrap(~name, ncol = 1)

  ggsave("_SharedFolder_quorum-enviro/test.png",
         width = 5, height = 10)  
  
  
  ggplot(Graph, aes(x = name)) +
    geom_bar()# +
    facet_wrap(~value2, ncol = 1)
  
  ggsave("_SharedFolder_quorum-enviro/test.png",
         width = 5, height = 10)  
  
  
  Graph <- Data %>% 
    select(c(Q55_A1, Q57_A1, Q57_A2, Q57_A3)) %>% 
    pivot_longer(., everything()) %>% 
    mutate(value2 = ifelse(value > 3, 4, value),
           value2 = ifelse(value < 3, 2, value2)) %>% 
    group_by(name, value2) %>% 
    summarise(n = n()) %>% 
    mutate(n_group = sum(n),
           prop = n/n_group*100) %>% 
    filter(value2 == 2)
  
  
  ggplot(Graph, aes(x = reorder(name, -prop), y = prop)) +
    geom_bar(stat = "identity", color = "#2E8B57",
             fill = "#2E8B57", alpha = 0.7) +
    scale_x_discrete(#breaks = c("Q55_A1", "Q57_A1", "Q57_A3", "Q57_A2"),
                     labels = c("Comme le fait le système\nactuellement en vigueur au Canada", "Fonds redistribués à\nla population",
                                "Fonds utilisés pour soutenir\ntravailleurs des\ncombustibles fossiles", "Fonds utilisés pour\ncréer des emplois dans le\ndomaine des énergies vertes")) +
    ggthemes::theme_clean() +
    labs(title = "\nProportion des répondants en désaccord avec la\nquestion suivante en fonction de l'utilisation des fonds",
         subtitle = "\nÊtes-vous en faveur ou en défaveur que l'on continue d'augmenter\nle prix des émissions de gaz à effet de serre?\n") +
    xlab("") +
    ylab("\nProportion (%)\n") +
    geom_text(aes(y = prop + 0.5, label = round(prop)))
    
    
    ggsave("_SharedFolder_quorum-enviro/test.png",
           width = 8, height = 10)  
    
  
  