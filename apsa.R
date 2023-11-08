library(tidyverse)

Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav")

## Codebook ####
#var_name <- c()
#q_label <- c()
#answers_labels <- c()
#answers_codes <- c()
#for (i in 1:length(names(Data))){
#  #i <- 15
#  col <- names(Data)[i]
#  var_name[i] <- col
#  q_label[i] <- attributes(Data[[col]])$label
#  answers_labels[i] <- paste0(names(attributes(Data[[col]])$labels), collapse = ", ")
#  answers_codes[i] <- paste0(attributes(Data[[col]])$labels, collapse = ", ")
#}
#
#codebook <- as.data.frame(cbind(var_name, q_label, answers_labels, answers_codes))
#
#writexl::write_xlsx(codebook, "_SharedFolder_quorum-enviro/codebook.xlsx")


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
  facet_wrap(~name) +
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


        
source("functions.R", encoding = "UTF-8")
topdown_fa(Graph)    


## 1. Descriptif univarié ####
univar_plot <- function(question, title){
  #question <- "Q55_A1"
  GraphData <- Data %>% 
    group_by(.data[[question]]) %>% 
    summarise(n = n()) %>% 
    mutate(n_total = sum(n),
           prop = n/n_total*100)
  graph <- ggplot(GraphData, aes(x = .data[[question]], y = prop)) +
    geom_bar(width = 0.97, stat = "identity", color = "#2E8B57",
             fill = "#2E8B57", alpha = 0.7) +
    geom_text(aes(y = prop+3, label = round(prop)), color = "#4B4544") +
    ggtitle(title) +
    theme_minimal() +
    scale_y_continuous(limits = c(0,100))+
    scale_x_continuous(labels = c("Strongly\nin disfavor", "Somewhat\nin disfavor", "Neutral",
                                  "Somewhat\nin favor", "Strongly\nin favor"),
                       breaks = 1:5) +
    labs(caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in favor' or 'somewhat in favor' have been combined into a single group referred to as 'in favor'.")) +
    xlab("") +
    ylab("\nProportion of\nthe survey sample (%)\n") +
    theme(plot.background = element_rect(fill = "white"))
  return(graph)
}

#attach(Data)

univar_plot("Q55_A1", "\nAre you in favor of continuing to increase the price\nof carbon dioxide emissions (like the existing system in Canada)?\n")
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb2_base.png")

univar_plot("Q55_A2", "\nAre you in favor of progressively decreasing\nfossil fuels production in Canada?\n")
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossils_base.png")

univar_plot("Q57_A1", "\nWould you be in favor of continuing to increase the price on carbon dioxide emissions\nif the funds would be distributed back to the population [...]?\n")
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb2_topop.png")

univar_plot("Q57_A2", "\nWould you be in favor of continuing to increase the price on carbon dioxide emissions\nif the funds would be used to create jobs in green energy [...]?\n")
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb2_green.png")

univar_plot("Q57_A3", "\nWould you be in favor of continuing to increase the price on carbon dioxide emissions\nif the funds would be used to support fossil fuels workers?\n")
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb2_fossil.png")

univar_plot("Q60", "\nWould you be in favor of progressively decreasing fossil fuels production in Canada\nif more jobs were created in green energy than lost in the process?\n")
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossils_green.png")

univar_plot("Q61", "\nWould you be in favor of progressively decreasing fossil fuels production in Canada\nif fossil fuel workers were to be ensured support in the transition [...]?\n")
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossils_fossils.png")

## 2. Descriptif multivarié ####

# Carbon dioxide ####

provinces <- names(attributes(Data$PROV)$labels)
names(provinces) <- as.character(attributes(Data$PROV)$labels)
provinces[provinces == "Colombie-Britannique"] <- "British Columbia"
provinces[provinces == "Québec"] <- "Quebec"

BaseGraph <- Data %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, PROV, Q55_A1, Q57_A1, Q57_A2, Q57_A3) %>% 
  pivot_longer(., cols = c(Q55_A1, starts_with("Q57"))) %>% 
  mutate(prov_join = case_when(
          PROV == 9 ~ "Ontario",
          PROV == 11 ~ "Quebec",
          PROV %in% c(1,12) ~ "Alberta and Saskatchewan",
          PROV == 2 ~ "British Columbia"),
         PROV = provinces[as.character(PROV)],
         position = case_when(
           value %in% 1:2 ~ "In disfavor",
           value %in% 4:5 ~ "In favor",
           value == 3 ~ "Neutral"
         ),
         salience = case_when(
           value %in% c(1,5) ~ "Strongly",
           value %in% c(2,4) ~ "Somewhat"
         ),
         salience = factor(salience, levels = c("Strongly", "Somewhat")),
         name = case_when(
           name == "Q55_A1" ~ "Like the existing\nsystem in Canada",
           name == "Q57_A1" ~ "Funds distributed\nback to the population",
           name == "Q57_A2" ~ "Funds used to create\njobs in green energy",
           name == "Q57_A3" ~ "Funds used to support\nfossil fuels workers"
         ))

# carb_favor
Graph1 <- BaseGraph %>% 
  group_by(name, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100)

Graph1 %>% 
  filter(position == "In favor") %>%
  ggplot(aes(x = reorder(name, prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#2E8B57", fill = "#2E8B57") +
  theme_minimal() +
  xlab("") +
  ylab("<br>Proportion of the<br>sample in favor (%)<br>") +
  labs(title = "<br>Proportion of the sample in favor with the following question<br>depending on how the funds are used",
       subtitle = "<br>Are you in favor of continuing to increase the price of carbon dioxide emissions?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in favor' or 'somewhat in favor' have been combined into a single group referred to as 'in favor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top") +
  geom_text(aes(y = prop_position+4, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())
  
ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb_favor.png",
       width = 9, height = 6)


# carb_favor province
carb_favor_province <- BaseGraph %>% 
  filter(!(is.na(prov_join))) %>% 
  group_by(name, prov_join, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, prov_join, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name, prov_join) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100)

carb_favor_province %>% 
  filter(position == "In favor") %>%
  mutate(prov_join = factor(prov_join, levels = c("British Columbia", "Ontario", "Quebec", "Alberta and Saskatchewan")),
         facet_label = paste0(prov_join, "\nn = ", n_question)) %>% 
  ggplot(aes(x = reorder(name, prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#2E8B57", fill = "#2E8B57") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("<br>Proportion of the<br>sample in favor (%)<br>") +
  labs(title = "<br>Proportion of the sample in favor with the following question<br>depending on how the funds are used",
       subtitle = "<br>Are you in favor of continuing to increase the price of carbon dioxide emissions?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in favor' or 'somewhat in favor' have been combined into a single group referred to as 'in favor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+8, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb_favor_provinces.png",
       width = 9, height = 6)

# carb favor province join
carb_favor_province_join <- BaseGraph %>% 
  filter(!(is.na(prov_join))) %>% 
  group_by(name, prov_join, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, prov_join, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name, prov_join) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100,
         facet_label = paste0(prov_join, "\nn = ", n_question))

carb_favor_province_join %>% 
  filter(position == "In favor") %>%
  #mutate(PROV = factor(PROV, levels = c("British Columbia", "Ontario", "Quebec", "Alberta", "Saskatchewan"))) %>% 
  ggplot(aes(x = reorder(name, prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#2E8B57", fill = "#2E8B57") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("\nProportion of the\nsample in favor (%)\n") +
  labs(title = "\nProportion of the sample in favor with the following question\ndepending on how the funds are used",
       subtitle = "\nAre you in favor of continuing to increase the price of carbon dioxide emissions?\n",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+8, label = round(prop_position)), color = "#4B4544")


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb_favor_prov_join.png",
       width = 9, height = 6)


# carb_disfavor
Graph1 %>% 
  filter(position == "In disfavor") %>%
  ggplot(aes(x = reorder(name, -prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#D4342E", fill = "#D4342E") +
  theme_minimal() +
  theme_minimal() +
  xlab("") +
  ylab("<br>Proportion of the<br>sample in disfavor (%)<br>") +
  labs(title = "<br>Proportion of the sample in disfavor with the following question<br>depending on how the funds are used",
       subtitle = "<br>Are you in favor of continuing to increase the price of carbon dioxide emissions?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in disfavor' or 'somewhat in disfavor' have been combined into a single group referred to as 'in disfavor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top") +
  geom_text(aes(y = prop_position+4, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb_disfavor.png",
       width = 9, height = 6)

# carb_disfavor province
carb_favor_province %>% 
  filter(position == "In disfavor") %>%
  mutate(PROV = factor(PROV, levels = c("British Columbia", "Ontario", "Quebec", "Alberta", "Saskatchewan"))) %>% 
  ggplot(aes(x = reorder(name, -prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#D4342E", fill = "#D4342E") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("\nProportion of the\nsample in disfavor (%)\n") +
  labs(title = "\nProportion of the sample in disfavor with the following question\ndepending on how the funds are used",
       subtitle = "\nAre you in favor of continuing to increase the price of carbon dioxide emissions?\n",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+8, label = round(prop_position)), color = "#4B4544")

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb_disfavor_provinces.png",
       width = 9, height = 6)



# carb_disfavor province join
carb_favor_province_join %>% 
  filter(position == "In disfavor") %>%
  #mutate(PROV = factor(PROV, levels = c("British Columbia", "Ontario", "Quebec", "Alberta", "Saskatchewan"))) %>% 
  ggplot(aes(x = reorder(name, -prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#D4342E", fill = "#D4342E") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("<br>Proportion of the<br>sample in disfavor (%)<br>") +
  labs(title = "<br>Proportion of the sample in disfavor with the following question<br>depending on how the funds are used",
       subtitle = "<br>Are you in favor of continuing to increase the price of carbon dioxide emissions?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in disfavor' or 'somewhat in disfavor' have been combined into a single group referred to as 'in disfavor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+8, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/carb_disfavor_prov_join.png",
       width = 9, height = 6)


# Decreasing fossil fuels ####
BaseGraph2 <- Data %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, PROV, Q55_A2, Q60, Q61) %>% 
  pivot_longer(., cols = c(Q55_A2, Q60, Q61)) %>% 
  mutate(prov_join = case_when(
    PROV == 9 ~ "Ontario",
    PROV == 11 ~ "Quebec",
    PROV %in% c(1,12) ~ "Alberta and Saskatchewan",
    PROV == 2 ~ "British Columbia"),
    PROV = provinces[as.character(PROV)],
    position = case_when(
      value %in% 1:2 ~ "In disfavor",
      value %in% 4:5 ~ "In favor",
      value == 3 ~ "Neutral"
    ),
    salience = case_when(
      value %in% c(1,5) ~ "Strongly",
      value %in% c(2,4) ~ "Somewhat"
    ),
    salience = factor(salience, levels = c("Strongly", "Somewhat")),
    name = case_when(
      name == "Q55_A2" ~ "No consequence\nspecified",
      name == "Q60" ~ "More support for\nfossil fuel workers",
      name == "Q61" ~ "More jobs created in\ngreen energy"
    ))

Graph2 <- BaseGraph2 %>% 
  group_by(name, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100)

# fossil favor
Graph2 %>% 
  filter(position == "In favor") %>%
  ggplot(aes(x = reorder(name, prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#2E8B57", fill = "#2E8B57") +
  theme_minimal() +
  xlab("") +
  ylab("<br>Proportion of the<br>sample in favor (%)<br>") +
  labs(title = "<br>Proportion of the sample in favor with the following question<br>depending on the consequence on the economy",
       subtitle = "<br>Are you in favor of progressively decreasing fossil fuels production in Canada?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in favor' or 'somewhat in favor' have been combined into a single group referred to as 'in favor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top") +
  geom_text(aes(y = prop_position+4, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossil_favor.png",
       width = 9, height = 6)

Graph2 %>% 
  filter(position == "In disfavor") %>%
  ggplot(aes(x = reorder(name, -prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#D4342E", fill = "#D4342E") +
  theme_minimal() +
  theme_minimal() +
  xlab("") +
  ylab("<br>Proportion of the<br>sample in disfavor (%)<br>") +
  labs(title = "<br>Proportion of the sample in disfavor with the following question<br>depending on the consequence on the economy",
       subtitle = "<br>Are you in favor of progressively decreasing fossil fuels production in Canada?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in disfavor' or 'somewhat in disfavor' have been combined into a single group referred to as 'in disfavor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top") +
  geom_text(aes(y = prop_position+4, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossil_disfavor.png",
       width = 9, height = 6)


# fossil favor provinces
fossil_favor_province <- BaseGraph2 %>% 
  filter(!(is.na(prov_join))) %>% 
  group_by(name, prov_join, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, prov_join, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name, prov_join) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100)

fossil_favor_province %>% 
  filter(position == "In favor") %>%
  mutate(prov_join = factor(prov_join, levels = c("British Columbia", "Ontario", "Quebec", "Alberta and Saskatchewan")),
         facet_label = paste0(prov_join, "\nn = ", n_question)) %>% 
  ggplot(aes(x = reorder(name, prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#2E8B57", fill = "#2E8B57") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("<br>Proportion of the<br>sample in favor (%)<br>") +
  labs(title = "<br>Proportion of the sample in favor with the following question<br>depending on the consequence on the economy",
       subtitle = "<br>Are you in favor of progressively decreasing fossil fuels production in Canada?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in favor' or 'somewhat in favor' have been combined into a single group referred to as 'in favor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+9, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossil_favor_provinces.png",
       width = 9, height = 6)


# fossil favor provinces join
fossil_favor_province_join <- BaseGraph2 %>% 
  filter(!(is.na(prov_join))) %>% 
  group_by(name, prov_join, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, prov_join, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name, prov_join) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100,
         facet_label = paste0(prov_join, "\nn = ", n_question))

fossil_favor_province_join %>% 
  filter(position == "In favor") %>%
  #mutate(PROV = factor(PROV, levels = c("British Columbia", "Ontario", "Quebec", "Alberta", "Saskatchewan"))) %>% 
  ggplot(aes(x = reorder(name, prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#2E8B57", fill = "#2E8B57") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("\nProportion of the\nsample in favor (%)\n") +
  labs(title = "\nProportion of the sample in favor with the following question\ndepending on the consequence on the economy",
       subtitle = "\nAre you in favor of progressively decreasing fossil fuels production in Canada?\n",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 6.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+8, label = round(prop_position)), color = "#4B4544")

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossil_favor_prov_join.png",
       width = 9, height = 6)


# fossil disfavor provinces
fossil_disfavor_province <- BaseGraph2 %>% 
  filter(!(is.na(prov_join))) %>% 
  group_by(name, prov_join, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, prov_join, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name, prov_join) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100)

fossil_disfavor_province %>% 
  filter(position == "In disfavor") %>%
  mutate(prov_join = factor(prov_join, levels = c("British Columbia", "Ontario", "Quebec", "Alberta and Saskatchewan")),
         facet_label = paste0(prov_join, "\nn = ", n_question)) %>% 
  ggplot(aes(x = reorder(name, -prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#D4342E", fill = "#D4342E") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("<br>Proportion of the<br>sample in disfavor (%)<br>") +
  labs(title = "<br>Proportion of the sample in disfavor with the following question<br>depending on the consequence on the economy",
       subtitle = "<br>Are you in favor of progressively decreasing fossil fuels production in Canada?<br>",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.\nRespondents who indicated they were either 'strongly in disfavor' or 'somewhat in disfavor' have been combined into a single group referred to as 'in disfavor'.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 5.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+9, label = round(prop_position)), color = "#4B4544") +
  theme(axis.title.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown())

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossil_disfavor_provinces.png",
       width = 9, height = 6)


# fossil disfavor provinces join
fossil_disfavor_province_join <- BaseGraph2 %>% 
  filter(!(is.na(prov_join))) %>% 
  group_by(name, prov_join, position, salience) %>% 
  summarise(n_group = n()) %>% 
  group_by(name, prov_join, position) %>% 
  mutate(n_position = sum(n_group)) %>% 
  group_by(name, prov_join) %>% 
  mutate(n_question = sum(n_group),
         prop_group = n_group/n_question*100,
         prop_position = n_position/n_question*100,
         facet_label = paste0(prov_join, "\nn = ", n_question))

fossil_disfavor_province_join %>% 
  filter(position == "In disfavor") %>%
  #mutate(PROV = factor(PROV, levels = c("British Columbia", "Ontario", "Quebec", "Alberta", "Saskatchewan"))) %>% 
  ggplot(aes(x = reorder(name, -prop_position), y = prop_group)) +
  geom_bar(stat = "identity", aes(alpha = salience),
           color = "#D4342E", fill = "#D4342E") +
  theme_minimal() +
  xlab("") +
  facet_wrap(~facet_label)+
  ylab("\nProportion of the\nsample in disfavor (%)\n") +
  labs(title = "\nProportion of the sample in disfavor with the following question\ndepending on the consequence on the economy",
       subtitle = "\nAre you in favor of progressively decreasing fossil fuels production in Canada?\n",
       caption = paste0("n = ", nrow(Data), ". Data collected from a survey conducted on August 2022 across Canada for all ages by the firm Synopsis.")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_alpha_manual("", values = c("Somewhat" = 0.3, "Strongly" = 0.7)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = "top",
        axis.text.x = element_text(size = 6.5, angle = 45, vjust = 0.65)) +
  geom_text(aes(y = prop_position+8, label = round(prop_position)), color = "#4B4544")

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/fossil_disfavor_provinces_join.png",
       width = 9, height = 6)


