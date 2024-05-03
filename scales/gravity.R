###***************###
# Packages ####
###**************###
library(tidyverse)
library(GGally)

###**************###
# Functions ####
###**************###
source("functions.R", encoding = "UTF-8")

###**************###
# Load Data ####
###**************###
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")
names(Data)

###**************###
# Last cleaning ####
###**************###
## 0 = pas inquiet, situation pas grave, 1 = inquiet, situation grave 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleScep_ prefix

# climateChangePersonalMenace
table(Data$gravity_climateChangePersonalMenace)
Data$scaleGravity_climateChangePersonalMenace <- Data$gravity_climateChangePersonalMenace
table(Data$scaleGravity_climateChangePersonalMenace)

# worriedClimateChange
table(Data$gravity_worriedClimateChange)
Data$scaleGravity_worriedClimateChange <- Data$gravity_worriedClimateChange
table(Data$scaleGravity_worriedClimateChange)

# majorCatastrophe
table(Data$gravity_majorCatastrophe)
Data$scaleGravity_majorCatastrophe <- Data$gravity_majorCatastrophe
table(Data$scaleGravity_majorCatastrophe)

# climateChangeEndHumanity
table(Data$gravity_climateChangeEndHumanity)
Data$scaleGravity_climateChangeEndHumanity <- Data$gravity_climateChangeEndHumanity
table(Data$scaleGravity_climateChangeEndHumanity)

###**************###
# 1. first try ####
###**************###

###**************###
## 1.1 Factor analysis ####
###**************###
FaData <- Data %>% 
  select(starts_with("scaleGravity")) %>% 
  drop_na()

topdown_fa(FaData)

###**************###
## 1.2 Check distribution of scale and variables ####
###**************###

Data2 <- Data %>% 
  mutate(scaleGravity = (Data %>%
                        select(starts_with("scaleGravity_")) %>%
                        rowSums())/length(names(Data %>% select(starts_with("scaleGravity_")))))

hist(Data2$scaleGravity)
#plot(cumsum(vector_prop_table(Data2$scaleGravity)))
#cumsum(vector_prop_table(Data2$scaleGravity))
#plot(vector_prop_table(Data2$scaleGravity))
#plot(table(Data2$scaleGravity))
#plot(table(log(Data2$scaleGravity)))

#plot(vector_prop_table(log(Data2$scaleGravity)))
#hist(log(Data2$scaleGravity))

names(FaData) <- gsub("scaleGravity_", "", names(FaData))
ggpairs(FaData,
        diag = list(continuous=wrap("barDiag",
                                    binwidth=0.25,
                                    fill = "blue",
                                    color = "blue",
                                    alpha = 0.6)),
        lower = list(continuous=ggpairs_jitter_with_smooth))


# For paper ---------------------------------------------------------------

fa <- factanal(FaData, factors = 1)

loadings <- data.frame(var = fa$loadings[, 0],
                       loading = fa$loadings[, 1]) %>% 
  mutate(var = rownames(.))

alpha <- round(psych::alpha(df)$total$raw_alpha, 2)

ggplot(loadings,
       aes(x=reorder(var, loading), y=loading)) + 
  coord_flip() +
  geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
  geom_text(aes(label=as.character(round(loading, 
                                         digits = 2))), vjust=0.35, hjust=-0.3, size = 5) +
  annotate("text", label=paste("Cronbach alpha =", as.character(cronbachAlpha)), 
           x=1.1, y=1.28, size=5) +
  annotate("text", label=paste("First eigenvalue =", as.character(factor1stEigen)), 
           x=0.75, y=1.28, size=5) +
  annotate("segment", x = 0.4, xend = 1.45, 
           y = 1, yend = 1, colour = "black") +
  annotate("segment", x = 1.45, xend = 1.45, 
           y = 1, yend = Inf, colour = "black") +
  scale_y_continuous(name="\n Factor loadings \n", 
                     limits=c(0, 1.55), breaks=seq(0, 1, by=0.1),
                     expand = c(0,0)) +
  scale_x_discrete(labels = c("worriedClimateChange" = "How concerned are you\nabout global warming?",
                              "climateChangePersonalMenace" = "Climate change poses a threat\nto me in my lifetime",
                              "majorCatastrophe" = "If things continue on their present\ncourse, we will soon experience\na major ecological catastrophe.",
                              "climateChangeEndHumanity" = "Climate change will lead\nto the end of humanity.")) +
  xlab("\n") + 
  theme_linedraw() +
  theme(axis.text.y = element_text(size=15,
                                   margin = margin(r = 5, l = 3)), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
        panel.grid=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/annex_factoranalysis.png",
       width = 10, height = 7)


## histogram

ggplot(Data2, aes(x = scale_gravity)) +
  geom_histogram(fill = "grey", color = "grey") +
  clessnverse::theme_clean_light() +
  xlab("\nConcern for climate change scale\n") +
  ylab("Frequency") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(0.2, 0.8),
                     labels = c("Not concerned", "Concerned"))

ggsave("_SharedFolder_quorum-enviro/apsa_sept10/papier/annex_histogram.png",
       width = 9, height = 6)
