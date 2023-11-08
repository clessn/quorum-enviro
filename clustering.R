# Packages ----------------------------------------------------------------
library(tidyverse)
library(factoextra)
source("functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

CluData <- Data %>% 
  select(starts_with("scale"), -scale_radicalisation) %>% 
  drop_na() %>% 
  mutate_all(as.integer)

# Dimensional reduction ---------------------------------------------------

acp_plots(CluData)
pr.out <- prcomp(CluData, scale = T)
pr.out$sdev
biplot(pr.out, col = c("white", "grey20"))


# Combine some variables --------------------------------------------------

#Data$scale_spiritual <- (Data$scale_scepticism+Data$scale_religiosity)/2
#hist(Data$scale_spiritual)

Data$scale_optimist <- (Data$scale_IAtech+Data$scale_supportCarot+Data$scale_supportGouvActions)/3
hist(Data$scale_optimist)

Data$scale_engaged <- (Data$scale_behaviour+Data$scale_gravity+Data$scale_intlLeadership)/3
hist(Data$scale_engaged)

CluData0 <- Data %>% 
  select(id, scale_religiosity, scale_scepticism, scale_optimist,
         scale_engaged, scale_supportStick) %>% 
  drop_na()

ids <- CluData0$id
CluData <- CluData0 %>% select(-id)

# Optimal number of clusters -----------------------------------------------
factoextra::fviz_nbclust(CluData, kmeans, method = "wss") +
  geom_vline(xintercept = 5)


# Test around 5 -----------------------------------------------------------

#for (i in 3:7){
#  ki <- kmeans(CluData, centers = i, nstart = 25)
#  assign(paste0("k", i), ki)
#}

#k4 <- kmeans(CluData, centers = 4, nstart = 25)
k4 <- readRDS("_SharedFolder_quorum-enviro/clustering/k4_2.rds")

#k3 <- readRDS("_SharedFolder_quorum-enviro/clustering/k3.rds")
#k4 <- readRDS("_SharedFolder_quorum-enviro/clustering/k4.rds")
#k5 <- readRDS("_SharedFolder_quorum-enviro/clustering/k5.rds")
#k6 <- readRDS("_SharedFolder_quorum-enviro/clustering/k6.rds")

fviz_cluster(k3, geom = "point",  data = CluData)
table(k3[["cluster"]])
fviz_cluster(k4, geom = "point",  data = CluData)
table(k4[["cluster"]])
fviz_cluster(k5, geom = "point",  data = CluData)
table(k5[["cluster"]])
fviz_cluster(k6, geom = "point",  data = CluData)
table(k6[["cluster"]])

### 5 or 6 seem good

new_k5 <- c("5" = 1,
            "2" = 2,
            "3" = 3,
            "1" = 4,
            "4" = 5)

new_k6 <- c("1" = 1,
            "6" = 2,
            "5" = 3,
            "4" = 4,
            "2" = 6,
            "3" = 5)

Data$k3[Data$id %in% ids] <- k3[["cluster"]]
Data$k4[Data$id %in% ids] <- k4[["cluster"]]
Data$k5[Data$id %in% ids] <- new_k5[as.character(k5[["cluster"]])]
Data$k6[Data$id %in% ids] <- new_k6[as.character(k6[["cluster"]])]

# Describing clusters -----------------------------------------------------

Desc <- Data %>% 
  select(-scale_spiritual, -scale_optimist, -scale_engaged) %>%
  select(starts_with("scale"), k5, k6) %>% 
  pivot_longer(cols = starts_with("scale"),
               names_to = "attitude",
               names_prefix = "scale_") %>% 
  pivot_longer(cols = starts_with("k"),
               names_to = "nk",
               values_to = "cluster") %>% 
  group_by(nk, cluster, attitude) %>% 
  summarise(value = mean(value))


Desc %>% 
  filter(cluster == "1") %>% 
  spider_graph(.)

Desc %>% 
  filter(cluster == "2") %>% 
  spider_graph(.)

Desc %>% 
  filter(cluster == "3") %>% 
  spider_graph(.)

Desc %>% 
  filter(cluster == "4") %>% 
  spider_graph(.)


Desc %>% 
  filter(cluster == "5") %>% 
  spider_graph(.)

Desc %>% 
  filter(cluster == "6") %>% 
  spider_graph(.)

## Too much clusters, let's go with 4
Desc <- Data %>% 
  select(-scale_spiritual, -scale_optimist, -scale_engaged) %>%
  select(starts_with("scale"), k4) %>% 
  pivot_longer(cols = starts_with("scale"),
               names_to = "attitude",
               names_prefix = "scale_") %>% 
  group_by(cluster = k4, attitude) %>% 
  summarise(value = mean(value))

# Modéré
Desc %>% 
  filter(cluster == "1") %>% 
  spider_graph2(.)

# Modéré religieux
Desc %>% 
  filter(cluster == "2") %>% 
  spider_graph2(.)

# Écolo 
Desc %>% 
  filter(cluster == "3") %>% 
  spider_graph2(.)

# Cynique
Desc %>% 
  filter(cluster == "4") %>% 
  spider_graph2(.)

## What about the scales that make the clustering 
Desc <- Data %>% 
  select(scale_religiosity, scale_scepticism, scale_optimist, scale_engaged, scale_supportStick, k4) %>%
  #select(starts_with("scale"), k4) %>% 
  pivot_longer(cols = starts_with("scale"),
               names_to = "attitude",
               names_prefix = "scale_") %>% 
  group_by(cluster = k4, attitude) %>% 
  summarise(value = mean(value)) %>% 
  mutate(cluster = as.character(cluster))

label_names <- c("supportStick" = "Supporte les\nrestrictions gouvernementales",
                 "engaged" = "Préoccupé et\nengagé",
                 "optimist" = "Supporte les \ninnovations",
                 "religiosity" = "Religiosité/\nspiritualité",
                 "scepticism" = "Sceptique")

cluster_colors <- c("1" = "",
                    "2" = "",
                    "3" = "",
                    "4" = "")

# Modéré religieux/spirituel
Desc %>% 
  filter(cluster == "1") %>% 
  spider_graph2(.,
                label_names = label_names,
                color = "orange")
ggsave("_SharedFolder_quorum-enviro/graphs/clustering/cluster1.png",
       width = 6, height = 6)

# Modéré athée
Desc %>% 
  filter(cluster == "2") %>% 
  spider_graph2(.,
                label_names = label_names,
                color = "lightblue")
ggsave("_SharedFolder_quorum-enviro/graphs/clustering/cluster2.png",
       width = 6, height = 6)

# Écolo 
Desc %>% 
  filter(cluster == "3") %>% 
  spider_graph2(.,
                label_names = label_names,
                color = "red")
ggsave("_SharedFolder_quorum-enviro/graphs/clustering/cluster3.png",
       width = 6, height = 6)

# Cynique
Desc %>% 
  filter(cluster == "4") %>% 
  spider_graph2(.,
                label_names = label_names,
                color = "palegreen4")
ggsave("_SharedFolder_quorum-enviro/graphs/clustering/cluster4.png",
       width = 6, height = 6)


saveRDS(Data, "_SharedFolder_quorum-enviro/data/cleanData/data_with_clusters.rds")
