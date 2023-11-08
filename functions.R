library(tidyverse)
library(psych)
library(utils)
library(ggplot2)

topdown_fa <- function(df, nfactors = 1) {
  # Cronbach's alpha (Test 1)
  cronbachAlpha <<- round(psych::alpha(df)$total$raw_alpha, 2)
  
  # Analyse factorielle (Test 2)
  
  factAnalysis <- factanal(df, factors=nfactors) # Analyse factorielle
  factorVarNames <- names(df)
  
  factorLoadings <- as.numeric(factAnalysis$loadings[,1]) 
  factor1stEigen <<- round(eigen(cor(df))$values[1], digit=2)
  
  
  FAplot <- ggplot(data.frame(factorVarNames,factorLoadings), 
                   aes(x=factorVarNames, y=factorLoadings)) + 
    coord_flip() +
    geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
    geom_text(aes(label=as.character(round(factorLoadings, 
                                           digits = 2))), vjust=0.35, hjust=-0.3, size = 5) +
    geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
    annotate("text", label=paste("Alpha de Cronbach =", as.character(cronbachAlpha)), 
             x=1.1, y=1.28, size=5) +
    annotate("text", label=paste("Première valeur propre =", as.character(factor1stEigen)), 
             x=0.75, y=1.28, size=5) +
    annotate("segment", x = 0.4, xend = 1.45, 
             y = 1, yend = 1, colour = "black") +
    annotate("segment", x = 1.45, xend = 1.45, 
             y = 1, yend = Inf, colour = "black") +
    scale_y_continuous(name="\n Coefficients de saturation \n", 
                       limits=c(0, 1.55), breaks=seq(0, 1, by=0.1),
                       expand = c(0,0)) +
    xlab("\n") + 
    theme_linedraw() +
    theme(axis.text.y = element_text(size=15,
                                     margin = margin(r = 5, l = 3)), 
          axis.title.y = element_text(size = 15), 
          axis.text.x = element_text(size = 15),
          axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
          panel.grid=element_blank())
  print(FAplot)
  print("What we want:")
  print(paste0("Alpha de Cronbach > 0.6 -> ",cronbachAlpha))
  print(paste0("Première Valeur Propre > 1 -> ",factor1stEigen))
  print(paste0("Tous les coefficients de saturation > 0.3"))
}

finverser <- function(vec_col){
  #vec_col <- df[[col]]
  unique_col <- unique(vec_col)
  unique_col <- unique_col[!is.na(unique_col)]
  n <- length(unique_col)
  max <- max(vec_col, na.rm = T)
  ord <- sort(as.vector(unique_col))
  rev <- rev(ord)
  for (i in 1:n){
    vec_col[vec_col == ord[i]] <- max + rev[i] 
  }
  vec_col <- vec_col - max
  return(vec_col)
}

ggpairs_jitter_with_smooth <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_jitter(alpha = 0.2) + 
    geom_smooth(method=method, ...)
  p
}

vector_prop_table <- function(vector){
  table <- prop.table(table(vector))
  return(table)
}

acp_plots <- function(data){
  pr.out <- prcomp(data, scale = T)
  pr.out$sdev
  pr.var <- pr.out$sdev^2
  pve <- pr.var / sum(pr.var) #pve : proportion of variation explained
  par(mfrow = c(1, 3))
  biplot(pr.out, col = c("white", "grey20"))
  plot(pve, xlab = "Composante principale",
       ylab = "Proportion de variance expliquée", ylim = c(0, 1),
       type = "b", pch = 16, las = 1)
  plot(cumsum(pve), xlab = "Composante principale",
       ylab = "Proportion de variance expliquée",
       ylim = c(0, 1), type = "b", pch = 16, las = 1)
  par(mfrow = c(1,1))
}

# Function to transform scales on 0-1
minmaxNormalization <- function(x) {
  return((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
}


bivariate_plot <- function(data,
                             vd,
                             vi,
                             type_vd,
                             type_vi,
                             path){
  data$col_vd <- data[[vd]]
  mean_all <- mean(data$col_vd, na.rm = T)
  data$group <- as.character(data[[vi]])
  graphdata <- data %>% 
    group_by(group) %>% 
    summarise(n = n(),
              mean_vd = mean(col_vd, na.rm = T))
  ggplot(graphdata, aes(x = group, y = mean_vd)) +
    geom_hline(yintercept = mean_all,
               linetype = "dashed",
               color = "red",
               size = 1.5) +
    geom_bar(stat = "identity",
             width = 0.7,
             fill = "#5B92FF",
             color = "#5B92FF",
             alpha = 0.6) +
    geom_text(aes(y = mean_vd - 0.02, label = paste0("n = ", n)),
              size = 6, fontface = "bold") +
    ylab(vd) +
    xlab(vi)
    theme_bw()
  ggsave(path)
}

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x"){
    "y"
  } else {
    "x"
  }
  ggproto("CordRadar", CoordPolar, theta = theta, clip = "off", r = r, start = start, direction = sign(direction), is_linear = function(coord) TRUE)
}

spider_graph <- function(data){
  plot <- data %>% 
    mutate(group = "a") %>% 
    ggplot(aes(x = attitude, y = value)) +
    geom_point(color = "#525252", size = 40,
               shape = 21, fill = NA,
               aes(y = 0), stroke = 2.5) +
    geom_polygon(aes(group = group),
                 fill = "white", color = "white",
                 size = 1, alpha = 0.2) +
    geom_polygon(aes(group = group),
                 fill = "#ED96FF", color = "#ED96FF",
                 size = 1, alpha = 0.2) +
    geom_line(aes(group = group),
              size = 1, color = "#ED96FF") +
    geom_text(aes(x = attitude,
                  label = attitude),
              y = 1.15,
              angle = 15,
              color = "#F2F2F2",
              lineheight = 0.25,
              show.legend = F) +
    facet_wrap(~nk) +
    xlab("") + 
    ylab("") +
    #ylim(0,1) +
    scale_y_continuous(limits=c(0,1)) + 
    coord_radar() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.x = element_text(size = 75,
          #                           lineheight = 0.25,
          #                           vjust="inward",hjust="inward"),
          axis.line.x = element_blank(),
          panel.grid.major.x = element_line(color = "#525252", size = 1.5),
          panel.grid.major.y = element_blank(),
          #panel.grid.major.y = element_line(color = "#525252", size = 5),
          text = element_text(family = "VT323", lineheight = 0.25),
          plot.background = element_rect(fill = "#494949"),
          panel.background = element_rect(fill = "#494949"),
          plot.title = element_text(family = "VT323", face = "bold", hjust = 0.5,
                                    size = 250, lineheight = 0.35),
          plot.title.position = "panel",
          plot.margin = margin(t=30,r=60,b=30,l=30))
  return(plot)
}

spider_graph2 <- function(data,
                          label_names,
                          color){
  plot <- data %>% 
    mutate(group = "a") %>% 
    ggplot(aes(x = attitude, y = value)) +
    #geom_point(color = "#525252", size = 20,
    #           shape = 21, fill = NA,
    #           stroke = 0.15, aes(y = 0)) +
    geom_polygon(aes(group = group),
                 fill = "grey", color = "grey",
                 size = 1, alpha = 0.2) +
    geom_polygon(aes(group = group),
                 size = 1, alpha = 0.2,
                 fill = color, color = color) +
    geom_line(aes(group = group),
              size = 1, color = color) +
    geom_text(aes(x = attitude,
                  label = label_names[attitude],
                  size = value),
              y = 1,
              #angle = 15,
              color = "black",
              show.legend = F) +
    xlab("") + 
    ylab("") +
    #ylim(0,1) +
    scale_size_continuous(range = c(3.5,4)) +
    scale_y_continuous(limits=c(0,0.9)) + 
    coord_radar() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.x = element_text(size = 75,
          #                           lineheight = 0.25,
          #                           vjust="inward",hjust="inward"),
          panel.grid.major.x = element_line(color = "grey",
                                     size = 0.2),
          axis.line.x = element_blank(),
          #panel.grid.major.y = element_line(),
          panel.grid.major.y = element_line(color = "grey", size = 0.2),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          plot.title.position = "panel",
          plot.margin = margin(t=10,r=10,b=10,l=10))
  return(plot)
}

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

predProbs_propDest <- function(data, attitude, ses_skeleton){
  model <- Data %>% 
    select(tol_propertyDest, attitude, ses_gender_male,
           educ_level, income, age_cat_model) %>% 
    glm(tol_propertyDest ~ .,
        family = binomial(),
        data = .)
  ses_skeleton$att <- attitude
  ses_skeleton[[attitude]] <- ses_skeleton$att_level
  ses_skeleton$vd <- "tol_propertyDest"
  ses_skeleton$pred_vd <- predict(model, ses_skeleton, type = "response")
  ses_skeleton <- ses_skeleton %>% 
    select(-attitude)
  return(ses_skeleton)
}

predProbs_violentActions <- function(data, attitude, ses_skeleton){
  model <- Data %>% 
    select(tol_violentActions, attitude, ses_gender_male,
           educ_level, income, age_cat_model) %>% 
    glm(tol_violentActions ~ .,
        family = binomial(),
        data = .)
  ses_skeleton$att <- attitude
  ses_skeleton[[attitude]] <- ses_skeleton$att_level
  ses_skeleton$vd <- "tol_violentActions"
  ses_skeleton$pred_vd <- predict(model, ses_skeleton, type = "response")
  ses_skeleton <- ses_skeleton %>% 
    select(-attitude)
  return(ses_skeleton)
}

#predProbs_graph <- function(graphdata, item_group, attitudes){
#  Graph %>% 
#    filter(vd == item_group,
#           att %in% attitudes) %>%
#    mutate(att = gsub("scale_", "", att)) %>% 
#    ggplot(aes(x = att_level, y = pred_vd,
#               group = att, color = att)) +
#    geom_jitter(width = 0.12,
#                height = 0.12,
#                alpha = 0.05) +
#    geom_smooth(method = "gam",
#                se = F, alpha = 0.9) +
#    facet_wrap(~,
#               nrow = 1) +
#    clessnverse::theme_clean_light()
#}
