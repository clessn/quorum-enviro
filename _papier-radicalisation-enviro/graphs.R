library(tidyverse)
library(GGally)
library(ggridges)
library(fastDummies)

source("functions.R", encoding = "UTF-8")

Data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds")

scale_data <- Data %>% select(starts_with("scale"))

ggpairs(scale_data,
        diag = list(continuous=wrap("barDiag",
                                    binwidth=0.25,
                                    fill = "blue",
                                    color = "blue",
                                    alpha = 0.6)),
        lower = list(continuous=ggpairs_jitter_with_smooth))


### Bivariate: scale_radicalisation
vis <- names(Data %>% 
               select(-c(id, ses_postal_code,
                         -starts_with(c("radicalisation", "covid")))))

for (i in 1:length(vis)){
  vi <- vis[i]
  bivariate_plot(Data, vd = "scale_radicalisation",
                 vi = vi,
                 path = paste0("_SharedFolder_quorum-enviro/explo/scale_radicalisation/", vi, ".png"))
  print(i)
}

### Bivariate: scale_gravity
for (i in 1:length(vis)){
  vi <- vis[i]
  bivariate_plot(Data, vd = "scale_gravity",
                 vi = vi,
                 path = paste0("_SharedFolder_quorum-enviro/explo/scale_gravity/", vi, ".png"))
  print(i)
}

### Bivariate: scale_scepticism
for (i in 1:length(vis)){
  vi <- vis[i]
  bivariate_plot(Data, vd = "scale_scepticism",
                 vi = vi,
                 path = paste0("_SharedFolder_quorum-enviro/explo/scale_scepticism/", vi, ".png"))
  print(i)
}


# Figure 1 : at large graduation of 13 items ####
points <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))

density <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(tolerance),
         item = factor(item),
         item = fct_reorder(.f = item,
                            .x = mean_item,
                            .desc = T))

ggplot(points, aes(x = mean_item,
                   y = reorder(item, -mean_item))) +
  geom_point() +
  theme_classic()

ggplot(density, aes(x = tolerance,
                    y = reorder(item, -mean_item))) +
  geom_density_ridges(scale = 1.05, bandwidth = 0.175,
                      color = "#2E8B57", fill = "#2E8B57",
                      alpha = 0.7, quantile_lines=T,
                      quantile_fun=function(x,...)mean(x)) +
  theme_classic() +
  ylab("") +
  xlab("Tolerance")

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig1_population.png",
       width = 10, height = 7)


# Figure 2: variation by SES ####

### Dumbbell plot for 13 radicalisation items

Dbell <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(tolerance)) %>% 
  group_by(item, ses_gender_male) %>% 
  summarise(tolerance = mean(tolerance),
            mean_item = mean(mean_item)) %>% 
  mutate(ses_gender_male = as.character(ses_gender_male)) %>%
  group_by(item) %>%
  mutate(x = min(tolerance),
         xend = max(tolerance),
         more_tolerant = ses_gender_male[which.max(tolerance)]) %>% 
  group_by(ses_gender_male) %>% 
  mutate(mean_ses = mean(tolerance)) %>% 
  ungroup() %>%
  mutate(xmin = min(mean_ses),
         xmax = max(mean_ses),
         more_tolerant_global = ses_gender_male[which.max(mean_ses)])


ggplot(Dbell, aes(x = tolerance, y = reorder(item, -mean_item))) +
  geom_vline(aes(xintercept = mean_ses,
                 color = ses_gender_male),
                 alpha = 0.3, size = 0.5) +
  #geom_rect(aes(xmin = xmin, xmax = xmax,
  #              fill = more_tolerant_global),
  #             alpha = 0.015, color = NA,
  #          ymin = 0, ymax = 14) +
  geom_segment(aes(x = x, xend = xend,
                   yend = item,
                   color = more_tolerant),
               alpha = 0.3, size = 4.5) +
  geom_point(aes(color = ses_gender_male), size = 5) +
  xlab("\nTolérance pour les actions suivantes\npour des causes climatiques") +
  ylab("") +
  theme_classic() +
  scale_color_manual("ses_gender_male",
                     values = c("0" = "red", "1" = "blue")) +
  scale_fill_manual("ses_gender_male",
                    values = c("0" = "red", "1" = "blue"))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig2_gender.png",
       width = 10, height = 7)

# By age
Age <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_")

ggplot(Age, aes(x = ses_age, y = tolerance)) +
  geom_jitter(alpha = 0.2,
              height = 0.4,
              width = 0.4) +
  geom_smooth() +
  facet_wrap(~item) +
  theme_bw()

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig2_age.png",
       width = 10, height = 7)

# Income high
Dbell <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(tolerance)) %>% 
  group_by(item, ses_incomeHigh) %>% 
  summarise(tolerance = mean(tolerance),
            mean_item = mean(mean_item)) %>% 
  mutate(ses_incomeHigh = as.character(ses_incomeHigh)) %>%
  group_by(item) %>%
  mutate(x = min(tolerance),
         xend = max(tolerance),
         more_tolerant = ses_incomeHigh[which.max(tolerance)]) %>% 
  group_by(ses_incomeHigh) %>% 
  mutate(mean_ses = mean(tolerance)) %>% 
  ungroup() %>%
  mutate(xmin = min(mean_ses),
         xmax = max(mean_ses),
         more_tolerant_global = ses_incomeHigh[which.max(mean_ses)])


ggplot(Dbell, aes(x = tolerance, y = reorder(item, -mean_item))) +
  geom_vline(aes(xintercept = mean_ses,
                 color = ses_incomeHigh),
             alpha = 0.3, size = 0.5) +
  #geom_rect(aes(xmin = xmin, xmax = xmax,
  #              fill = more_tolerant_global),
  #             alpha = 0.015, color = NA,
  #          ymin = 0, ymax = 14) +
  geom_segment(aes(x = x, xend = xend,
                   yend = item,
                   color = more_tolerant),
               alpha = 0.3, size = 4.5) +
  geom_point(aes(color = ses_incomeHigh), size = 5) +
  xlab("\nTolérance pour les actions suivantes\npour des causes climatiques") +
  ylab("") +
  theme_classic() +
  scale_color_manual("ses_incomeHigh",
                     values = c("0" = "red", "1" = "blue")) +
  scale_fill_manual("ses_incomeHigh",
                    values = c("0" = "red", "1" = "blue"))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig2_incomeHigh.png",
       width = 10, height = 7)

# education

Educ <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  mutate(educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5, educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1, educ_level))

ggplot(Educ, aes(x = educ_level, y = tolerance)) +
  geom_jitter(alpha = 0.2,
              height = 0.2,
              width = 0.2) +
  geom_smooth(method = "glm") +
  facet_wrap(~item) +
  theme_bw()

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig2_educ.png",
       width = 10, height = 7)

# Quebec vs ROC
Dbell <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(tolerance)) %>% 
  group_by(item, ses_prov_qc) %>% 
  summarise(tolerance = mean(tolerance),
            mean_item = mean(mean_item)) %>% 
  mutate(ses_prov_qc = as.character(ses_prov_qc)) %>%
  group_by(item) %>%
  mutate(x = min(tolerance),
         xend = max(tolerance),
         more_tolerant = ses_prov_qc[which.max(tolerance)]) %>% 
  group_by(ses_prov_qc) %>% 
  mutate(mean_ses = mean(tolerance)) %>% 
  ungroup() %>%
  mutate(xmin = min(mean_ses),
         xmax = max(mean_ses),
         more_tolerant_global = ses_prov_qc[which.max(mean_ses)])


ggplot(Dbell, aes(x = tolerance, y = reorder(item, -mean_item))) +
  #geom_vline(aes(xintercept = mean_ses,
  #               color = ses_prov_qc),
  #           alpha = 0.3, size = 0.5) +
  ##geom_rect(aes(xmin = xmin, xmax = xmax,
  #              fill = more_tolerant_global),
  #             alpha = 0.015, color = NA,
  #          ymin = 0, ymax = 14) +
  geom_segment(aes(x = x, xend = xend,
                   yend = item,
                   color = more_tolerant),
               alpha = 0.3, size = 4.5) +
  geom_point(aes(color = ses_prov_qc), size = 5) +
  xlab("\nTolérance pour les actions suivantes\npour des causes climatiques") +
  ylab("") +
  theme_classic() +
  scale_color_manual("ses_prov_qc",
                     values = c("0" = "red", "1" = "blue")) +
  scale_fill_manual("ses_prov_qc",
                    values = c("0" = "red", "1" = "blue"))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig2_QcVsRoc.png",
       width = 10, height = 7)

# ontario
Dbell <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  mutate(mean_item = mean(tolerance)) %>% 
  group_by(item, ses_prov_on) %>% 
  summarise(tolerance = mean(tolerance),
            mean_item = mean(mean_item)) %>% 
  mutate(ses_prov_on = as.character(ses_prov_on)) %>%
  group_by(item) %>%
  mutate(x = min(tolerance),
         xend = max(tolerance),
         more_tolerant = ses_prov_on[which.max(tolerance)]) %>% 
  group_by(ses_prov_on) %>% 
  mutate(mean_ses = mean(tolerance)) %>% 
  ungroup() %>%
  mutate(xmin = min(mean_ses),
         xmax = max(mean_ses),
         more_tolerant_global = ses_prov_on[which.max(mean_ses)])


ggplot(Dbell, aes(x = tolerance, y = reorder(item, -mean_item))) +
  #geom_vline(aes(xintercept = mean_ses,
  #               color = ses_prov_on),
  #           alpha = 0.3, size = 0.5) +
  ##geom_rect(aes(xmin = xmin, xmax = xmax,
  #              fill = more_tolerant_global),
  #             alpha = 0.015, color = NA,
  #          ymin = 0, ymax = 14) +
  geom_segment(aes(x = x, xend = xend,
                   yend = item,
                   color = more_tolerant),
               alpha = 0.3, size = 4.5) +
  geom_point(aes(color = ses_prov_on), size = 5) +
  xlab("\nTolérance pour les actions suivantes\npour des causes climatiques") +
  ylab("") +
  theme_classic() +
  scale_color_manual("ses_prov_on",
                     values = c("0" = "red", "1" = "blue")) +
  scale_fill_manual("ses_prov_on",
                    values = c("0" = "red", "1" = "blue"))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig2_ontario.png",
       width = 10, height = 7)

# preoccupation

preo <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_")

ggplot(preo, aes(x = scale_gravity, y = tolerance)) +
  geom_jitter(alpha = 0.2,
              height = 0.2,
              width = 0.3) +
  geom_smooth() +
  facet_wrap(~item) +
  theme_bw()

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig2_scaleGravity.png",
       width = 10, height = 7)

# Figure 3 : regressions ####
vds <- names(Data %>% 
               select(starts_with("radicalisation_tolerate")))

## Prepare independent variables
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

Data2 <- Data %>% 
  mutate(educ_level = ifelse(ses_educBHS == 1, 0, NA),
         educ_level = ifelse(ses_educCollege == 1, 0.5, educ_level),
         educ_level = ifelse(ses_educUniv == 1, 1, educ_level),
         responsability = responsability_citizensVsGvnt,
         over35yo = ifelse(ses_age >= 35, 1, 0),
         age_cat = get_age_category(ses_age),
         age_cat_norm = minmaxNormalization(age_cat),
         ses_age = minmaxNormalization(ses_age)) %>% 
  fastDummies::dummy_columns(.,
                             select_columns = c("age_cat"))

vds <- names(Data2 %>% 
               select(starts_with("radicalisation_tolerate")))
names(Data2) <- gsub("radicalisation_tolerate_",
                    "",
                    names(Data2))
vds <- gsub("radicalisation_tolerate_",
            "",
            vds)

GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)
for (i in 1:length(vds)){
  #i <- 1
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity*ses_age + ses_gender_male +
                             responsability + educ_level +
                 ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

sysfonts::font_add_google("Roboto")
showtext::showtext_auto()

GraphData %>% 
  filter(vi %in% c("scale_gravity", "ses_age", "responsability",
                   "scale_gravity:ses_age")) %>% 
  mutate(sign = as.character(ifelse(pval <= 0.05, "Significatif", "Non-significatif"))) %>% 
  ggplot(aes(x = coef, y = reorder(item, -mean_tol),
             alpha = sign)) +
  scale_x_continuous(limits = c(-1, 1)) +
  ylab("") +
  xlab("\nCoefficient de régression") +
  facet_wrap(~vi, nrow = 1) +
  geom_point(color = "#6CAAF5", size = 1.5) +
  geom_segment(color = "#6CAAF5", size = 1, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#6CAAF5", size = 1, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = NA),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig3_coefficients-tolLevel.png",
       width = 10, height = 7)


## Sans interaction
GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)
for (i in 1:length(vds)){
  #i <- 1
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity + ses_age + ses_gender_male +
                             responsability + educ_level +
                 ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

#sysfonts::font_add_google("Roboto")
#showtext::showtext_auto()

GraphData %>% 
  filter(vi %in% c("scale_gravity", "ses_age", "responsability")) %>% 
  mutate(sign = as.character(ifelse(pval <= 0.05, "Significatif", "Non-significatif"))) %>% 
  ggplot(aes(x = coef, y = reorder(item, -mean_tol),
             alpha = sign)) +
  scale_x_continuous(limits = c(-1, 1)) +
  ylab("") +
  xlab("\nCoefficient de régression") +
  facet_wrap(~vi, nrow = 1) +
  geom_point(color = "#6CAAF5", size = 1.5) +
  geom_segment(color = "#6CAAF5", size = 1, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#6CAAF5", size = 1, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = NA),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig3_coefficients-tolLevel2.png",
       width = 10, height = 7)



## Avec interaction, sans responsability
GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)
for (i in 1:length(vds)){
  #i <- 1
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity*ses_age + ses_gender_male +
                             educ_level +
                 ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

#sysfonts::font_add_google("Roboto")
#showtext::showtext_auto()

GraphData %>% 
  filter(vi %in% c("scale_gravity", "ses_age", "scale_gravity:ses_age")) %>% 
  mutate(sign = as.character(ifelse(pval <= 0.05, "Significatif", "Non-significatif"))) %>% 
  ggplot(aes(x = coef, y = reorder(item, -mean_tol),
             alpha = sign)) +
  scale_x_continuous(limits = c(-1, 1)) +
  ylab("") +
  xlab("\nCoefficient de régression") +
  facet_wrap(~vi, nrow = 1) +
  geom_point(color = "#6CAAF5", size = 1.5) +
  geom_segment(color = "#6CAAF5", size = 1, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#6CAAF5", size = 1, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = NA),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig3_coefficients-tolLevel3.png",
       width = 10, height = 7)


## Sans interaction, sans responsability
GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)
for (i in 1:length(vds)){
  #i <- 1
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity + ses_age + ses_gender_male +
                             educ_level +
                 ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

#sysfonts::font_add_google("Roboto")
#showtext::showtext_auto()

GraphData %>% 
  filter(vi %in% c("scale_gravity", "ses_age")) %>% 
  mutate(sign = as.character(ifelse(pval <= 0.05, "Significatif", "Non-significatif"))) %>% 
  ggplot(aes(x = coef, y = reorder(item, -mean_tol),
             alpha = sign)) +
  scale_x_continuous(limits = c(-1, 1)) +
  ylab("") +
  xlab("\nCoefficient de régression") +
  facet_wrap(~vi, nrow = 1) +
  geom_point(color = "#6CAAF5", size = 1.5) +
  geom_segment(color = "#6CAAF5", size = 1, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#6CAAF5", size = 1, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = NA),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig3_coefficients-tolLevel4.png",
       width = 10, height = 7)

## Tester avec différentes variables d'âge ####

## Variable ordinale (groupes de 5 en une variable ordonnée)
GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)

for (i in 1:length(vds)){
  #i <- 1
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity + age_cat +
                              ses_gender_male +
                             responsability + educ_level +
                 ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

sysfonts::font_add_google("Roboto")
showtext::showtext_auto()

GraphData %>% 
  filter(vi %in% c("scale_gravity", "age_cat", "responsability",
                   "scale_gravity:ses_age")) %>% 
  mutate(sign = as.character(ifelse(pval <= 0.05, "Significatif", "Non-significatif"))) %>% 
  ggplot(aes(x = coef, y = reorder(item, -mean_tol),
             alpha = sign)) +
  scale_x_continuous(limits = c(-1, 1)) +
  ylab("") +
  xlab("\nCoefficient de régression") +
  facet_wrap(~vi, nrow = 1) +
  geom_point(color = "#6CAAF5", size = 1.5) +
  geom_segment(color = "#6CAAF5", size = 1, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#6CAAF5", size = 1, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = NA),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

#ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig3_coefficients-tolLevel5.png",
#       width = 10, height = 7)

## Binaire 35 ans et +
GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)

for (i in 1:length(vds)){
  #i <- 1
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity + over35yo +
                              ses_gender_male +
                             responsability + educ_level +
                 ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

GraphData %>% 
  filter(vi %in% c("scale_gravity", "over35yo", "responsability")) %>% 
  mutate(sign = as.character(ifelse(pval <= 0.05, "Significatif", "Non-significatif"))) %>% 
  ggplot(aes(x = coef, y = reorder(item, -mean_tol),
             alpha = sign)) +
  scale_x_continuous(limits = c(-1, 1)) +
  ylab("") +
  xlab("\nCoefficient de régression") +
  facet_wrap(~vi, nrow = 1) +
  geom_point(color = "#6CAAF5", size = 1.5) +
  geom_segment(color = "#6CAAF5", size = 1, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#6CAAF5", size = 1, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = NA),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig3_coefficients-tolLevel6.png",
       width = 10, height = 7)


## Dummies des catégories de jeunes
GraphData <- data.frame(
  item = as.character(),
  vi = as.character(),
  coef = as.numeric(),
  se = as.numeric(),
  pval = as.numeric()
)

for (i in 1:length(vds)){
  i <- 5
  vd <- vds[i]
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity + age_cat_18 +
                               age_cat_23 + age_cat_28 + age_cat_33 + age_cat_38 +
                               age_cat_43 + age_cat_48 + age_cat_53 + age_cat_58 +
                               age_cat_63 + age_cat_68 + age_cat_73 + age_cat_78 +
                               ses_gender_male + responsability + educ_level +
                               ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  modeli <- eval(parse(text = paste0("lm(", vd, " ~ scale_gravity + age_cat_18 +
                               ses_gender_male + responsability + educ_level +
                               ses_incomeHigh + ses_incomeLow,
               data = Data2)")))
  item <- rep(vd, 9)
  vi <- names(modeli$coefficients)
  coef <- modeli$coefficients
  se <- summary(modeli)$coefficients[,2]
  pval <- round(summary(modeli)$coefficients[,4], 3)
  GraphDatai <- as.data.frame(cbind(item, vi, coef, se, pval),
                              row.names = F)
  GraphData <- rbind(GraphData, GraphDatai)
  GraphData$coef <- as.numeric(GraphData$coef)
  GraphData$se <- as.numeric(GraphData$se)
  GraphData$pval <- as.numeric(GraphData$pval)
}

tol_levels_df <- Data %>% 
  pivot_longer(cols = starts_with("radicalisation_tolerate"),
               names_to = "item", values_to = "tolerance",
               names_prefix = "radicalisation_tolerate_") %>% 
  group_by(item) %>% 
  summarise(mean_item = mean(tolerance))
tol_levels <- tol_levels_df$mean_item
names(tol_levels) <- tol_levels_df$item

GraphData$mean_tol <- tol_levels[GraphData$item]

GraphData %>% 
  filter(vi %in% c("scale_gravity", "responsability",
                   "age_cat_18", "age_cat_23", "age_cat_28",
                   "age_cat_33", "age_cat_38", "age_cat_43",
                   "age_cat_48", "age_cat_53", "age_cat_58",
                   "age_cat_63", "age_cat_68", "age_cat_73",
                   "age_cat_78")) %>% 
  mutate(sign = as.character(ifelse(pval <= 0.05, "Significatif", "Non-significatif"))) %>% 
  ggplot(aes(x = coef, y = reorder(item, -mean_tol),
             alpha = sign)) +
  scale_x_continuous(limits = c(-1, 1)) +
  ylab("") +
  xlab("\nCoefficient de régression") +
  facet_wrap(~vi) +
  geom_point(color =   "#5F95D3", size = 0.75) +
  geom_segment(color = "#5F95D3", size = 0.5, aes(x = coef - se, xend = coef, yend = reorder(item, -mean_tol))) +
  geom_segment(color = "#5F95D3", size = 0.5, aes(xend = coef + se, x = coef, yend = reorder(item, -mean_tol))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  geom_segment(aes(color = sign,
                   yend = reorder(item, -mean_tol)),
               x = -1.5, xend = 1,
               size = 0.2,
               alpha = 0.7) +
  scale_color_manual(values = c("Significatif" = "#5F95D3",
                                  "Non-significatif" = "#E2E2E2"),
                     guide = "none") +
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = NA),
        axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2),
        axis.text.y = element_text(size = 20,
                                   lineheight = 10),
        panel.grid.major.y = element_line(size = 0.25,
                                          color = "#E2E2E2"),
        axis.ticks.y = element_line(size = 0.25,
                                    color = "#E2E2E2"),
        axis.ticks.length.y = unit(0, "cm"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/fig3_coefficients-tolLevel7.png",
       width = 10, height = 7)



# Figures additionnelles: bivariate scale_gravity ####

## Age
Data %>% 
  ggplot(aes(x = ses_age, y = scale_gravity)) +
  geom_jitter(width = 0.2, height = 0.2,
              alpha = 0.5, color = "#6CAAF5") +
  geom_smooth(se = F, size = 2, color = "darkgrey") +
  clessnverse::theme_clean_light(base_size = 35) +
  #clessnverse::theme_clean_light() +
  geom_vline(xintercept = 45) +
  scale_x_continuous(breaks = c(25, 45, 50, 75)) +
  theme(axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))
  

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/scale-gravity_age.png",
       width = 10, height = 7)

Data %>% 
  mutate(over45 = ifelse(ses_age > 45, "46 ans et +", "18-45 ans")) %>% 
  ggplot(aes(x = scale_gravity, y = factor(over45))) +
  geom_density_ridges(scale = 0.95, bandwidth = 0.05,
                      color = "#6CAAF5", fill = "#6CAAF5",
                      alpha = 0.7, quantile_lines=T,
                      quantile_fun=function(x,...)mean(x)) +
  ylab("") +
  scale_x_continuous(breaks = c(0,1)) +
  clessnverse::theme_clean_light(base_size = 35) +
  #clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/scale-gravity_age2.png",
       width = 10, height = 7)

Data %>% 
  mutate(over35 = ifelse(ses_age > 35, "36 ans et +", "18-35 ans")) %>% 
  ggplot(aes(x = scale_gravity, y = factor(over35))) +
  geom_density_ridges(scale = 0.95, bandwidth = 0.05,
                      color = "#6CAAF5", fill = "#6CAAF5",
                      alpha = 0.7, quantile_lines=T,
                      quantile_fun=function(x,...)mean(x)) +
  ylab("") +
  scale_x_continuous(breaks = c(0,1)) +
  clessnverse::theme_clean_light(base_size = 35) +
  #clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/scale-gravity_age3.png",
       width = 10, height = 7)


## Genre
Data %>% 
  mutate(gender = ifelse(ses_gender_male == 1, "male", "female")) %>% 
  ggplot(aes(x = scale_gravity, y = factor(gender))) +
  geom_density_ridges(scale = 0.95, bandwidth = 0.05,
                      color = "#6CAAF5", fill = "#6CAAF5",
                      alpha = 0.7, quantile_lines=T,
                      quantile_fun=function(x,...)mean(x)) +
  ylab("") +
  scale_x_continuous(breaks = c(0,1)) +
  clessnverse::theme_clean_light(base_size = 35) +
  #clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/scale-gravity_gender.png",
       width = 10, height = 7)


## Income
Data %>% 
  mutate(income = ifelse(ses_incomeLow == 1,   "low", NA),
         income = ifelse(ses_incomeMid == 1,   "mid", income),
         income = ifelse(ses_incomeHigh == 1, "high", income)) %>% 
  ggplot(aes(x = scale_gravity, y = factor(income, levels = c("low", "mid", "high")))) +
  geom_density_ridges(scale = 0.95, bandwidth = 0.05,
                      color = "#6CAAF5", fill = "#6CAAF5",
                      alpha = 0.7, quantile_lines=T,
                      quantile_fun=function(x,...)mean(x)) +
  ylab("income") +
  scale_x_continuous(breaks = c(0,1)) +
  clessnverse::theme_clean_light(base_size = 35) +
  #clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/scale-gravity_income.png",
       width = 10, height = 7)


## Educ level
Data %>% 
  mutate(educ_level = ifelse(ses_educBHS == 1, "BHS", NA),
         educ_level = ifelse(ses_educCollege == 1, "College", educ_level),
         educ_level = ifelse(ses_educUniv == 1, "Université", educ_level)) %>% 
  ggplot(aes(x = scale_gravity, y = factor(educ_level, levels = c("BHS", "College", "Université")))) +
  geom_density_ridges(scale = 0.95, bandwidth = 0.05,
                      color = "#6CAAF5", fill = "#6CAAF5",
                      alpha = 0.7, quantile_lines=T,
                      quantile_fun=function(x,...)mean(x)) +
  ylab("") +
  scale_x_continuous(breaks = c(0,1)) +
  clessnverse::theme_clean_light(base_size = 35) +
  #clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5,
                                    lineheight = 0.2))

ggsave("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/scale-gravity_educ.png",
       width = 10, height = 7)

