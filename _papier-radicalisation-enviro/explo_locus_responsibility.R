# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("_papier-radicalisation-enviro/article/environment.R")

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data.rds") %>% 
  filter(ses_age >= 18) %>% 
  mutate(ses_region = case_when(
    ses_region_qc == 1 ~ "Quebec",
    ses_region_ont == 1 ~ "Ontario",
    ses_region_west == 1 ~ "Western provinces",
    ses_region_mari == 1 ~ "Maritimes"
  ),
  ses_region = factor(ses_region),
  age_cat = get_age_category(ses_age),
  age_cat = ifelse(age_cat >= 33, 33, age_cat),
  age_cat = factor(age_cat, ordered = TRUE, levels = c(18, 23, 28, 33)),
  educ_level = ifelse(ses_educBHS == 1, 0, NA),
  educ_level = ifelse(ses_educCollege == 1, 0.5,
                      educ_level),
  educ_level = ifelse(ses_educUniv == 1, 1,
                      educ_level),
  responsibility_climateChange_Govt = (responsability_climateChangeFedGovt + responsability_climateChangeProvGovt) / 2) %>% 
  select(age_cat, ses_region, ses_gender_male, educ_level,
         ses_incomeHigh, ses_incomeLow, ses_bornCanada, scale_gravity,
         responsibility_climateChange_Govt, responsibility_climateChange_Enterprise = responsability_climateChangeEnterprise,
         responsibility_climateChange_Citizens = responsability_climateChangeCitizens)

table(data$age_cat)
unique(data$age_cat)

# Models ----------------------------------------------------------------

vds <- c("responsibility_climateChange_Govt", "responsibility_climateChange_Enterprise",
         "responsibility_climateChange_Citizens")

vis <- c("age_cat", "educ_level", "ses_region")

for (i in vis){
  levels <- sort(unique(data[[i]]))
  model_list <- list()
  for(j in levels){
    for (k in vds){
      formula <- as.formula(paste(k, "~ ."))
      mdata <- data %>%
        filter(!!sym(i) == j) %>% 
        select(-!!sym(i), -all_of(vds[vds != k]))
      model <- lm(formula, data = mdata)
      if (k == vds[1]){
        model_list[[as.character(j)]] <- list()
        }
    model_list[[as.character(j)]][[gsub("responsibility_climateChange_", "", k)]] <- model
    }
  }
  assign(paste("models_", i, sep = ""), model_list)
}


# Graphs ------------------------------------------------------------------

## Age ---------------------------------------------------------------------

for (i in names(models_age_cat)){
  modelsummary::modelplot(models = models_age_cat[[i]],
                          coef_omit = "ses_bornCanada|ses_incomeLow|ses_incomeHigh|ses_gender_male") +
    labs(title = paste0("age: ", i)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    clessnize::theme_clean_light()
  ggsave(paste0("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/explo_locus_responsibility/age_", i, ".png"),
         width = 7, height = 6)
  message(i)
}

# Educ --------------------------------------------------------------------


for (i in names(models_educ_level)){
  modelsummary::modelplot(models = models_educ_level[[i]],
                          coef_omit = "ses_bornCanada|ses_incomeLow|ses_incomeHigh|ses_gender_male") +
    labs(title = paste0("educ: ", i)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    clessnize::theme_clean_light()
  ggsave(paste0("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/explo_locus_responsibility/educ_", i, ".png"),
         width = 7, height = 6)
  message(i)
}

## Region ------------------------------------------------------------------

for (i in names(models_ses_region)){
  modelsummary::modelplot(models = models_ses_region[[i]],
                          coef_omit = "ses_bornCanada|ses_incomeLow|ses_incomeHigh|ses_gender_male") +
    labs(title = paste0("region: ", i)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    clessnize::theme_clean_light()
  ggsave(paste0("_SharedFolder_quorum-enviro/_papier-radicalisation-enviro/graphs/explo_locus_responsibility/region_", i, ".png"),
         width = 7, height = 6)
  message(i)
}
































  
