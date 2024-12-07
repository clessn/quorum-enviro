# Packages ---------------------------------------------------------------
library(dplyr)
library(tinytable)

# Data --------------------------------------------------------------------

#### Load density data
density_data <- readRDS("../responsibility_climate/_SharedFolder_responsibility_climate/data/density_data.rds")

data <- readRDS("_SharedFolder_quorum-enviro/data/cleanData/data_with_clusters.rds") %>% 
  rename(stateInterv_continueIncreaseGESPrice = stateInterv_continueIncreaseGESPrice) %>% 
  mutate(region = case_when(
    ses_prov %in% c("alberta", "sk") ~ "Alberta and\nSaskatchewan",
    ses_prov %in% c("cb") ~ "British Columbia",
    ses_prov %in% c("on") ~ "Ontario",
    ses_prov %in% c("qc") ~ "Quebec",
  ),
  region = ifelse(is.na(region), "Other region", region),
  region = factor(region, levels = c("Quebec", "Ontario",
                                     "British Columbia",
                                     "Alberta and\nSaskatchewan",
                                     "Other region")),
  party_id = factor(politics_idFederal),
  ses_postal_code = as.character(as.vector(ses_postal_code))
  ) %>% 
  left_join(., density_data %>% select(-province), by = c("ses_postal_code" = "fsa"))

data$party_id <- relevel(data$party_id, ref = "PLC")

# Put into long format for easier grouping ------------------------------------------------------

df_long <- data |> 
  tidyr::pivot_longer(
    cols = starts_with(c("stateInterv_continueIncreaseGESPrice", "stateInterv_decreaseFossilProd")),
    names_to = "scenario",
    names_prefix = "stateInterv_",
    values_to = "likert"
  ) |> 
  mutate(
    ses_income = case_when(
      ses_incomeLow == 1 ~ "$0 - $30k",
      ses_incomeMid == 1 ~ "$30k - $110k",
      ses_incomeHigh == 1 ~ "$110k +"
    ),
    ses_income = factor(ses_income, levels = c("$0 - $30k", "$30k - $110k", "$110k +")),
    ses_gender = case_when(
      ses_gender_male == 1 ~ "Male",
      ses_gender_female == 1 ~ "Female",
      ses_gender_other == 1 ~ "Other"
    ),
    ses_age_cat = case_when(
      ses_age_24m == 1 ~ "18-24",
      ses_age_2534 == 1 ~ "25-34",
      ses_age_3544 == 1 ~ "35-44",
      ses_age_4554 == 1 ~ "45-54",
      ses_age_5564 == 1 ~ "55-64",
      ses_age_6574 == 1 ~ "65-74",
      ses_age_75p == 1 ~ "75+"
    ),
    ses_ethn = case_when(
      ses_ethn_white == 1 ~ "White",
      ses_ethn_black == 1 ~ "Other",
      ses_ethn_firstNations == 1 ~ "Other",
      ses_ethn_asian == 1 ~ "Asian",
      ses_ethn_hispanic == 1 ~ "Other",
      ses_ethn_arab == 1 ~ "Other",
      ses_ethn_other == 1 ~ "Other"
    ),
    ses_population_centre = case_when(
      ses_population_centre == "population_centre" ~ "Urban",
      ses_population_centre == "rural" ~ "Rural",
    )
  ) |> 
  select(id, scenario, likert, ses_income, ses_gender, ses_age_cat, ses_population_centre, ses_ethn)


# Grouping ------------------------------------------------------------------

ses_columns <- c("ses_gender", "ses_income", "ses_age_cat", "ses_population_centre", "ses_ethn")
ses_labels <- c("gender", "income", "age_cat", "population_centre", "ethnicity")

result <- lapply(seq_along(ses_columns), function(i) {
  df_long |> 
    group_by(across(all_of(ses_columns[i])), scenario) |> 
    summarise(
      mean = mean(likert, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    ) |> 
    rename(
      category = !!ses_columns[i]
    ) |> 
    mutate(
      variable = ses_labels[i]
    )
}) |> 
  bind_rows() |> 
  tidyr::drop_na() |> 
  filter(n > 15) |> 
  relocate(variable) |> 
  mutate(
    category_label = paste0(category, " (n = ", n, ")")
  ) |> 
  tidyr::pivot_wider(
    id_cols = c("variable", "category_label"),
    names_from = scenario,
    values_from = "mean"
  ) |> 
  rename(
    `Like existing system  ` = continueIncreaseGESPrice,
    `Fossil Jobs  ` = continueIncreaseGESPrice_FossilJobs,
    `Green Jobs  ` = continueIncreaseGESPrice_GreenJobs,
    `Redistributed Pop  ` = continueIncreaseGESPrice_RedistributedPop,
    `Like existing system` = decreaseFossilProd,
    `Fossil Jobs` = decreaseFossilProd_FossilJobs,
    `Green Jobs` = decreaseFossilProd_GreenJobs,
    `Socio-demographic` = category_label
  )

result |>
  select(-variable) |> 
  tt(
    digits = 2,
    caption = "Mean of the likert scale (0 = strongly disagree, 1 = strongly agree)<br>Urban = Population density > 400 people per km²",
  ) |> 
  group_tt(
    i = list(
      "Gender" = which(result$variable == "gender")[1],
      "Income" = which(result$variable == "income")[1],
      "Age" = which(result$variable == "age_cat")[1],
      "Rural/Urban" = which(result$variable == "population_centre")[1],
      "Ethnicity" = which(result$variable == "ethnicity")[1]
    ),
    indent = 3,
    j = list(
      "Continuing to increase price of carbon dioxide emissions" = 2:5,
      "Decreasing fossil fuels production" = 6:8
    )
  ) |> 
  theme_tt(
    theme = "grid",
    width = 0.9
  ) |> 
  style_tt(
    i = 2:23, # Appliquer l'alignement aux cellules, pas à l'en-tête
    j = 2:8, # Appliquer l'alignement aux colonnes du tableau
    align = "c"
  ) |> 
  style_tt(
    j = 1,
    i = 0:23,
    bold = TRUE,
    background = "#f2f2f2",
  ) |> 
  style_tt(
    i = 0,
    j = 1:8,
    background = "#f2f2f2"
  ) |> 
  style_tt(
    j = 5,
    line = "r",
    line_width = 0.2
  ) |>
  format_tt(
    num_zero = TRUE,             # Conserve les zéros terminaux (ex. 0.60)
    num_fmt = "decimal"          # Affiche les nombres en format décimal
  )
  save_tt(
    output = "just_green_transition/annex_subgroups/annex_subgroups.html",
    overwrite = TRUE
  )

