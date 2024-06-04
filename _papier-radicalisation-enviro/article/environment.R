groups <- c("Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive", "Nonviolent,\nnon-disruptive",
            "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive", "Nonviolent,\ndisruptive",
            "Violence and\nmaterial destruction",
            "Violence and\nmaterial destruction",
            "Violence and\nmaterial destruction",
            "Violence and\nmaterial destruction",
            "Violence and\nmaterial destruction")

names(groups) <- c("signPetition", "boycott", "divest", "manifestation", "occupyPublicSpace",
                   "attachTreeVehicule", "blockBridgeRoad", "blockPipelineConstruction", "vandalismObjects",
                   "sabotagingInfrastructure", "throwingObjectsInfrastructure", "fightPolice", "violatingPowerful")

# Clean item names ####
clean_names <- c("Signing a petition", 
                 "Boycotting products\nand companies",
                 "Divesting investments",
                 "Participating in a manifestation",
                 "Temporarily occupying\na public space",
                 "Tying himself/herself to\na tree or a vehicle",
                 "Blocking a bridge or a road",
                 "Blocking the construction\nof a pipeline",
                 "Vandalizing objects",
                 "Sabotaging infrastructure,\nvehicles, etc.",
                 "Throwing an object at\ninfrastructure, vehicles, etc.",
                 "Confronting police officers\nin a demonstration",
                 "Violating individuals in\npositions of power")

names(clean_names) <- c("signPetition",
                        "boycott",
                        "divest",
                        "manifestation",
                        "occupyPublicSpace",
                        "attachTreeVehicule",
                        "blockBridgeRoad",
                        "blockPipelineConstruction",
                        "vandalismObjects",
                        "sabotagingInfrastructure",
                        "throwingObjectsInfrastructure",
                        "fightPolice", "violatingPowerful")

clean_vis <- c("Citizens", "Governments", "Enterprises")
names(clean_vis) <- c("responsability_climateChangeCitizens",
                      "responsibility_climateChange_Govt",
                      "responsability_climateChangeEnterprise")


get_age_category <- function(ages){
  groups <- seq(from = 13,
                to = max(ages, na.rm = T),
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

get_age_category2 <- function(ages){
  groups <- seq(from = 18,
                to = 110,
                by = 10)
  age_cats <- c()
  for (i in 1:length(ages)){
    if (!is.na(ages[i])){
      age_cats[i] <- max(groups[groups<=ages[i]],
                         na.rm = T)
    } else (age_cats[i] <- NA)
  }
  return(age_cats)
}

margin_error <- function(moyenne, sd, n, niveau_de_confiance = 0.95) {
  z <- qnorm(1 - (1 - niveau_de_confiance) / 2)
  me <- z * (sd / sqrt(n))
  return(me)
}
