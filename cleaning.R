# Packages ####
library(tidyverse)
#source("functions.R", encoding = "UTF-8")

# Load Data ####
Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav") %>%  
  mutate(id = 1:nrow(.))

# Functions ####
source("functions.R", encoding = "UTF-8")

### Create clean dataframe
CleanData <- data.frame(id = Data$id,
                        ses_postal_code = Data$CP,
                        ses_age = 2022-Data$Q7)

###******************************************###
# SES ####
###******************************************###

## Province ####
table(Data$PROV)
CleanData$ses_prov_alberta <- NA
CleanData$ses_prov_alberta[Data$PROV==1] <- 1
CleanData$ses_prov_alberta[Data$PROV!=1] <- 0
table(CleanData$ses_prov_alberta)

table(Data$PROV)
CleanData$ses_prov_cb <- NA
CleanData$ses_prov_cb[Data$PROV==2] <- 1
CleanData$ses_prov_cb[Data$PROV!=2] <- 0
table(CleanData$ses_prov_cb)

table(Data$PROV)
CleanData$ses_prov_mb <- NA
CleanData$ses_prov_mb[Data$PROV==3] <- 1
CleanData$ses_prov_mb[Data$PROV!=3] <- 0
table(CleanData$ses_prov_mb)

table(Data$PROV)
CleanData$ses_prov_nb <- NA
CleanData$ses_prov_nb[Data$PROV==4] <- 1
CleanData$ses_prov_nb[Data$PROV!=4] <- 0
table(CleanData$ses_prov_nb)

table(Data$PROV)
CleanData$ses_prov_tnl <- NA
CleanData$ses_prov_tnl[Data$PROV==5] <- 1
CleanData$ses_prov_tnl[Data$PROV!=5] <- 0
table(CleanData$ses_prov_tnl)

table(Data$PROV)
CleanData$ses_prov_ns <- NA
CleanData$ses_prov_ns[Data$PROV==6] <- 1
CleanData$ses_prov_ns[Data$PROV!=6] <- 0
table(CleanData$ses_prov_ns)

table(Data$PROV)
CleanData$ses_prov_on <- NA
CleanData$ses_prov_on[Data$PROV==9] <- 1
CleanData$ses_prov_on[Data$PROV!=9] <- 0
table(CleanData$ses_prov_on)

table(Data$PROV)
CleanData$ses_prov_ipe <- NA
CleanData$ses_prov_ipe[Data$PROV==10] <- 1
CleanData$ses_prov_ipe[Data$PROV!=10] <- 0
table(CleanData$ses_prov_ipe)

table(Data$PROV)
CleanData$ses_prov_qc <- NA
CleanData$ses_prov_qc[Data$PROV==11] <- 1
CleanData$ses_prov_qc[Data$PROV!=11] <- 0
table(CleanData$ses_prov_qc)

table(Data$PROV)
CleanData$ses_prov_sk <- NA
CleanData$ses_prov_sk[Data$PROV==12] <- 1
CleanData$ses_prov_sk[Data$PROV!=12] <- 0
table(CleanData$ses_prov_sk)

####### using pivot longer to create categorized variable
CleanData$ses_prov <- CleanData %>% 
  pivot_longer(., cols = starts_with("ses_prov_"),
               names_to = "ses_prov",
               names_prefix = "ses_prov_") %>% 
  filter(value==1) %>% 
  pull(., ses_prov)

## Gender ####
table(Data$SEXE)
CleanData$ses_gender_male <- NA
CleanData$ses_gender_male[Data$SEXE==1] <- 1
CleanData$ses_gender_male[Data$SEXE!=1] <- 0
table(CleanData$ses_gender_male)

table(Data$SEXE)
CleanData$ses_gender_female <- NA
CleanData$ses_gender_female[Data$SEXE==2] <- 1
CleanData$ses_gender_female[Data$SEXE!=2] <- 0
table(CleanData$ses_gender_female)


# Gender ####
table(Data$SEXE)
CleanData$ses_gender_other <- NA
CleanData$ses_gender_other[Data$SEXE==3] <- 1
CleanData$ses_gender_other[Data$SEXE!=3] <- 0
table(CleanData$ses_gender_other)

## Region (only in quebec) ####
table(Data$REGION)
CleanData$ses_region_mtlIsl <- NA
CleanData$ses_region_mtlIsl[Data$REGION==1] <- 1
CleanData$ses_region_mtlIsl[Data$REGION%in%c(2,3,4)] <- 0
table(CleanData$ses_region_mtlIsl)

table(Data$REGION)
CleanData$ses_region_mtlRMR <- NA
CleanData$ses_region_mtlRMR[Data$REGION==2] <- 1
CleanData$ses_region_mtlRMR[Data$REGION%in%c(1,3,4)] <- 0
table(CleanData$ses_region_mtlRMR)

table(Data$REGION)
CleanData$ses_region_qc <- NA
CleanData$ses_region_qc[Data$REGION==3] <- 1
CleanData$ses_region_qc[Data$REGION%in%c(1,2,4)] <- 0
table(CleanData$ses_region_qc)

table(Data$REGION)
CleanData$ses_region_outsideQcMtl <- NA
CleanData$ses_region_outsideQcMtl[Data$REGION==4] <- 1
CleanData$ses_region_outsideQcMtl[Data$REGION%in%c(1,2,3)] <- 0
table(CleanData$ses_region_outsideQcMtl)

## Age group ####
table(Data$AGE)
CleanData$ses_age_24m <- NA
CleanData$ses_age_24m[Data$AGE==2] <- 1
CleanData$ses_age_24m[Data$AGE!=2] <- 0
table(CleanData$ses_age_24m)

table(Data$AGE)
CleanData$ses_age_2534 <- NA
CleanData$ses_age_2534[Data$AGE==3] <- 1
CleanData$ses_age_2534[Data$AGE!=3] <- 0
table(CleanData$ses_age_2534)

table(Data$AGE)
CleanData$ses_age_3544 <- NA
CleanData$ses_age_3544[Data$AGE==4] <- 1
CleanData$ses_age_3544[Data$AGE!=4] <- 0
table(CleanData$ses_age_3544)

table(Data$AGE)
CleanData$ses_age_4554 <- NA
CleanData$ses_age_4554[Data$AGE==5] <- 1
CleanData$ses_age_4554[Data$AGE!=5] <- 0
table(CleanData$ses_age_4554)

table(Data$AGE)
CleanData$ses_age_5564 <- NA
CleanData$ses_age_5564[Data$AGE==6] <- 1
CleanData$ses_age_5564[Data$AGE!=6] <- 0
table(CleanData$ses_age_5564)

table(Data$AGE)
CleanData$ses_age_6574 <- NA
CleanData$ses_age_6574[Data$AGE==7] <- 1
CleanData$ses_age_6574[Data$AGE!=7] <- 0
table(CleanData$ses_age_6574)

table(Data$AGE)
CleanData$ses_age_75p <- NA
CleanData$ses_age_75p[Data$AGE==8] <- 1
CleanData$ses_age_75p[Data$AGE!=8] <- 0
table(CleanData$ses_age_75p)

## Language ####
table(Data$Q2)
CleanData$ses_langFr <- NA
CleanData$ses_langFr[Data$Q2==1] <- 1
CleanData$ses_langFr[Data$Q2!=1] <- 0
table(CleanData$ses_langFr)

table(Data$Q2)
CleanData$ses_langEn <- NA
CleanData$ses_langEn[Data$Q2==2] <- 1
CleanData$ses_langEn[Data$Q2!=2] <- 0
table(CleanData$ses_langEn)

table(Data$Q2)
CleanData$ses_langOther <- NA
CleanData$ses_langOther[Data$Q2==3] <- 1
CleanData$ses_langOther[Data$Q2!=3] <- 0
table(CleanData$ses_langOther)

## Civil status ####
table(Data$Q3)
CleanData$ses_married <- NA
CleanData$ses_married[Data$Q3==1] <- 1
CleanData$ses_married[Data$Q3!=1] <- 0
table(CleanData$ses_married)

table(Data$Q3)
CleanData$ses_couple <- NA
CleanData$ses_couple[Data$Q3==2] <- 1
CleanData$ses_couple[Data$Q3!=2] <- 0
table(CleanData$ses_couple)

table(Data$Q3)
CleanData$ses_separated <- NA
CleanData$ses_separated[Data$Q3%in%c(4,5)] <- 1
CleanData$ses_separated[!(Data$Q3%in%c(4,5))] <- 0
table(CleanData$ses_separated)

table(Data$Q3)
CleanData$ses_widow <- NA
CleanData$ses_widow[Data$Q3==6] <- 1
CleanData$ses_widow[Data$Q3!=6] <- 0
table(CleanData$ses_widow)

table(Data$Q3)
CleanData$ses_neverMarried <- NA
CleanData$ses_neverMarried[Data$Q3==7] <- 1
CleanData$ses_neverMarried[Data$Q3!=7] <- 0
table(CleanData$ses_neverMarried)

## Born in canada ####
table(Data$Q4)
CleanData$ses_bornCanada <- NA
CleanData$ses_bornCanada[Data$Q4==1] <- 1
CleanData$ses_bornCanada[Data$Q4!=1] <- 0
table(CleanData$ses_bornCanada)

## year in canada ####
CleanData$ses_yearCanada <- Data$Q5

## ethnicity ####
table(Data$Q6)
CleanData$ses_ethn_white <- NA
CleanData$ses_ethn_white[Data$Q6==1] <- 1
CleanData$ses_ethn_white[Data$Q6!=1] <- 0
table(CleanData$ses_ethn_white)

table(Data$Q6)
CleanData$ses_ethn_black <- NA
CleanData$ses_ethn_black[Data$Q6==2] <- 1
CleanData$ses_ethn_black[Data$Q6!=2] <- 0
table(CleanData$ses_ethn_black)

table(Data$Q6)
CleanData$ses_ethn_firstNations <- NA
CleanData$ses_ethn_firstNations[Data$Q6==3] <- 1
CleanData$ses_ethn_firstNations[Data$Q6!=3] <- 0
table(CleanData$ses_ethn_firstNations)

table(Data$Q6)
CleanData$ses_ethn_asian <- NA
CleanData$ses_ethn_asian[Data$Q6==4] <- 1
CleanData$ses_ethn_asian[Data$Q6!=4] <- 0
table(CleanData$ses_ethn_asian)

table(Data$Q6)
CleanData$ses_ethn_hispanic <- NA
CleanData$ses_ethn_hispanic[Data$Q6==5] <- 1
CleanData$ses_ethn_hispanic[Data$Q6!=5] <- 0
table(CleanData$ses_ethn_hispanic)

table(Data$Q6)
CleanData$ses_ethn_arab <- NA
CleanData$ses_ethn_arab[Data$Q6==6] <- 1
CleanData$ses_ethn_arab[Data$Q6!=6] <- 0
table(CleanData$ses_ethn_arab)

table(Data$Q6)
CleanData$ses_ethn_other <- NA
CleanData$ses_ethn_other[Data$Q6==7] <- 1
CleanData$ses_ethn_other[Data$Q6!=7] <- 0
table(CleanData$ses_ethn_other)


## Educ ####
table(Data$Q8)
CleanData$ses_educBHS <- NA
CleanData$ses_educBHS[Data$Q8 <= 3] <- 1
CleanData$ses_educBHS[Data$Q8 > 3] <- 0
table(CleanData$ses_educBHS)

table(Data$Q8)
CleanData$ses_educCollege <- NA
CleanData$ses_educCollege[Data$Q8 %in% c(4,6)] <- 1
CleanData$ses_educCollege[!(Data$Q8 %in% c(4,6))] <- 0
table(CleanData$ses_educCollege)

table(Data$Q8)
CleanData$ses_educUniv <- NA
CleanData$ses_educUniv[Data$Q8 %in% c(5,7,8)] <- 1
CleanData$ses_educUniv[!(Data$Q8 %in% c(5,7,8))] <- 0
table(CleanData$ses_educUniv)

## Student ####
table(Data$Q9)
CleanData$ses_student <- NA
CleanData$ses_student[Data$Q9 == 1] <- 1
CleanData$ses_student[Data$Q9 == 2] <- 0
table(CleanData$ses_student)

## Income ####
table(Data$Q10)
CleanData$ses_incomeLow <- NA
CleanData$ses_incomeLow[Data$Q10 <= 2] <- 1
CleanData$ses_incomeLow[Data$Q10 > 2] <- 0
table(CleanData$ses_incomeLow)

table(Data$Q10)
CleanData$ses_incomeMid <- NA
CleanData$ses_incomeMid[Data$Q10 %in% 3:5] <- 1
CleanData$ses_incomeMid[!(Data$Q10 %in% 3:5)] <- 0
table(CleanData$ses_incomeMid)

table(Data$Q10)
CleanData$ses_incomeHigh <- NA
CleanData$ses_incomeHigh[Data$Q10 >= 6] <- 1
CleanData$ses_incomeHigh[Data$Q10 < 6] <- 0
table(CleanData$ses_incomeHigh)

## Occupation ####
table(Data$Q11)
CleanData$ses_isUnemployed <- NA
CleanData$ses_isUnemployed[Data$Q11 == 7] <- 1
CleanData$ses_isUnemployed[Data$Q11 != 7] <- 0
table(CleanData$ses_isUnemployed)

CleanData$ses_isRetired <- NA
CleanData$ses_isRetired[Data$Q11 == 8] <- 1
CleanData$ses_isRetired[Data$Q11 != 8] <- 0
table(CleanData$ses_isRetired)

CleanData$ses_isStudent <- NA
CleanData$ses_isStudent[Data$Q11 == 9] <- 1
CleanData$ses_isStudent[Data$Q11 != 9] <- 0
table(CleanData$ses_isStudent)

CleanData$ses_isEducation_Research_Government <- NA
CleanData$ses_isEducation_Research_Government[Data$Q11 == 1] <- 1
CleanData$ses_isEducation_Research_Government[Data$Q11 != 1] <- 0
table(CleanData$ses_isEducation_Research_Government)

CleanData$ses_isConstruction <- NA
CleanData$ses_isConstruction[Data$Q11 == 2] <- 1
CleanData$ses_isConstruction[Data$Q11 != 2] <- 0
table(CleanData$ses_isConstruction)

CleanData$ses_isTransformation <- NA
CleanData$ses_isTransformation[Data$Q11 == 3] <- 1
CleanData$ses_isTransformation[Data$Q11 != 3] <- 0
table(CleanData$ses_isTransformation)

CleanData$ses_isFabrication <- NA
CleanData$ses_isFabrication[Data$Q11 == 4] <- 1
CleanData$ses_isFabrication[Data$Q11 != 4] <- 0
table(CleanData$ses_isFabrication)

CleanData$ses_isNatSciences <- NA
CleanData$ses_isNatSciences[Data$Q11 == 5] <- 1
CleanData$ses_isNatSciences[Data$Q11 != 5] <- 0
table(CleanData$ses_isNatSciences)

CleanData$ses_isPrimaryIndustries <- NA
CleanData$ses_isPrimaryIndustries[Data$Q11 == 6] <- 1
CleanData$ses_isPrimaryIndustries[Data$Q11 != 6] <- 0
table(CleanData$ses_isPrimaryIndustries)

## Kids ####
table(Data$Q13)
CleanData$ses_noHouseholdKids <- NA
CleanData$ses_noHouseholdKids[Data$Q13 == 0] <- 1
CleanData$ses_noHouseholdKids[Data$Q13 != 0] <- 0
table(CleanData$ses_noHouseholdKids)

CleanData$ses_fewHouseholdKids <- NA
CleanData$ses_fewHouseholdKids[Data$Q13 == 1 | Data$Q13 == 2 ] <- 1
CleanData$ses_fewHouseholdKids[Data$Q13 != 1 & Data$Q13 != 2] <- 0
table(CleanData$ses_fewHouseholdKids)

CleanData$ses_manyHouseholdKids <- NA
CleanData$ses_manyHouseholdKids[Data$Q13 >= 3] <- 1
CleanData$ses_manyHouseholdKids[Data$Q13 <= 2] <- 0
table(CleanData$ses_manyHouseholdKids)


## Propriétaire ####
table(Data$Q14)
CleanData$ses_proprio <- NA
CleanData$ses_proprio[Data$Q14 == 2] <- 1
CleanData$ses_proprio[Data$Q14 != 2] <- 0
table(CleanData$ses_proprio)

## Household ####
table(Data$Q15)
CleanData$ses_householdFewerThan4 <- NA
CleanData$ses_householdFewerThan4[Data$Q15 <= 4] <- 1
CleanData$ses_householdFewerThan4[Data$Q15 >= 5] <- 0
table(CleanData$ses_householdFewerThan4)

table(Data$Q15)
CleanData$ses_householdMoreThan4 <- NA
CleanData$ses_householdMoreThan4[Data$Q15 >= 5] <- 1
CleanData$ses_householdMoreThan4[Data$Q15 <= 4] <- 0
table(CleanData$ses_householdMoreThan4)

## Religion ####
table(Data$Q16)
CleanData$ses_relProtest <- NA
CleanData$ses_relProtest[Data$Q16 == 1] <- 1
CleanData$ses_relProtest[Data$Q16 != 1] <- 0
table(CleanData$ses_relProtest)

CleanData$ses_relCatho <- NA
CleanData$ses_relCatho[Data$Q16 == 2] <- 1
CleanData$ses_relCatho[Data$Q16 != 2] <- 0
table(CleanData$ses_relCatho)

CleanData$ses_relOrtho <- NA
CleanData$ses_relOrtho[Data$Q16 == 3] <- 1
CleanData$ses_relOrtho[Data$Q16 != 3] <- 0
table(CleanData$ses_relOrtho)

CleanData$ses_relIslam <- NA
CleanData$ses_relIslam[Data$Q16 == 4] <- 1
CleanData$ses_relIslam[Data$Q16 != 4] <- 0
table(CleanData$ses_relIslam)

CleanData$ses_relJewish <- NA
CleanData$ses_relJewish[Data$Q16 == 5] <- 1
CleanData$ses_relJewish[Data$Q16 != 5] <- 0
table(CleanData$ses_relJewish)

CleanData$ses_relBoud <- NA
CleanData$ses_relBoud[Data$Q16 == 6] <- 1
CleanData$ses_relBoud[Data$Q16 != 6] <- 0
table(CleanData$ses_relBoud)

CleanData$ses_relHindu <- NA
CleanData$ses_relHindu[Data$Q16 == 7] <- 1
CleanData$ses_relHindu[Data$Q16 != 7] <- 0
table(CleanData$ses_relHindu)

CleanData$ses_relOther <- NA
CleanData$ses_relOther[Data$Q16 == 8] <- 1
CleanData$ses_relOther[Data$Q16 != 8] <- 0
table(CleanData$ses_relOther)

CleanData$ses_relNone <- NA
CleanData$ses_relNone[Data$Q16 == 9] <- 1
CleanData$ses_relNone[Data$Q16 != 9] <- 0
table(CleanData$ses_relNone)

## Life by religion ####

table(Data$Q17)
CleanData$ses_lifeReligion <- NA
CleanData$ses_lifeReligion[Data$Q17 == 1] <- 0
CleanData$ses_lifeReligion[Data$Q17 == 2] <- 0.25
CleanData$ses_lifeReligion[Data$Q17 == 3] <- 0.50
CleanData$ses_lifeReligion[Data$Q17 == 4] <- 0.75
CleanData$ses_lifeReligion[Data$Q17 == 5] <- 1
table(CleanData$ses_lifeReligion)

## Religious events ####

table(Data$Q18)
CleanData$ses_attendsReligiousEvents <- NA
CleanData$ses_attendsReligiousEvents[Data$Q18 == 1] <- 0
CleanData$ses_attendsReligiousEvents[Data$Q18 == 2] <- 0.25
CleanData$ses_attendsReligiousEvents[Data$Q18 == 3] <- 0.50
CleanData$ses_attendsReligiousEvents[Data$Q18 == 4] <- 0.75
CleanData$ses_attendsReligiousEvents[Data$Q18 == 5] <- 1
table(CleanData$ses_attendsReligiousEvents)

## God existence ####

table(Data$Q19)

CleanData$ses_existsUniqueGod <- NA
CleanData$ses_existsUniqueGod[Data$Q19 == 1] <- 1
CleanData$ses_existsUniqueGod[Data$Q19 != 1] <- 0
table(CleanData$ses_existsUniqueGod)

CleanData$ses_existsVitalForce <- NA
CleanData$ses_existsVitalForce[Data$Q19 == 2] <- 1
CleanData$ses_existsVitalForce[Data$Q19 != 2] <- 0
table(CleanData$ses_existsVitalForce)

CleanData$ses_existsDK <- NA
CleanData$ses_existsDK[Data$Q19 == 3] <- 1
CleanData$ses_existsDK[Data$Q19 != 3] <- 0
table(CleanData$ses_existsDK)

CleanData$ses_existsNoGod <- NA
CleanData$ses_existsNoGod[Data$Q19 == 4] <- 1
CleanData$ses_existsNoGod[Data$Q19 != 4] <- 0
table(CleanData$ses_existsNoGod)

## sexual orientation ####

table(Data$Q20)

CleanData$ses_hetero <- NA
CleanData$ses_hetero[Data$Q20 == 1] <- 1
CleanData$ses_hetero[Data$Q20 != 1] <- 0
table(CleanData$ses_hetero)

CleanData$ses_gai <- NA
CleanData$ses_gai[Data$Q20 == 2] <- 1
CleanData$ses_gai[Data$Q20 != 2] <- 0
table(CleanData$ses_gai)

CleanData$ses_bisex <- NA
CleanData$ses_bisex[Data$Q20 == 3] <- 1
CleanData$ses_bisex[Data$Q20 != 3] <- 0
table(CleanData$ses_bisex)

CleanData$ses_sexOri_other <- NA
CleanData$ses_sexOri_other[Data$Q20 >= 4] <- 1
CleanData$ses_sexOri_other[Data$Q20 <= 3] <- 0
table(CleanData$ses_sexOri_other)

## Electric vehicule ####

table(Data$Q74)
CleanData$electricVehicule <- NA
CleanData$electricVehicule[Data$Q74 == 1] <- 1
CleanData$electricVehicule[Data$Q74 == 2] <- 0
table(CleanData$electricVehicule)

## Dwelling ####
table(Data$Q123)


###******************************************###
# Perceptions ####
###******************************************###

## J'ai personnellement été affecté.e par les effets du réchauffement climatique ###
table(Data$Q22_A4)
CleanData$preception_isAffectedByClimateChange <- NA
CleanData$perception_isAffectedByClimateChange[Data$Q22_A4 == 1] <- 0
CleanData$perception_isAffectedByClimateChange[Data$Q22_A4 == 2] <- 0.25
CleanData$perception_isAffectedByClimateChange[Data$Q22_A4 == 3] <- 0.50
CleanData$perception_isAffectedByClimateChange[Data$Q22_A4 == 4] <- 0.75
CleanData$perception_isAffectedByClimateChange[Data$Q22_A4 == 5] <- 1
table(CleanData$perception_isAffectedByClimateChange)

# Q34_A3 Les humains sont destinés à dominer le reste de la nature
table(Data$Q34_A3)
CleanData$perception_humansDominateNature <- NA
CleanData$perception_humansDominateNature <- minmaxNormalization(Data$Q34_A3)
table(CleanData$perception_humansDominateNature) # 0 = Fortement en désaccord... 1 = Fortement en accord


###******************************************###
# Gravity of the issue ####
###******************************************###

# Q40 Les changements climatiques constituent une menace pour moi au cours de ma vie.
table(Data$Q40)
CleanData$gravity_climateChangePersonalMenace <- NA
CleanData$gravity_climateChangePersonalMenace <- minmaxNormalization(Data$Q40)
table(CleanData$gravity_climateChangePersonalMenace) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q41 À quel point êtes-vous préoccupé.e par le réchauffement climatique?
table(Data$Q41)
CleanData$gravity_worriedClimateChange <- NA
CleanData$gravity_worriedClimateChange <- minmaxNormalization(Data$Q41)
table(CleanData$gravity_worriedClimateChange) # 0 = Pas du tout... 1 = Énormément

# Q42 Quand pensez-vous que les changements climatiques commenceront à nuire aux Canadiens
table(Data$Q42)
Data$Q42[Data$Q42==7] <- NA
CleanData$gravity_whenWillHarmCanadians <- NA
CleanData$gravity_whenWillHarmCanadians <- minmaxNormalization(Data$Q42)
table(CleanData$gravity_whenWillHarmCanadians) # 0 = Jamais, 0.2 = Dans 100 ans, 0.4 = 50 ans, 0.6 = 25 ans, 0.8 = 10 ans, 1 = Maintenant

# Q43_A1 Si les choses continuent ainsi, nous connaîtrons bientôt une catastrophe écologique majeure
table(Data$Q43_A1)
CleanData$gravity_majorCatastrophe <- NA
CleanData$gravity_majorCatastrophe <- minmaxNormalization(Data$Q43_A1)
table(CleanData$gravity_majorCatastrophe) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q43_A2 La soi-disante « crise écologique » à laquelle l'humanité fait face est grandement exagérée
table(Data$Q43_A2)
CleanData$gravity_crisisIsExaggerated <- NA
CleanData$gravity_crisisIsExaggerated <- minmaxNormalization(Data$Q43_A2)
table(CleanData$gravity_crisisIsExaggerated) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q72_A3 Les changements climatiques mèneront à la fin de l'humanité
table(Data$Q72_A3)
CleanData$gravity_climateChangeEndHumanity <- NA
CleanData$gravity_climateChangeEndHumanity <- minmaxNormalization(Data$Q72_A3)
table(CleanData$gravity_climateChangeEndHumanity) # 0 = Fortement en désaccord... 1 = Fortement en accord


###******************************************###
# Science, technology, IA ####
###******************************************###

## À quel point êtes-vous certain.e que les changements climatiques sont en train de se produire? ##

table(Data$Q27)
CleanData$science_climateChangeIsHappening <- NA
CleanData$science_climateChangeIsHappening[Data$Q27 == 1] <- 0
CleanData$science_climateChangeIsHappening[Data$Q27 == 2] <- 0.33
CleanData$science_climateChangeIsHappening[Data$Q27 == 3] <- 0.67
CleanData$science_climateChangeIsHappening[Data$Q27 == 4] <- 1
table(CleanData$science_climateChangeIsHappening)


# Q49 - Faites-vous confiance aux scientifiques comme source d'information sur les enjeux environnementaux?
table(Data$Q49)
CleanData$science_trustScientists <- NA
CleanData$science_trustScientists <- minmaxNormalization(Data$Q49)
table(CleanData$science_trustScientists)


# Q50_A1 - Il y a un consensus établi en science du climat que les changements climatiques sont en cours
table(Data$Q50_A1)
CleanData$science_consensusClimateChange <- NA
CleanData$science_consensusClimateChange <- minmaxNormalization(Data$Q50_A1)
table(CleanData$science_consensusClimateChange) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q50_A2 - L’intelligence artificielle est un bon outil pour lutter contre les changements climatiques
table(Data$Q50_A2)
CleanData$science_AIToolAgainstClimateChange <- NA
CleanData$science_AIToolAgainstClimateChange <- minmaxNormalization(Data$Q50_A2)
table(CleanData$science_AIToolAgainstClimateChange) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q50_A3 - J'en connais suffisamment sur l'intelligence artificielle pour comprendre son impact potentiel sur ma vie privée
table(Data$Q50_A3)
CleanData$science_knowEnoughAI <- NA
CleanData$science_knowEnoughAI <- minmaxNormalization(Data$Q50_A3)
table(CleanData$science_knowEnoughAI) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q50_A4 - Je suis au courant des derniers développements technologiques dans mes champs d'intérêts
table(Data$Q50_A4)
CleanData$science_updatedTechDevsMyFields <- NA
CleanData$science_updatedTechDevsMyFields <- minmaxNormalization(Data$Q50_A4)
table(CleanData$science_updatedTechDevsMyFields) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q50_A5 - Le progrès technologique peut être une solution aux problèmes environnementaux
table(Data$Q50_A5)
CleanData$science_techClimateChangeSolution <- NA
CleanData$science_techClimateChangeSolution <- minmaxNormalization(Data$Q50_A5)
table(CleanData$science_techClimateChangeSolution) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q43_A3 Les émissions des voitures ne contribuent pas aux changements climatiques causés par l'être humain
### Codage inversé pour éviter double négation, l'énoncé devient donc:
####### Les émissions des voitures contribuent aux changements climatiques causés par l'être humain
table(Data$Q43_A3)
CleanData$science_carContributeClimateChange <- NA
CleanData$science_carContributeClimateChange <- finverser(Data$Q43_A3)
CleanData$science_carContributeClimateChange <- minmaxNormalization(CleanData$science_carContributeClimateChange)
table(CleanData$science_carContributeClimateChange) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q43_A4 Les scientifiques exagèrent les preuves des changements climatiques
table(Data$Q43_A4)
CleanData$science_scientistsExaggerateClimateChangeEvidence <- NA
CleanData$science_scientistsExaggerateClimateChangeEvidence <- minmaxNormalization(Data$Q43_A4)
table(CleanData$science_scientistsExaggerateClimateChangeEvidence) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q48 À votre avis, quelle est la principale cause des changements climatiques?
table(Data$Q48)

## Processus naturels
table(Data$Q48)
CleanData$science_climateChangeMainCause_naturalProcesses <- 0
CleanData$science_climateChangeMainCause_naturalProcesses[Data$Q48 == 1] <- 1
table(CleanData$science_climateChangeMainCause_naturalProcesses)

## Activités humaines
table(Data$Q48)
CleanData$science_climateChangeMainCause_humanActivities <- 0
CleanData$science_climateChangeMainCause_humanActivities[Data$Q48 == 2] <- 1
table(CleanData$science_climateChangeMainCause_humanActivities)

## Nature et humains
table(Data$Q48)
CleanData$science_climateChangeMainCause_natureAndHumans <- 0
CleanData$science_climateChangeMainCause_natureAndHumans[Data$Q48 == 3] <- 1
table(CleanData$science_climateChangeMainCause_natureAndHumans)

# Q72_A2 Dans l'ensemble, la technologie a été une bonne chose pour l'humanité
table(Data$Q72_A2)
CleanData$science_techGoodForHumanity <- NA
CleanData$science_techGoodForHumanity <- minmaxNormalization(Data$Q72_A2)
table(CleanData$science_techGoodForHumanity) # 0 = Fortement en désaccord... 1 = Fortement en accord


# Êtes-vous en accord ou en désaccord avec les énoncés suivants 
# :-Les avancées technologiques vont rendre inutiles nos efforts environnementaux individuels

table(Data$Q63_A3)
CleanData$science_climateChangeTechnoEffortsNull <- NA
CleanData$science_climateChangeTechnoEffortsNull[Data$Q63_A3 == 1] <- 0
CleanData$science_climateChangeTechnoEffortsNull[Data$Q63_A3 == 2] <- 0.25
CleanData$science_climateChangeTechnoEffortsNull[Data$Q63_A3 == 3] <- 0.5
CleanData$science_climateChangeTechnoEffortsNull[Data$Q63_A3 == 4] <- 0.75
CleanData$science_climateChangeTechnoEffortsNull[Data$Q63_A3 == 5] <- 1
table(CleanData$science_climateChangeTechnoEffortsNull)

###******************************************###
# State intervention ####
###******************************************###

# Q21 - Seriez-vous en faveur d'une transition vers une économie canadienne excluant les énergies fossiles 
# (tels que le pétrole, le gaz naturel et le charbon)?
table(Data$Q21)
CleanData$stateInterv_EconomyCANoFossil <- NA
CleanData$stateInterv_EconomyCANoFossil <- minmaxNormalization(Data$Q21)
table(CleanData$stateInterv_EconomyCANoFossil) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q22_A3 - Nous devrions développer les pistes cyclables et les transports publics, 
# même si cela implique de réduire l'espace pour les voitures
table(Data$Q22_A3)
CleanData$stateInterv_moreBikePathPubTransport <- NA
CleanData$stateInterv_moreBikePathPubTransport <- minmaxNormalization(Data$Q22_A3)
table(CleanData$stateInterv_moreBikePathPubTransport) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q22_A5 - Nous devrions rendre l'essence et le diesel plus chers pour nous inciter à moins utiliser la voiture
table(Data$Q22_A5)
CleanData$stateInterv_makeGasMoreExpensive <- NA
CleanData$stateInterv_makeGasMoreExpensive <- minmaxNormalization(Data$Q22_A5)
table(CleanData$stateInterv_makeGasMoreExpensive) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q25 - À quel point le gouvernement pourrait-il en faire plus pour lutter contre les changements climatiques?
table(Data$Q25)
CleanData$stateInterv_govtDoMoreClimateChange <- NA
CleanData$stateInterv_govtDoMoreClimateChange <- minmaxNormalization(Data$Q25)
table(CleanData$stateInterv_govtDoMoreClimateChange) # 0 = Rien de plus; 0.5 = Un peu plus; 1 = Beaucoup plus

# Q31_A1 - Accorder des remboursements d'impôts aux personnes qui achètent des véhicules électriques
table(Data$Q31_A1)
CleanData$stateInterv_electricCarTaxRefund <- NA
CleanData$stateInterv_electricCarTaxRefund <- minmaxNormalization(Data$Q31_A1)
table(CleanData$stateInterv_electricCarTaxRefund) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q31_A2 - Imposer davantage de réglementations et d'interdictions pour empêcher les gens de nuire à l'environnement
table(Data$Q31_A2)
CleanData$stateInterv_moreRegulationsEnviro <- NA
CleanData$stateInterv_moreRegulationsEnviro <- minmaxNormalization(Data$Q31_A2)
table(CleanData$stateInterv_moreRegulationsEnviro) # 0 = Fortement en désaccord... 1 = Fortement en accord


# Augmenter prix des GES
## Êtes-vous en |nbsp;faveur ou en défaveur avec les énoncés suivants :-Que l'on continue d'augmenter le prix des émissions de gaz à effet de serre (comme le fait le système actuellement en vigueur au Canada)
# Q55_A1 - Que l'on continue d'augmenter le prix des émissions de gaz à effet de serre (comme le fait le système actuellement en vigueur au Canada)
table(Data$Q55_A1)
CleanData$stateInterv_continueIncreaseGESPrice <- NA
CleanData$stateInterv_continueIncreaseGESPrice <- minmaxNormalization(Data$Q55_A1)
table(CleanData$stateInterv_continueIncreaseGESPrice) # 0 = Fortement en défaveur... 1 = Fortement en faveur

# Q57_A1 - Seriez-vous en faveur de poursuivre la hausse du prix des émissions de dioxyde de carbone (comme c'est le cas avec l'actuel système au Canada) si les fonds étaient...
# Redistribués à la population, afin que les ménages aient plus d'argent dans leurs poches
table(Data$Q57_A1)
CleanData$stateInterv_continueIncreaseGESPrice_RedistributedPop <- NA
CleanData$stateInterv_continueIncreaseGESPrice_RedistributedPop <- minmaxNormalization(Data$Q57_A1)
table(CleanData$stateInterv_continueIncreaseGESPrice_RedistributedPop) # 0 = Fortement en défaveur... 1 = Fortement en faveur

# Q57_A2 - Seriez-vous en faveur de poursuivre la hausse du prix des émissions de dioxyde de carbone (comme c'est le cas avec l'actuel système au Canada)
## si|nbsp;les fonds étaient...-Utilisés pour créer des emplois dans les domaines de l'énergie verte, du transport e
table(Data$Q57_A2)
CleanData$stateInterv_continueIncreaseGESPrice_GreenJobs <- NA
CleanData$stateInterv_continueIncreaseGESPrice_GreenJobs <- minmaxNormalization(Data$Q57_A2)
table(CleanData$stateInterv_continueIncreaseGESPrice_GreenJobs) # 0 = Fortement en défaveur... 1 = Fortement en faveur

# Q57_A3 - Seriez-vous en faveur de poursuivre la hausse du prix des émissions de dioxyde de carbone (comme c'est le cas avec l'actuel système au Canada)
## si|nbsp;les fonds étaient...-Utilisés pour soutenir les travailleurs du secteur des combustibles fossiles, avec d
table(Data$Q57_A3)
CleanData$stateInterv_continueIncreaseGESPrice_FossilJobs <- NA
CleanData$stateInterv_continueIncreaseGESPrice_FossilJobs <- minmaxNormalization(Data$Q57_A3)
table(CleanData$stateInterv_continueIncreaseGESPrice_FossilJobs) # 0 = Fortement en défaveur... 1 = Fortement en faveur


# Diminution progressive de prod de combustibles
## Êtes-vous en |nbsp;faveur ou en défaveur avec les énoncés suivants :-D'une diminution progressive de la production de combustibles fossiles au Canada
# Q55_A2 - D'une diminution progressive de la production de combustibles fossiles au Canada
table(Data$Q55_A2)
CleanData$stateInterv_decreaseFossilProd <- NA
CleanData$stateInterv_decreaseFossilProd <- minmaxNormalization(Data$Q55_A2)
table(CleanData$stateInterv_decreaseFossilProd) # 0 = Fortement en défaveur... 1 = Fortement en faveur

# Q60 - Seriez-vous en faveur d'une diminution progressive de la production de combustibles fossiles au Canada
## si davantage d'emplois (de qualité équivalente) étaient créés dans les domaines de l'énergie, du transport et des technologies vertes que perdus au cours
table(Data$Q60)
CleanData$stateInterv_decreaseFossilProd_GreenJobs <- NA
CleanData$stateInterv_decreaseFossilProd_GreenJobs <- minmaxNormalization(Data$Q60)
table(CleanData$stateInterv_decreaseFossilProd_GreenJobs) # 0 = Fortement en défaveur... 1 = Fortement en faveur

# Q61 - Seriez-vous en faveur d'une diminution progressive de la production de combustibles fossiles au Canada
## si les travailleurs du secteur des combustibles fossiles étaient assurés d'être soutenus dans la transition, par exemple à travers des formations rémunér
table(Data$Q61)
CleanData$stateInterv_decreaseFossilProd_FossilJobs <- NA
CleanData$stateInterv_decreaseFossilProd_FossilJobs <- minmaxNormalization(Data$Q61)
table(CleanData$stateInterv_decreaseFossilProd_FossilJobs) # 0 = Fortement en défaveur... 1 = Fortement en faveur


###******************************************###
# Responsability ####
###******************************************###

## Mon propre mode de vie a contribué aux problèmes environnementaux actuels ###

table(Data$Q34_A1)
CleanData$responsability_lifestyleContributedClimateChange <- NA
CleanData$responsability_lifestyleContributedClimateChange[Data$Q34_A1 == 1] <- 0
CleanData$responsability_lifestyleContributedClimateChange[Data$Q34_A1 == 2] <- 0.25
CleanData$responsability_lifestyleContributedClimateChange[Data$Q34_A1 == 3] <- 0.50
CleanData$responsability_lifestyleContributedClimateChange[Data$Q34_A1 == 3] <- 0.5
CleanData$responsability_lifestyleContributedClimateChange[Data$Q34_A1 == 4] <- 0.75
CleanData$responsability_lifestyleContributedClimateChange[Data$Q34_A1 == 5] <- 1
table(CleanData$responsability_lifestyleContributedClimateChange)


#À quel point votre famille et vos amis font-ils des efforts pour lutter contre les changements climatiques?
table(Data$Q29)
CleanData$responsability_effortsClimateChangeFamily <- NA
CleanData$responsability_effortsClimateChangeFamily[Data$Q29 == 1] <- 0
CleanData$responsability_effortsClimateChangeFamily[Data$Q29 == 2] <- 0.33
CleanData$responsability_effortsClimateChangeFamily[Data$Q29 == 3] <- 0.67
CleanData$responsability_effortsClimateChangeFamily[Data$Q29 == 4] <- 1
table(CleanData$responsability_effortsClimateChangeFamily)


# À quelle fréquence discutez-vous du réchauffement climatique avec votre famille et vos amis?
table(Data$Q30)
CleanData$responsability_climateChangeDiscuss <- NA
CleanData$responsability_climateChangeDiscuss[Data$Q30 == 1] <- 0
CleanData$responsability_climateChangeDiscuss[Data$Q30 == 2] <- 0.33
CleanData$responsability_climateChangeDiscuss[Data$Q30 == 3] <- 0.67
CleanData$responsability_climateChangeDiscuss[Data$Q30 == 4] <- 1
table(CleanData$responsability_climateChangeDiscuss)

# À quel point ressentez-vous une responsabilité individuelle de protéger l'environnement?
table(Data$Q32)
CleanData$responsability_individualToProtect <- NA
CleanData$responsability_individualToProtect[Data$Q32 == 1] <- 0
CleanData$responsability_individualToProtect[Data$Q32 == 2] <- 0.5
CleanData$responsability_individualToProtect[Data$Q32 == 3] <- 1
table(CleanData$responsability_individualToProtect)

# Êtes-vous en accord ou en désaccord avec les énoncés suivants :-Les citoyens ordinaires, 
# et pas seulement les autorités et les décideurs, ont une grande part de responsabilité à l'égard de l'environnement

table(Data$Q34_A2)
CleanData$responsability_citizensVsGvnt <- NA
CleanData$responsability_citizensVsGvnt[Data$Q34_A2 == 1] <- 0
CleanData$responsability_citizensVsGvnt[Data$Q34_A2 == 2] <- 0.25
CleanData$responsability_citizensVsGvnt[Data$Q34_A2 == 3] <- 0.5
CleanData$responsability_citizensVsGvnt[Data$Q34_A2 == 4] <- 0.75
CleanData$responsability_citizensVsGvnt[Data$Q34_A2 == 5] <- 1
table(CleanData$responsability_citizensVsGvnt)

#Quel est le degré de responsabilité des acteurs suivants en ce qui a trait à la
#prise de mesures visant la protection de l'environnement|nbsp;»:-Gouvernement fédéral

table(Data$Q45_A1)
CleanData$responsability_climateChangeFedGovt <- NA
CleanData$responsability_climateChangeFedGovt[Data$Q45_A1 == 1] <- 0
CleanData$responsability_climateChangeFedGovt[Data$Q45_A1 == 2] <- 0.33
CleanData$responsability_climateChangeFedGovt[Data$Q45_A1 == 3] <- 0.67
CleanData$responsability_climateChangeFedGovt[Data$Q45_A1 == 4] <- 1
table(CleanData$responsability_climateChangeFedGovt)

# Quel est le degré de responsabilité des acteurs suivants en ce qui a trait à la 
# « prise de mesures visant la protection de l'environnement|nbsp;»:-Gouvernements provinciaux

table(Data$Q45_A2)
CleanData$responsability_climateChangeProvGovt <- NA
CleanData$responsability_climateChangeProvGovt[Data$Q45_A2 == 1] <- 0
CleanData$responsability_climateChangeProvGovt[Data$Q45_A2 == 2] <- 0.33
CleanData$responsability_climateChangeProvGovt[Data$Q45_A2 == 3] <- 0.67
CleanData$responsability_climateChangeProvGovt[Data$Q45_A2 == 4] <- 1
table(CleanData$responsability_climateChangeProvGovt)

# Quel est le degré de responsabilité des acteurs suivants en ce qui a trait à la
# « prise de mesures visant la protection de l'environnement|nbsp;»:-Administrations municipales

table(Data$Q45_A3)
CleanData$responsability_climateChangeMuniGovt <- NA
CleanData$responsability_climateChangeMuniGovt[Data$Q45_A3 == 1] <- 0
CleanData$responsability_climateChangeMuniGovt[Data$Q45_A3 == 2] <- 0.33
CleanData$responsability_climateChangeMuniGovt[Data$Q45_A3 == 3] <- 0.67
CleanData$responsability_climateChangeMuniGovt[Data$Q45_A3 == 4] <- 1
table(CleanData$responsability_climateChangeMuniGovt)


# Quel est le degré de responsabilité des acteurs suivants en ce qui a trait à la
# « prise de mesures visant la protection de l'environnement|nbsp;»:-Organismes à but non lucratif

table(Data$Q45_A4)
CleanData$responsability_climateChangeOBNL <- NA
CleanData$responsability_climateChangeOBNL[Data$Q45_A4 == 1] <- 0
CleanData$responsability_climateChangeOBNL[Data$Q45_A4 == 2] <- 0.33
CleanData$responsability_climateChangeOBNL[Data$Q45_A4 == 3] <- 0.67
CleanData$responsability_climateChangeOBNL[Data$Q45_A4 == 4] <- 1
table(CleanData$responsability_climateChangeOBNL)

# Quel est le degré de responsabilité des acteurs suivants en ce qui a trait à la 
# « prise de mesures visant la protection de l'environnement|nbsp;»:-Entreprises

table(Data$Q45_A5)
CleanData$responsability_climateChangeEnterprise <- NA
CleanData$responsability_climateChangeEnterprise[Data$Q45_A5 == 1] <- 0
CleanData$responsability_climateChangeEnterprise[Data$Q45_A5 == 2] <- 0.33
CleanData$responsability_climateChangeEnterprise[Data$Q45_A5 == 3] <- 0.67
CleanData$responsability_climateChangeEnterprise[Data$Q45_A5 == 4] <- 1
table(CleanData$responsability_climateChangeEnterprise)

# Quel est le degré de responsabilité des acteurs suivants en ce qui a trait à la
# « prise de mesures visant la protection de l'environnement|nbsp;»:-Particuliers

table(Data$Q45_A6)
CleanData$responsability_climateChangeCitizens <- NA
CleanData$responsability_climateChangeCitizens[Data$Q45_A6 == 1] <- 0
CleanData$responsability_climateChangeCitizens[Data$Q45_A6 == 2] <- 0.33
CleanData$responsability_climateChangeCitizens[Data$Q45_A6 == 3] <- 0.67
CleanData$responsability_climateChangeCitizens[Data$Q45_A6 == 4] <- 1
table(CleanData$responsability_climateChangeCitizens)


# Consommez-vous de la viande (ou des produits d'origine animale)?
table(Data$Q67)
CleanData$responsability_MeatFrequency <- NA
CleanData$responsability_MeatFrequency[Data$Q67 == 1] <- 1
CleanData$responsability_MeatFrequency[Data$Q67 == 2] <- 0.5
CleanData$responsability_MeatFrequency[Data$Q67 %in% c(3,4)] <- 0
table(CleanData$responsability_MeatFrequency)

table(Data$Q67)
CleanData$responsability_vege <- NA
CleanData$responsability_vege[Data$Q67 == 3] <- 1
CleanData$responsability_vege[Data$Q67 %in% c(1,2,4)] <- 0
table(CleanData$responsability_vege)

table(Data$Q67)
CleanData$responsability_vegan <- NA
CleanData$responsability_vegan[Data$Q67 == 4] <- 1
CleanData$responsability_vegan[Data$Q67 %in% c(1,2,3)] <- 0
table(CleanData$responsability_vegan)


# Êtes-vous en |nbsp;faveur ou en défaveur avec les énoncés suivants :-Les autorités et les décideurs,
# et pas seulement les citoyens ordinaires, ont une grande part de responsabilité à l'égard de l'environnement

table(Data$Q72_A2)
CleanData$responsability_GvntVsCitizens <- NA
CleanData$responsability_GvntVsCitizens[Data$Q72_A2 == 1] <- 0
CleanData$responsability_GvntVsCitizens[Data$Q72_A2 == 2] <- 0.25
CleanData$responsability_GvntVsCitizens[Data$Q72_A2 == 3] <- 0.5
CleanData$responsability_GvntVsCitizens[Data$Q72_A2 == 4] <- 0.75
CleanData$responsability_GvntVsCitizens[Data$Q72_A2 == 5] <- 1
table(CleanData$responsability_GvntVsCitizens)


###******************************************###
# International ####
###******************************************###

# Q34_A4 Les pays riches comme le Canada ont une obligation morale de faire preuve de
##       leadership international en réduisant leurs émissions de gaz à effet de serre
table(Data$Q34_A4)
CleanData$international_richCountriesMustLead <- NA
CleanData$international_richCountriesMustLead <- minmaxNormalization(Data$Q34_A4)
table(CleanData$international_richCountriesMustLead) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q38 À quel point pensez-vous que le réchauffement climatique nuira aux Canadien.ne.s?
table(Data$Q38)
CleanData$international_climateChangeHarmCA <- NA
CleanData$international_climateChangeHarmCA <- minmaxNormalization(Data$Q38)
table(CleanData$international_climateChangeHarmCA) # 0 = Pas du tout... 1 = Énormément

# Q39 À quel point pensez-vous que le réchauffement climatique nuira aux populations des pays en développement?
table(Data$Q39)
CleanData$international_climateChangeHarmDevelopment <- NA
CleanData$international_climateChangeHarmDevelopment <- minmaxNormalization(Data$Q39)
table(CleanData$international_climateChangeHarmDevelopment) # 0 = Pas du tout... 1 = Énormément

# Q64_A1 Le Canada devrait accepter les réfugiés provenant de pays aux prises avec des catastrophes écologiques
table(Data$Q64_A1)
CleanData$international_CanadaShouldAcceptRefugees <- NA
CleanData$international_CanadaShouldAcceptRefugees <- minmaxNormalization(Data$Q64_A1)
table(CleanData$international_CanadaShouldAcceptRefugees) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q64_A2 Les pays riches devraient aider les pays devant gérer des réfugiés climatiques
table(Data$Q64_A2)
CleanData$international_richCountriesHelpRefugees <- NA
CleanData$international_richCountriesHelpRefugees <- minmaxNormalization(Data$Q64_A2)
table(CleanData$international_richCountriesHelpRefugees) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Pensez-vous que le Canada devrait admettre:
# Q68 Immigrants
table(Data$Q68)
CleanData$international_nbImmigrantsCanada <- NA
CleanData$international_nbImmigrantsCanada[Data$Q68 == 1] <- 1 # Plus d'immigrants
CleanData$international_nbImmigrantsCanada[Data$Q68 == 2] <- 0 # Moins d'immigrants
CleanData$international_nbImmigrantsCanada[Data$Q68 == 3] <- 0.5 # A peu près le même nombre d'immigrants
table(CleanData$international_nbImmigrantsCanada)

# Q69 Réfugiés
table(Data$Q69)
CleanData$international_nbRefugeesCanada <- NA
CleanData$international_nbRefugeesCanada[Data$Q69 == 1] <- 1 # Plus de réfugiés
CleanData$international_nbRefugeesCanada[Data$Q69 == 2] <- 0 # Moins de réfugiés
CleanData$international_nbRefugeesCanada[Data$Q69 == 3] <- 0.5 # A peu près le même nombre de réfugiés
table(CleanData$international_nbRefugeesCanada)


###******************************************###
# Radicalisation ####
###******************************************###

# Q62 Êtes-vous en accord ou en désaccord avec les énoncés suivants :Les citoyens mécontents du gouvernement ne doivent jamais recourir<
##    à la violence pour exprimer leurs sentiments?
table(Data$Q62)
CleanData$radicalisation_angryJustifiesViolence <- NA
CleanData$radicalisation_angryJustifiesViolence <- minmaxNormalization(Data$Q62)
table(CleanData$radicalisation_angryJustifiesViolence) # 0 = Fortement en désaccord... 1 = Fortement en accord

# Q63 À quel point seriez-vous prêt à tolérer les actions politiques suivantes pour l’avancement de la cause climatique?

## Signer une pétition
table(Data$Q63_A1)
CleanData$radicalisation_tolerate_signPetition <- NA
CleanData$radicalisation_tolerate_signPetition <- minmaxNormalization(Data$Q63_A1)
table(CleanData$radicalisation_tolerate_signPetition) # 0 = aucune tolérance... 1 = Tolérance élevée

## Boycotter des produits et compagnies
table(Data$Q63_A2)
CleanData$radicalisation_tolerate_boycott <- NA
CleanData$radicalisation_tolerate_boycott <- minmaxNormalization(Data$Q63_A2)
table(CleanData$radicalisation_tolerate_boycott) # 0 = aucune tolérance... 1 = Tolérance élevée

## Désinvestir des placements
table(Data$Q63_A3)
CleanData$radicalisation_tolerate_divest <- NA
CleanData$radicalisation_tolerate_divest <- minmaxNormalization(Data$Q63_A3)
table(CleanData$radicalisation_tolerate_divest) # 0 = aucune tolérance... 1 = Tolérance élevée

## Participer à une manifestation
table(Data$Q63_A4)
CleanData$radicalisation_tolerate_manifestation <- NA
CleanData$radicalisation_tolerate_manifestation <- minmaxNormalization(Data$Q63_A4)
table(CleanData$radicalisation_tolerate_manifestation) # 0 = aucune tolérance... 1 = Tolérance élevée

## Occuper temporairement un espace public
table(Data$Q63_A5)
CleanData$radicalisation_tolerate_occupyPublicSpace <- NA
CleanData$radicalisation_tolerate_occupyPublicSpace <- minmaxNormalization(Data$Q63_A5)
table(CleanData$radicalisation_tolerate_occupyPublicSpace) # 0 = aucune tolérance... 1 = Tolérance élevée

## S'attacher arbre véhicule
table(Data$Q63_A6)
CleanData$radicalisation_tolerate_attachTreeVehicule <- NA
CleanData$radicalisation_tolerate_attachTreeVehicule <- minmaxNormalization(Data$Q63_A6)
table(CleanData$radicalisation_tolerate_attachTreeVehicule) # 0 = aucune tolérance... 1 = Tolérance élevée

## Bloquer pont ou route
table(Data$Q63_A7)
CleanData$radicalisation_tolerate_blockBridgeRoad <- NA
CleanData$radicalisation_tolerate_blockBridgeRoad <- minmaxNormalization(Data$Q63_A7)
table(CleanData$radicalisation_tolerate_blockBridgeRoad) # 0 = aucune tolérance... 1 = Tolérance élevée

## Bloquer pont ou route
table(Data$Q63_A7)
CleanData$radicalisation_tolerate_blockBridgeRoad <- NA
CleanData$radicalisation_tolerate_blockBridgeRoad <- minmaxNormalization(Data$Q63_A7)
table(CleanData$radicalisation_tolerate_blockBridgeRoad) # 0 = aucune tolérance... 1 = Tolérance élevée

## Bloquer construction oléoduc
table(Data$Q63_A8)
CleanData$radicalisation_tolerate_blockPipelineConstruction <- NA
CleanData$radicalisation_tolerate_blockPipelineConstruction <- minmaxNormalization(Data$Q63_A8)
table(CleanData$radicalisation_tolerate_blockPipelineConstruction) # 0 = aucune tolérance... 1 = Tolérance élevée

## Vandalisme sur objets
table(Data$Q63_A9)
CleanData$radicalisation_tolerate_vandalismObjects <- NA
CleanData$radicalisation_tolerate_vandalismObjects <- minmaxNormalization(Data$Q63_A9)
table(CleanData$radicalisation_tolerate_vandalismObjects) # 0 = aucune tolérance... 1 = Tolérance élevée

## Saboter infrastructures, véhicules
table(Data$Q63_A10)
CleanData$radicalisation_tolerate_sabotagingInfrastructure <- NA
CleanData$radicalisation_tolerate_sabotagingInfrastructure <- minmaxNormalization(Data$Q63_A10)
table(CleanData$radicalisation_tolerate_sabotagingInfrastructure) # 0 = aucune tolérance... 1 = Tolérance élevée

## Tirer objet sur infrastructures
table(Data$Q63_A11)
CleanData$radicalisation_tolerate_throwingObjectsInfrastructure <- NA
CleanData$radicalisation_tolerate_throwingObjectsInfrastructure <- minmaxNormalization(Data$Q63_A11)
table(CleanData$radicalisation_tolerate_throwingObjectsInfrastructure) # 0 = aucune tolérance... 1 = Tolérance élevée

## Affronter policiers dans manifestation 
table(Data$Q63_A12)
CleanData$radicalisation_tolerate_fightPolice <- NA
CleanData$radicalisation_tolerate_fightPolice <- minmaxNormalization(Data$Q63_A12)
table(CleanData$radicalisation_tolerate_fightPolice) # 0 = aucune tolérance... 1 = Tolérance élevée

## Violenter individus en position de pouvoir
table(Data$Q63_A13)
CleanData$radicalisation_tolerate_violatingPowerful <- NA
CleanData$radicalisation_tolerate_violatingPowerful <- minmaxNormalization(Data$Q63_A13)
table(CleanData$radicalisation_tolerate_violatingPowerful) # 0 = aucune tolérance... 1 = Tolérance élevée


###******************************************###
# Economy ####
###******************************************###

## Nous nous inquiétons trop de l'avenir de l'environnement et pas assez des questions économiques d'aujourd'hui ###

table(Data$Q22_A1)
CleanData$economy_worryTooMuchAboutEnvironment <- NA
CleanData$economy_worryTooMuchAboutEnvironment[Data$Q22_A1 == 1] <- 0
CleanData$economy_worryTooMuchAboutEnvironment[Data$Q22_A1 == 2] <- 0.25
CleanData$economy_worryTooMuchAboutEnvironment[Data$Q22_A1 == 3] <- 0.5
CleanData$economy_worryTooMuchAboutEnvironment[Data$Q22_A1 == 4] <- 0.75
CleanData$economy_worryTooMuchAboutEnvironment[Data$Q22_A1 == 5] <- 1
table(CleanData$economy_worryTooMuchAboutEnvironment)

## Les actions du gouvernement pour réduire les émissions de gaz à effet de serre nuisent à l'économie ###

table(Data$Q22_A2)
CleanData$economy_governmentClimatePolicyHurtsEconomy <- NA
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 1] <- 0
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 2] <- 0.25
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 3] <- 0.5
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 4] <- 0.75
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 5] <- 1
table(CleanData$economy_governmentClimatePolicyHurtsEconomy)


###******************************************###
# Politics ####
###******************************************###

# Q70 En politique fédérale, vous considérez-vous habituellement
## comme un.e conservateur.trice, libéral.e, néo-démocrate, bloquiste, vert.e, un partisan du PPC, ou rien de cela?
table(Data$Q70)
CleanData$politics_idFederal <- NA
CleanData$politics_idFederal[Data$Q70==1] <- "PCC"
CleanData$politics_idFederal[Data$Q70==2] <- "PLC"
CleanData$politics_idFederal[Data$Q70==3] <- "NPD"
CleanData$politics_idFederal[Data$Q70==4] <- "BQ"
CleanData$politics_idFederal[Data$Q70==5] <- "PVC"
CleanData$politics_idFederal[Data$Q70==6] <- "PPC"
CleanData$politics_idFederal[Data$Q70==7] <- "noId"

#### Conservateur 
table(Data$Q70)
CleanData$politics_idFederal_PCC <- 0
CleanData$politics_idFederal_PCC[Data$Q70==1] <- 1
table(CleanData$politics_idFederal_PCC)

#### Liberal
table(Data$Q70)
CleanData$politics_idFederal_PLC <- 0
CleanData$politics_idFederal_PLC[Data$Q70==2] <- 1
table(CleanData$politics_idFederal_PLC)

#### NPD
table(Data$Q70)
CleanData$politics_idFederal_NPD <- 0
CleanData$politics_idFederal_NPD[Data$Q70==3] <- 1
table(CleanData$politics_idFederal_NPD)

#### BQ
table(Data$Q70)
CleanData$politics_idFederal_BQ <- 0
CleanData$politics_idFederal_BQ[Data$Q70==4] <- 1
table(CleanData$politics_idFederal_BQ)

#### PVC
table(Data$Q70)
CleanData$politics_idFederal_PVC <- 0
CleanData$politics_idFederal_PVC[Data$Q70==5] <- 1
table(CleanData$politics_idFederal_PVC)

#### PPC 
table(Data$Q70)
CleanData$politics_idFederal_PPC <- 0
CleanData$politics_idFederal_PPC[Data$Q70==6] <- 1
table(CleanData$politics_idFederal_PPC)

#### No id
table(Data$Q70)
CleanData$politics_idFederal_noId <- 0
CleanData$politics_idFederal_noId[Data$Q70==7] <- 1
table(CleanData$politics_idFederal_noId)



# Q71 En politique provinciale, vous considérez-vous habituellement
## comme un.e caquiste, libéral.e, solidaire, péquiste, conservateur.trice ou rien de cela?
table(Data$Q71)
CleanData$politics_idProvincial <- NA
CleanData$politics_idProvincial[Data$Q71==1] <- "CAQ"
CleanData$politics_idProvincial[Data$Q71==2] <- "PLQ"
CleanData$politics_idProvincial[Data$Q71==3] <- "QS"
CleanData$politics_idProvincial[Data$Q71==4] <- "PQ"
CleanData$politics_idProvincial[Data$Q71==5] <- "PCQ"
CleanData$politics_idProvincial[Data$Q71==6] <- "noId"

#### CAQ 
table(Data$Q71)
CleanData$politics_idProvincial_CAQ <- 0
CleanData$politics_idProvincial_CAQ[Data$Q71==1] <- 1
table(CleanData$politics_idProvincial_CAQ)

#### PLQ 
table(Data$Q71)
CleanData$politics_idProvincial_PLQ <- 0
CleanData$politics_idProvincial_PLQ[Data$Q71==2] <- 1
table(CleanData$politics_idProvincial_PLQ)

#### QS
table(Data$Q71)
CleanData$politics_idProvincial_QS <- 0
CleanData$politics_idProvincial_QS[Data$Q71==3] <- 1
table(CleanData$politics_idProvincial_QS)

#### PQ
table(Data$Q71)
CleanData$politics_idProvincial_PQ <- 0
CleanData$politics_idProvincial_PQ[Data$Q71==4] <- 1
table(CleanData$politics_idProvincial_PQ)

#### PCQ
table(Data$Q71)
CleanData$politics_idProvincial_PCQ <- 0
CleanData$politics_idProvincial_PCQ[Data$Q71==5] <- 1
table(CleanData$politics_idProvincial_PCQ)

#### No id
table(Data$Q71)
CleanData$politics_idProvincial_noId <- 0
CleanData$politics_idProvincial_noId[Data$Q71==6] <- 1
table(CleanData$politics_idProvincial_noId)

###******************************************###
# Complot COVID Mathieu Turgeon ####
###******************************************###

CleanData$covid_origin_A <- Data$Q76A
CleanData$covid_origin_B <- Data$Q76B
CleanData$covid_origin_C <- Data$Q76C
CleanData$covid_origin_D <- Data$Q76D


###******************************************###
# Scales ####
###******************************************###

###******************************************###
## Scepticism ####
###******************************************###

### Prep variables ####

## 0 = pas sceptique, 1 = scepticisme 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleScep_ prefix

table(CleanData$gravity_whenWillHarmCanadians)
CleanData$scaleScep_whenWillHarmCanadians <- finverser(CleanData$gravity_whenWillHarmCanadians)
table(CleanData$scaleScep_whenWillHarmCanadians)

table(CleanData$gravity_crisisIsExaggerated)
CleanData$scaleScep_crisisIsExaggerated <- CleanData$gravity_crisisIsExaggerated
table(CleanData$scaleScep_crisisIsExaggerated)

table(CleanData$science_trustScientists)
CleanData$scaleScep_trustScientists <- finverser(CleanData$science_trustScientists)
table(CleanData$scaleScep_trustScientists)

table(CleanData$science_consensusClimateChange)
CleanData$scaleScep_consensusClimateChange <- finverser(CleanData$science_consensusClimateChange)
table(CleanData$scaleScep_consensusClimateChange)

table(CleanData$science_carContributeClimateChange)
CleanData$scaleScep_carContribute <- finverser(CleanData$science_carContributeClimateChange)
table(CleanData$scaleScep_carContribute)

table(CleanData$science_scientistsExaggerateClimateChangeEvidence)
CleanData$scaleScep_scientistsExaggerate <- CleanData$science_scientistsExaggerateClimateChangeEvidence
table(CleanData$scaleScep_scientistsExaggerate)

# human causal effect
table(CleanData$science_climateChangeMainCause_naturalProcesses)
table(CleanData$science_climateChangeMainCause_natureAndHumans)
table(CleanData$science_climateChangeMainCause_humanActivities)
CleanData$scaleScep_natureCauseClimateChange <- NA
CleanData$scaleScep_natureCauseClimateChange[CleanData$science_climateChangeMainCause_humanActivities == 1] <- 0
CleanData$scaleScep_natureCauseClimateChange[CleanData$science_climateChangeMainCause_naturalProcesses == 1] <- 1
CleanData$scaleScep_natureCauseClimateChange[CleanData$science_climateChangeMainCause_natureAndHumans == 1] <- 1
table(CleanData$scaleScep_natureCauseClimateChange)

# Make scale
CleanData <- CleanData %>% 
  mutate(scale_scepticism = (CleanData %>%
                        select(starts_with("scaleScep_")) %>%
                        rowSums())/length(names(CleanData %>% select(starts_with("scaleScep_")))))


table(Data$Q22_A2)
CleanData$economy_governmentClimatePolicyHurtsEconomy <- NA
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 1] <- 0
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 2] <- 0.25
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 3] <- 0.50
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 4] <- 0.75
CleanData$economy_governmentClimatePolicyHurtsEconomy[Data$Q22_A1 == 5] <- 1
table(CleanData$economy_governmentClimatePolicyHurtsEconomy)


###******************************************###
## Gravity ####
###******************************************###

### Prep variables ####

## 0 = pas inquiet, situation pas grave, 1 = inquiet, situation grave 
### Reverse the variables that need to be reversed with finverser
### Add them as a new variable with the scaleGravity_ prefix

# climateChangePersonalMenace
table(CleanData$gravity_climateChangePersonalMenace)
CleanData$scaleGravity_climateChangePersonalMenace <- CleanData$gravity_climateChangePersonalMenace
table(CleanData$scaleGravity_climateChangePersonalMenace)

# worriedClimateChange
table(CleanData$gravity_worriedClimateChange)
CleanData$scaleGravity_worriedClimateChange <- CleanData$gravity_worriedClimateChange
table(CleanData$scaleGravity_worriedClimateChange)

# majorCatastrophe
table(CleanData$gravity_majorCatastrophe)
CleanData$scaleGravity_majorCatastrophe <- CleanData$gravity_majorCatastrophe
table(CleanData$scaleGravity_majorCatastrophe)

# climateChangeEndHumanity
table(CleanData$gravity_climateChangeEndHumanity)
CleanData$scaleGravity_climateChangeEndHumanity <- CleanData$gravity_climateChangeEndHumanity
table(CleanData$scaleGravity_climateChangeEndHumanity)

# Make scale
CleanData <- CleanData %>% 
  mutate(scale_gravity = (CleanData %>%
                           select(starts_with("scaleGravity_")) %>%
                           rowSums())/length(names(CleanData %>% select(starts_with("scaleGravity_")))))


###******************************************###
## Radicalisation ####
###******************************************###

### Prep variables ####

## 0 = pas radical, 1 = radical 
### Reverse the variables that need to be reversed with finverser

# Make scale
CleanData <- CleanData %>% 
  mutate(scale_radicalisation = (CleanData %>%
                            select(starts_with("radicalisation_")) %>%
                            rowSums())/length(names(CleanData %>% select(starts_with("radicalisation_")))))


###******************************************###
# Remove duplicated columns that made the scales ####
###******************************************###

CleanData <- CleanData %>% 
  select(-starts_with(c("scaleScep_", "scaleGravity_")))


###******************************************###
# Save to RDS ####
###******************************************###
saveRDS(CleanData, "_SharedFolder_quorum-enviro/data/cleanData/data.rds")
write.csv(CleanData, "_SharedFolder_quorum-enviro/data/cleanData/data.csv",
          row.names = F, fileEncoding = "UTF-8")
