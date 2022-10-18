# Packages ####
library(tidyverse)
#source("functions.R", encoding = "UTF-8")

# Note: cleaning of some variables have been done in other files in this repo
####### apsa.R
####### popcorn_22-09-22.R
####### violence_pol.R


# Load Data ####
Data <- haven::read_sav("_SharedFolder_quorum-enviro/data/ULA011-données.Sav") %>%  
  mutate(id = 1:nrow(.))

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

## Propriétaire ####
table(Data$Q14)
CleanData$ses_proprio <- NA
CleanData$ses_proprio[Data$Q14 == 2] <- 1
CleanData$ses_proprio[Data$Q14 != 2] <- 0
table(CleanData$ses_proprio)


###******************************************###
# Perceptions ####
###******************************************###


###******************************************###
# State intervention ####
###******************************************###


###******************************************###
# Responsability ####
###******************************************###

###******************************************###
# International ####
###******************************************###


###******************************************###
# Radicalisation ####
###******************************************###


###******************************************###
# Politics ####
###******************************************###


saveRDS(CleanData, "_SharedFolder_quorum-enviro/data/cleanData/data.rds")
