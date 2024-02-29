### Code for Educational Mobility and Attitudes toward Immigrants: A Cross-national Comparison ####
## 1. Preparing data ##
# This code explains:
# 1) How to load the dataset dowloaded in step 0 (refer to 0_AccessingData.R) and
# 2) How to recode variables for the analysis
# 3) How to select the relevant observations in the final sample 

#### Required packages

library(tidyverse)#data manipulation

# 1) loading working dataset and selecting sub-population ####

# Loading dataset
load("./data/ess_mobil.RData")


## 2) Data transformations ####

## 2a) Recoding educational level (edulvla)####
#r_education: respondent's education with three levels
#m_education: mother's education with three levels
#f_education: father's education with three levels
#p_education: highest parental education with three levels

ess_mobil <- 
  ess_mobil %>% 
  mutate(r_education = case_when(eisced %in% c(1,2,3) ~ 1, #low: below upper sec.
                                 eisced %in% c(4,5) ~ 2, #medium:upper sec non-tert
                                 eisced %in% c(6,7) ~ 3, #high:tertiary
                                 eisced == 55 ~ NA_real_, eisced %in% c(77,88,99) ~ NA_real_,
                                 is.na(eisced) ~ NA_real_),
         m_education = case_when(eiscedm %in% c(1,2,3) ~ 1, #low: below upper sec.
                                 eiscedm %in% c(4,5) ~ 2, #medium:upper sec non-tert
                                 eiscedm %in% c(6,7) ~ 3, #high:tertiary
                                 eiscedm == 55 ~ NA_real_, eiscedm %in% c(77,88,99) ~ NA_real_,
                                 is.na(eiscedm) ~ NA_real_),
         f_education = case_when(eiscedf %in% c(1,2,3) ~ 1, #low: below upper sec.
                                 eiscedf %in% c(4,5) ~ 2, #medium:upper sec non-tert
                                 eiscedf %in% c(6,7) ~ 3, #high:tertiary
                                 eiscedf == 55 ~ NA_real_, eiscedf %in% c(77,88,99) ~ NA_real_,
                                 is.na(eiscedf) ~ NA_real_)) %>% 
  mutate(p_education = case_when(is.na(m_education) ~ as.numeric(f_education),#if NA mother then father
                                 is.na(f_education) ~ as.numeric(m_education), #if NA father then mother
                                 TRUE ~ pmax(m_education, f_education)))#the highest level btwn parents

# 2b) Creating mobility variables ####

ess_mobil <- 
  ess_mobil %>% 
  mutate(mobility = case_when(as.numeric(r_education) == as.numeric(p_education) ~ "0_No_mobility",
                              as.numeric(r_education) > as.numeric(p_education) ~ "Upward",
                              as.numeric(r_education) < as.numeric(p_education) ~ "Downward",
                              TRUE ~ NA_character_),
         downward = if_else(mobility == "Downward", 1, 0),
         upward = if_else(mobility == "Upward", 1, 0),
         stable = if_else(mobility == "0_No_mobility", 1, 0),
         mobile = if_else(mobility != "0_No_mobility", 1, 0),
         diagonal = case_when(r_education == 1 & p_education == 1 ~ "1Low_low",
                              r_education == 2 & p_education == 2 ~ "2Medium_medium",
                              r_education == 3 & p_education == 3 ~ "3High_high",
                              TRUE ~ "4Mobility"))

# 2c) Recoding origin and destination for DMM models #####
#Dref function only accepts values as factors
#this code transforms education from respondent and parents in factors:
#fweight: final weight combining population weight an post-stratification weight

ess_mobil <- 
  ess_mobil %>% 
  mutate(r_education = as_factor(r_education),
         p_education = as_factor(p_education),
         fweight = dweight*pweight)


# 2d) Recoding control variables####

ess_mobil <- ess_mobil %>% 
  mutate(gender = case_when(gndr == 1 ~ "Male",
                            gndr == 2 ~ "Female",
                            is.na(gndr) ~ NA_character_),
         age = case_when (agea  <= 44 ~ "26-44",
                          agea  >= 45 & agea  <= 64 ~ "45-64",
                          agea  >= 65 ~ "65plus",
                          is.na(agea) ~ NA_character_),
         immigrant = case_when(brncntr == 2  ~ "1st_Gen",
                               brncntr == 1 & (mocntr == 2 | facntr == 2)  ~ "2nd_Gen",
                               brncntr == 1 ~ "0_Native",
                               TRUE ~ NA_character_),
         location = case_when (domicil  < 4 ~ "0_Urban",
                               domicil  %in% c(4,5) ~ "1_Rural",
                               is.na(domicil) ~ NA_character_),
         employment = case_when (mnactic == 1 ~ "0_Employed",
                                 mnactic == 3 ~ "1_Unemployed",
                                 mnactic %in% c(2,4,5,6,7,8,9) ~ "2_Not_active",
                                 is.na(mnactic) ~ NA_character_),
         fincome = case_when(hincfel == 1 ~ "3_Living comfortably",
                             hincfel == 2 ~ "2_Coping",
                             hincfel == 3 ~ "1_Difficult",
                             hincfel == 4 ~ "0_Very_difficult",
                             is.na(hincfel) ~ NA_character_),
         worse = 10 - imwbcnt)#inverted scale. higher -> immigr. make cntry worse


# 3) selecting data for the final sample by: ####
# Selecting last 6 waves in which parental education is available
# Selecting only those aged 26 or older and citizens of the country in which the interview was conducted; and
# Droping cases with missing values for any of the variables included in the final analysis

ess_ccp <- 
  ess_mobil %>%
  filter(essround > 3 & agea > 25 & ctzcntr == 1) %>% 
  filter(!is.na(imwbcnt) & !is.na(eisced) & !eisced %in% c(0,55) & #outcome and respondents' education and
           ((!is.na(eiscedm) & !eiscedm %in% c(0,55)) | #mothers' education or
              (!is.na(eiscedf) & !eiscedf %in% c(0,55))) & #fathers' education and
           !is.na(agea) & !is.na(gndr) & !is.na(immigrant) & !is.na(domicil) & #socio-demographic and
           !is.na(mnactic) & !is.na(hincfel))#economic


# Saving data file with the final sample
save(ess_ccp, file = "./data/ess_ccp.RData")
