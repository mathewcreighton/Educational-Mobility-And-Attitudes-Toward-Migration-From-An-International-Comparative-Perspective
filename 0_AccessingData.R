### Code for Educational Mobility and Attitudes toward Immigrants: A Cross-national Comparison ###

## 0. Access data ##
# This code explains how to access the ESS data files
# If you do not want to change anything in the analysis, you can skip this step and load the 
# working dataset that is available at the replication package running the code 1_PreparingData.R

#### Required packages ####

library(tidyverse)#data manipulation
library(haven)#load SPSS dataset and metadata

#### 1) Accessing data ##########

# This research is based on data from the European Social Survey (ESS)
# The ESS dataset is publicly available for free at their website.
# Nine rounds of data collections were conducted until 2020. 

# 1a) To run this code, you need to download the following data files in SPSS format: 

# The cumulative dataset with rouns 1 to 8 is available at https://www.europeansocialsurvey.org/downloadwizard/
# The dataset for the latest round (9) is available here: https://www.europeansocialsurvey.org/data/round-index.html

# 1b) After downloading both files, you should save them at the sub-folder "data"

#### 2) Importing data #########

# ESS data is available in SPSS, Stata and SAS format
# We conducted this analysis using R
# These are the steps to import the SPSS data files into R


# 2a) Loading original dataset

# Importing the cumulative SPSS data file (.sav) with all rounds (1-8) 
ess_cumulative <- read_sav("./data/ESS1-8e01.sav")

# 2b) Selecting relevant variables
# In this step, we select the variables used in the analysis

ess_cumulative <- 
  ess_cumulative %>%
  select(idno, cntry, essround, inwyr, inwyys, inwmm, inwmms, #id and date of interview
       pspwght, pweight, dweight,#weight
       brncntr, facntr, mocntr, ctzcntr, blgetmg,  #nationality/citizenship
       imdfetn, imsmetn, impcntr, imbgeco, imueclt, imwbcnt, #immigration
       eimpcnt, imbleco, imdetbs, imdetmr, imtcjob, imwbcrm, #immigration
       eduyrs, eisced, eiscedp, eiscedf, eiscedm, #education levels (ISCED) and educ years
       edulvla, edulvlb, edulvlma, edulvlmb, edulvlfa, edulvlfb, #educ level harmonised
       yrbrn, agea, gndr, domicil, #demographics
       uempla, uempli, mainact, mnactic, wrkac6m, uemp3m, uemp12m, #occupation
       hinctnt, hinctnta, hincfel)#household income

## 2c) Merging with ESS 9 dataset
# In this step, we import the Round 9 data file and select the relevant variables

ess9 <- read_sav("./data/ESS9e01_1.sav")
ess9 <- ess9 %>%
select(idno, cntry, essround, inwyye, inwmme, #id and date of interview
       pweight, dweight,#weight
       brncntr, facntr, mocntr, ctzcntr, blgetmg,  #nationality/citizenship
       imdfetn, imsmetn, impcntr, imbgeco, imueclt, imwbcnt, #immigration
       eduyrs, eisced, eiscedp, eiscedf, eiscedm, #education levels (ISCED) and educ years
       edulvlb, edulvlmb, edulvlfb, #educ level harmonised
       yrbrn, agea, gndr, domicil, #demographics
       uempla, uempli, mainact, mnactic, wrkac6m, uemp3m, uemp12m, #occupation
       hinctnta, hincfel)#household income

# The current version of ESS9 does not have post-stratification weights calculated
# For ESS9 data, We use design weights as post-stratification weights
ess9$pspwght <- ess9$dweight


# 3) Binding both datasets:
ess_mobil <- bind_rows(ess_cumulative, ess9)

# 4) saving working dataset
# This file is already pre-saved in the replication package
save(ess_mobil, file = "./data/ess_mobil.RData")
