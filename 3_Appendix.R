### Code for Educational Mobility and Attitudes toward Immigrants: A Cross-national Comparison ####
## 3. Appendix ##

# To run this code, it is necessary to import the data and recode the variables as described in 1_PreparingData.R
# This code also makes use of tables generated in 2_Analysis.R

# This code explains how to generate the results reported in Appendix

#### Required packages ####

library(tidyverse)#data manipulation
library(gnm)#diagonal mobility models

# Function to fit models for each countryand generate a summary table

get_summary <- function(countryiso){
  df <- subset(ess_ccp, cntry == countryiso)
  model <- gnm(worse ~ Dref(p_education, r_education) + 
                 gender + agea + immigrant + 
                 location + employment + fincome + factor(essround), verbose = TRUE,
               constrain = "r_education1",
               family = gaussian, data = df, weights = dweight)
  
  summary <- coef(summary(model))[,c(1,2,4)]
  N <- sum(df$dweight)
  
  data.frame(summary) %>%
    mutate(variable = rownames(.),
           cntry = countryiso)
}

#two countries only have data for one round, this function drops the year control
get_summary_oneround <- function(countryiso){
  df <- subset(ess_ccp, cntry == countryiso)
  model <- gnm(worse ~ Dref(p_education, r_education) + 
                 gender + agea + immigrant + 
                 location + employment + fincome, verbose = TRUE,
               constrain = "r_education1",
               family = gaussian, data = df, weights = dweight)
  
  summary <- coef(summary(model))[,c(1,2,4)]
  
  data.frame(summary) %>%
    mutate(variable = rownames(.),
           cntry = countryiso)
}


s_AT <- get_summary("AT")
s_BE <- get_summary("BE")
s_BG <- get_summary("BG")
s_CH <- get_summary("CH")
s_CY <- get_summary("CY")
s_CZ <- get_summary("CZ")
s_DE <- get_summary("DE")
s_DK <- get_summary("DK")
s_EE <- get_summary("EE")
s_ES <- get_summary("ES")
s_FI <- get_summary("FI")
s_FR <- get_summary("FR")
s_GB <- get_summary("GB")
s_GR <- get_summary_oneround("GR")
s_HR <- get_summary("HR")
s_HU <- get_summary("HU")
s_IE <- get_summary("IE")
s_IL <- get_summary("IL")
s_IS <- get_summary("IS")
s_IT <- get_summary("IT")
s_LT <- get_summary("LT")
s_NL <- get_summary("NL")
s_NO <- get_summary("NO")
s_PL <- get_summary("PL")
s_PT <- get_summary("PT")
s_RS <- get_summary_oneround("RS")
s_RU <- get_summary("RU")
s_SE <- get_summary("SE")
s_SI <- get_summary("SI")
s_SK <- get_summary("SK")
s_UA <- get_summary("UA")

Annex1 <- 
  bind_rows(s_AT, s_BE, s_BG, s_CH, s_CY, s_CZ, s_DE,
            s_DK, s_EE, s_ES, s_FI, s_FR, s_GB,
            s_GR, s_HR, s_HU, s_IE, s_IL, s_IS,
            s_IT, s_LT, s_NL, s_NO, s_PL, s_PT,
            s_RS, s_RU, s_SE, s_SI, s_SK, s_UA)

Observations <- 
  ess_ccp %>% 
  group_by(cntry) %>% 
  summarise(variable = "Observations",
            Estimate = round(sum(dweight),0))

Annex1 <- bind_rows(Annex1, Observations)

Annex1_wt <- 
  country_weights_table %>% #This table was generate in 2_Analysis.R
  mutate(Parent = paste0(round(p_education, 2), " (", round(p_se, 2), ")"),
         Respondent = paste0(round(r_education, 2), " (", round(r_se, 2),")")) %>% 
  rename(cntry = country) %>% 
  select(cntry, Parent, Respondent) %>% 
  pivot_longer(cols = c(Parent, Respondent),
               names_to = "variable",
               values_to = "coeff")


Annex1 %>% 
  mutate(coeff = paste0(round(Estimate, 3), " ",
                        paste0("(", round(Std..Error, 2), ")"), " ",
                        case_when(Pr...t.. < 0.001 ~ "***",
                                  Pr...t.. >= 0.001 & Pr...t.. < 0.01 ~ "**",
                                  Pr...t.. >= 0.01 & Pr...t.. < 0.05 ~ "*",
                                  TRUE ~ "" ))) %>% 
  select(cntry, variable, coeff) %>%
  bind_rows(Annex1_wt) %>%
  pivot_wider(names_from = cntry, values_from = coeff) %>%
  write.table(., "clipboard", sep = "\t", row.names = F)
