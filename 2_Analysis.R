### Code for Educational Mobility and Attitudes toward Immigrants: A Cross-national Comparison ####
## 2. Analysis ##

# To run this code, it is necessary to import the data and recode the variables as described in 1_PreparingData.R

# This code explains:
# 1) How to generate the results reported in Table 1 
# 2) How to generate the results reported in Table 2
# 3) How to generate the results reported in Table 3
# 4) How to generate the diagonal values required for Table 4
# 5) How to generate the weights required for Table 4


#### Required packages

library(tidyverse)#data manipulation
library(haven)#load SPSS dataset and metadata
library(gnm)#diagonal mobility models
library(Hmisc)#for weighted standard deviation

# 0) Load data file with the final sample 
load("./data/ess_ccp.RData")

# 1a) Table 1 - Outcome variable by country####
# Getting mean value for the outcome by country and mobility group

Table1_means_mobility <- 
  ess_ccp %>% 
  group_by(cntry, diagonal) %>% 
  summarise(worse_mean = round(weighted.mean(worse, w = dweight, na.rm = T),2),
            SD = round(sqrt(wtd.var(worse, weights = dweight)), 1)) %>%
  pivot_wider(id_cols = cntry,
              names_from = diagonal,
              values_from = c(worse_mean, SD))

#Getting the mean value and N by country
Table1 <- ess_ccp %>% 
  group_by(cntry) %>% 
  summarise(Mean_overall = round(weighted.mean(worse, w = dweight, na.rm = T),2),
            SD_overall = round(sqrt(wtd.var(worse, weights = dweight)), 1),
            Observations = sum(dweight)) %>% 
  left_join(Table1_means_mobility, by = "cntry") 

#Adjusting column order and exporting to clipboard to format spreadsheet
Table1 <- 
  Table1 %>%
  mutate(country = countrycode(cntry, "iso2c", "country.name")) %>% 
  select(13, 5, 9, 6, 10, 7, 11, 2, 3, 4)

write.table(Table1, "clipboard", sep = "\t", row.names = FALSE) #for windows

#write_clip(Table1) #for linux

rm(Table1_means_mobility)

# Getting values for Total row

ess_ccp %>% 
  group_by(diagonal) %>% 
  summarise(worse_mean = paste0(round(weighted.mean(worse, w = fweight, na.rm = T),2), 
                                " (", round(sqrt(wtd.var(worse, weights = fweight)), 1), ")")) %>%
  spread("diagonal", "worse_mean") 


ess_ccp %>% 
  summarise(`Mean (sd)` = paste0(round(weighted.mean(worse, w = fweight, na.rm = T),2), 
                                 " (", round(sqrt(wtd.var(worse, weights = fweight)), 1), ")"),
            Observations = round(sum(fweight)))


# 2) Table 2 - Proportion, overall mean and mean by mobility type for each ind_var####

#Total number of observations
sum(ess_ccp$fweight, na.rm = T)

#Function to generate descriptives for each variables
t2_row <- function (dataf, x) {
  dataf %>%
    group_by_(x) %>% 
    summarise(mean = round(weighted.mean(worse, w = fweight, na.rm = T),2),
              sd = round(sqrt(wtd.var(worse, weights = fweight)), 1),
              prop_sample = round(sum(fweight)/197362*100,1),
              downward = round(sum(downward*fweight)/sum(fweight)*100,1),
              stable = round(sum(stable*fweight)/sum(fweight)*100,1),
              upward = round(sum(upward*fweight)/sum(fweight)*100,1),
              observations = round(sum(fweight)))
  
}

# Generating descriptive tables for each variable
t2_gender <- t2_row(ess_ccp, "gender") %>% rename(variable = gender)
t2_age <- t2_row(ess_ccp, "age") %>% rename(variable = age)
t2_immig <- t2_row(ess_ccp, "immigrant") %>% rename(variable = immigrant)
t2_location <- t2_row(ess_ccp, "location") %>% rename(variable = location)
t2_employment <- t2_row(ess_ccp, "employment") %>% rename(variable = employment)
t2_fincome <- t2_row(ess_ccp, "fincome") %>% rename(variable = fincome)

# Collating results for each variable into one table
Table2 <- bind_rows(t2_gender, t2_age, t2_immig, t2_location, t2_employment, t2_fincome)
rm(t2_gender, t2_age, t2_immig, t2_location, t2_employment, t2_fincome)

write.table(Table2, "clipboard", sep = "\t", row.names = FALSE)
#write_clip(Table2)

#### 3) Table 3 - Models ####

### m_empty: Empty model with country and round effects

m_empty <- gnm(worse ~ -1 + Dref(p_education, r_education) + 
                 factor(essround) + cntry, 
               weights = fweight, family = gaussian, 
               constrain = "r_education1", data = ess_ccp)

#Coefficients, AIC and Weights

pickCoef(m_empty, "Dref(., .)", fixed = TRUE, value = TRUE) 
DrefWeights(m_empty)
AIC(m_empty)

### m_socio: Empty model + Gender, Immigrant Status, Age and Urban/Rural

m_socio <- gnm(worse ~ -1 + Dref(p_education, r_education) + 
                 factor(essround) + cntry +
                 gender + immigrant + agea + location,
               weights = fweight, family = gaussian, 
               constrain = "r_education1", data = ess_ccp)

#Coefficients, AIC and Weights

summary(m_socio)
DrefWeights(m_socio)
AIC(m_socio)

### m_ecosocio: Socio model + perception of income and employment status

m_ecosocio <- gnm(worse ~ -1 + Dref(p_education, r_education) + 
                    factor(essround) + cntry +
                    gender + immigrant + agea + location +
                    fincome + employment,
                  weights = fweight, family = gaussian, 
                  constrain = "r_education1", data = ess_ccp)

summary(m_ecosocio)
DrefWeights(m_ecosocio)
AIC(m_ecosocio)

# 4) Table 4 - Getting diagonal values for each country ####


get_diagonal <- function(countryiso){
  df <- subset(ess_ccp, cntry == countryiso)
  model <- gnm(worse ~ Dref(p_education, r_education) + 
                 factor(essround) + gender + immigrant + agea + 
                 location + fincome + employment, verbose = TRUE,
               constrain = "r_education1",
               family = gaussian, data = df, weights = dweight)
  
  intercept <- pickCoef(model, "(Intercept)", fixed = TRUE, value = TRUE)
  coeff <- pickCoef(model, "Dref(., .)", fixed = TRUE, value = TRUE)
  
  data.frame(c(intercept, coeff)) %>% 
    mutate(cell = rownames(.),
           cntry = countryiso)
}

#two countries only have data for one round, this function drops the year control
get_diagonal_oneround <- function(countryiso){
  df <- subset(ess_ccp, cntry == countryiso)
  model <- gnm(worse ~ Dref(p_education, r_education) + 
                 gender + immigrant + agea + 
                 location + fincome + employment, verbose = TRUE,
               constrain = "r_education1",
               family = gaussian, data = df, weights = dweight)
  
  intercept <- pickCoef(model, "(Intercept)", fixed = TRUE, value = TRUE)
  coeff <- pickCoef(model, "Dref(., .)", fixed = TRUE, value = TRUE)
  
  data.frame(c(intercept, coeff)) %>% 
    mutate(cell = rownames(.),
           cntry = countryiso)
}


d_AT <- get_diagonal("AT")
d_BE <- get_diagonal("BE")
d_BG <- get_diagonal("BG")
d_CH <- get_diagonal("CH")
d_CY <- get_diagonal("CY")
d_CZ <- get_diagonal("CZ")
d_DE <- get_diagonal("DE")
d_DK <- get_diagonal("DK")
d_EE <- get_diagonal("EE")
d_ES <- get_diagonal("ES")
d_FI <- get_diagonal("FI")
d_FR <- get_diagonal("FR")
d_GB <- get_diagonal("GB")
d_GR <- get_diagonal_oneround("GR")
d_HR <- get_diagonal("HR")
d_HU <- get_diagonal("HU")
d_IE <- get_diagonal("IE")
d_IL <- get_diagonal("IL")
d_IS <- get_diagonal("IS")
d_IT <- get_diagonal("IT")
d_LT <- get_diagonal("LT")
d_NL <- get_diagonal("NL")
d_NO <- get_diagonal("NO")
d_PL <- get_diagonal("PL")
d_PT <- get_diagonal("PT")
d_RS <- get_diagonal_oneround("RS")
d_RU <- get_diagonal("RU")
d_SE <- get_diagonal("SE")
d_SI <- get_diagonal("SI")
d_SK <- get_diagonal("SK")
d_UA <- get_diagonal("UA")

Table4_diagonal <- 
  bind_rows(d_AT, d_BE, d_BG, d_CH, d_CY, d_CZ, d_DE,
            d_DK, d_EE, d_ES, d_FI, d_FR, d_GB,
            d_GR, d_HR, d_HU, d_IE, d_IL, d_IS,
            d_IT, d_LT, d_NL, d_NO, d_PL, d_PT,
            d_RS, d_RU, d_SE, d_SI, d_SK, d_UA)

Table4_diagonal <- 
  Table4_diagonal %>%
  filter(cell != "Dref(., .).p_education|r_education1	") %>%
  pivot_wider(id_cols = cntry, names_from = cell, 
              values_from = c.intercept..coeff.) %>%
  rename(cell_11 = `(Intercept)`) %>% 
  mutate(cell_22 = cell_11 + `Dref(., .).p_education|r_education2`,
         cell_33 = cell_11 + `Dref(., .).p_education|r_education3`) %>%
  select(cntry, cell_11, cell_22, cell_33)

write.table(Table4_diagonal, "clipboard", sep = "\t", row.names = FALSE)

# 5) Table 4 - Getting weights for each country ####

full_model <- as.formula(worse ~ Dref(p_education, r_education) + 
                           factor(essround) + gender + immigrant + agea + 
                           location + fincome + employment)

wt_AT <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "AT"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "AT")

wt_BE <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "BE"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "BE")

wt_BG <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "BG"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "BG")


wt_CH <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "CH"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "CH")

wt_CY <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "CY"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "CY")

wt_CZ <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "CZ"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "CZ")

wt_DE <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "DE"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "DE")

wt_DK <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "DK"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "DK")


wt_EE <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "EE"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "EE")

wt_ES <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "ES"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "ES")

wt_FI <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "FI"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "FI")

wt_FR <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "FR"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "FR")

wt_GB <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "GB"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "GB")

wt_GR <- DrefWeights(gnm(worse ~ Dref(p_education, r_education) + 
                           gender + immigrant + agea + 
                           location + fincome + employment, 
                         weights = dweight, data = subset(ess_ccp, cntry == "GR"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "GR")

wt_HR <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "HR"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "HR")

wt_HU <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "HU"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "HU")

wt_IE <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "IE"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "IE")

wt_IL <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "IL"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "IL")

wt_IS <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "IS"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "IS")

wt_IT <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "IT"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "IT")

wt_LT <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "LT"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "LT")

wt_NL <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "NL"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "NL")

wt_NO <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "NO"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "NO")

wt_PL <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "PL"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "PL")

wt_PT <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "PT"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "PT")

wt_RS <- DrefWeights(gnm(worse ~ Dref(p_education, r_education) + 
                           gender + immigrant + agea + 
                           location + fincome + employment, 
                         weights = dweight, data = subset(ess_ccp, cntry == "RS"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "RS")

wt_RU <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "RU"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "RU")

wt_SE <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "SE"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "SE")


wt_SI <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "SI"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "SI")

wt_SK <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "SK"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "SK")

wt_UA <- DrefWeights(gnm(full_model, weights = dweight, data = subset(ess_ccp, cntry == "UA"))) %>% 
  data.frame() %>%  mutate(coeff = rownames(.), country = "UA")

country_weights <- bind_rows(wt_AT, wt_BE, wt_BG, wt_CH, wt_CY, wt_CZ, wt_DE, wt_DK,
                             wt_EE, wt_ES, wt_FI, wt_FR, wt_GB, wt_GR, wt_HR,
                             wt_HU, wt_IE, wt_IL, wt_IS, wt_IT, wt_LT,
                             wt_NL, wt_NO, wt_PL, wt_PT, wt_RS, wt_RU,
                             wt_SE, wt_SI, wt_SK, wt_UA)

country_wt_se <- 
  country_weights %>% 
  filter(coeff == "se") %>% 
  rename(p_se = p_education, r_se = r_education) %>% 
  select(-coeff)

country_weights_table <- 
  country_weights %>% 
  filter(coeff == "weight") %>% 
  left_join(country_wt_se, by = "country") %>%
  select(country, p_education, p_se, r_education, r_se)

write.table(country_weights_table, "clipboard", sep = "\t", row.names = F)

# Final values for Table 4 were calculated in the Excel file
