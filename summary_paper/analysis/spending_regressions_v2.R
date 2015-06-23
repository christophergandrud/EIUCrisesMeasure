# ---------------------------------------------------------------------------- #
# Elections, Electoral Loss Probabilities, and Government Liabilities Shocks
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(devtools)
library(rio)
library(dplyr)
library(lubridate)
library(DataCombine)
library(ggplot2)

# Set working directory. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

# Load plot function
source_gist('d270ff55c2ca26286e90')

comb <- import('analysis/covariate_data/epfms_covariates.csv')

# Convert key varaiables to factors
comb$iso2c <- comb$iso2c %>% as.factor
comb$election_year <- comb$election_year %>% as.factor
comb$election_year_1 <- comb$election_year_1 %>% as.factor

##### Create residuals #####
# Create output gap change and government liabilities change variables
comb$output_change <- comb$output_gap - comb$output_gap_1


# Output Gap Residuals
m_r1 <- lm(gov_liabilities_gdp2005 ~ gov_liabilities_gdp2005_1 + output_gap + 
               iso2c, data = comb)

sub_gov_liab <- comb %>% DropNA(c('gov_liabilities_gdp2005_1', 'output_gap'))
sub_gov_liab$residuals_output <- residuals(m_r1)

# Financial Stress Residuals
m_r2 <- lm(residuals_output ~ mean_stress + iso2c, data = sub_gov_liab)
sub_gov_liab <- sub_gov_liab %>% DropNA(c('mean_stress'))
sub_gov_liab$residuals_stress <- residuals(m_r2)

sub_gov_liab <- slide(sub_gov_liab, Var = 'residuals_stress', 
                      NewVar = 'residuals_stress_1',
                      GroupVar = 'country', TimeVar = 'year')

sub_gov_liab$rs_change <- sub_gov_liab$residuals_stress -
                            sub_gov_liab$residuals_stress_1 

#### Regressions on Stress Residuals ####
m1_s <- lm(residuals_stress ~ election_year_1 + iso2c, data = sub_gov_liab)

m2_s <- lm(residuals_stress ~ election_year_1*lpr + iso2c, data = sub_gov_liab)
m2.1_s <- lm(residuals_stress ~ election_year_1*lpr + iso2c, 
             data = subset(sub_gov_liab, country != 'Iceland'))


m3_s <- lm(residuals_stress ~ election_year_1*lpr + execrlc + polconiii + iso2c, 
         data = sub_gov_liab)

#### Regression on Change in the Stress Residuals ####
m1_sc <- lm(rs_change ~ election_year_1 + iso2c, data = sub_gov_liab)

m2_sc <- lm(rs_change ~ election_year_1*lpr + iso2c, data = sub_gov_liab)

m3_sc <- lm(rs_change ~ election_year_1*lpr + execrlc + polconiii + iso2c, 
         data = sub_gov_liab)


# ------------------------- Robustness Compare to Econ Spending -------------- #
#### Create Total Spending Residuals ####
comb$gov_spend_change <- comb$gov_spend_gdp2005 - comb$gov_spend_gdp2005_1
m_r1_spend <- lm(gov_spend_change ~ output_gap + iso2c,
                 data = comb)
gov_spend_residuals <- residuals(m_r1_spend)
sub_gov_liab_spend <- comb %>% DropNA(c('gov_spend_change'))
sub_gov_liab_spend$residuals_output <- gov_spend_residuals

m_r2_spend <- lm(residuals_output ~ mean_stress + iso2c, 
                 data = sub_gov_liab_spend)
sub_gov_liab_spend <- sub_gov_liab_spend %>% DropNA(c('mean_stress'))
sub_gov_liab_spend$residuals_stress <- residuals(m_r2_spend)

