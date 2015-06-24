# ---------------------------------------------------------------------------- #
# Elections, Electoral Loss Probabilities, and Government Liabilities Shocks
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
library(DataCombine)
library(ggplot2)

# Set working directory. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/')

comb <- import('analysis/covariate_data/epfms_covariates.csv')

# Convert key varaiables to factors
comb$iso2c <- comb$iso2c %>% as.factor
comb$election_year <- comb$election_year %>% as.factor
comb$election_year_1 <- comb$election_year_1 %>% as.factor

##### Create residuals #####
# Create output gap change and government liabilities change variables

# Output Gap Residuals
m_r1 <- lm(gov_liabilities_gdp2005 ~ gov_liabilities_gdp2005_1 + output_gap + 
               iso2c, data = comb)

sub_gov_liab <- comb %>% DropNA(c('gov_liabilities_gdp2005_1', 'output_gap'))
sub_gov_liab$residuals_output_liab <- residuals(m_r1)
sub_gov_liab <- slide(sub_gov_liab, Var = 'residuals_output_liab', 
                            NewVar = 'residuals_output_liab_1',
                            GroupVar = 'country', TimeVar = 'year')

# Financial Stress Residuals
m_r2 <- lm(residuals_output_liab ~ residuals_output_liab_1 + mean_stress + iso2c, data = sub_gov_liab)
sub_gov_liab <- sub_gov_liab %>% DropNA(c('mean_stress'))
sub_gov_liab$residuals_stress_liab <- residuals(m_r2)

sub_gov_liab <- slide(sub_gov_liab, Var = 'residuals_output_liab', 
                      NewVar = 'residuals_stress_liab_1',
                      GroupVar = 'country', TimeVar = 'year')

sub_gov_liab$rs_change_liab <- sub_gov_liab$residuals_stress_liab -
                            sub_gov_liab$residuals_stress_liab_1 

# ------------------------- Econ Spending Residuals -------------- #
#### Create Total Spending Residuals ####
m_r1_econ <- lm(gov_econ_spend_gdp2005 ~ gov_econ_spend_gdp2005_1 + output_gap + iso2c,
                data = comb)

sub_gov_liab_spend <- comb %>% DropNA(c('gov_econ_spend_gdp2005', 
                                        'gov_econ_spend_gdp2005_1',
                                        'output_gap'))
sub_gov_liab_spend$residuals_output_spend <- residuals(m_r1_econ)
sub_gov_liab_spend <- slide(sub_gov_liab_spend, Var = 'residuals_output_spend', 
                      NewVar = 'residuals_output_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

m_r2_econ <- lm(residuals_output_spend ~ residuals_output_spend_1 + 
                    mean_stress + iso2c, 
                data = sub_gov_liab_spend)
sub_gov_liab_spend <- sub_gov_liab_spend %>% 
                        DropNA(c('residuals_output_spend_1', 'mean_stress'))
sub_gov_liab_spend$residuals_stress_spend <- residuals(m_r2_econ)

sub_gov_liab_spend <- slide(sub_gov_liab_spend, Var = 'residuals_stress_spend', 
                      NewVar = 'residuals_stress_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

sub_gov_liab_spend$rs_change_spend <- sub_gov_liab_spend$residuals_stress_spend -
                                    sub_gov_liab_spend$residuals_stress_spend_1 

# ------------------------------- Regressions -------------------------------- #
#### Election Year #### 
# Spending
m1_t0 <- lm(rs_change_spend ~ election_year + iso2c, data = sub_gov_liab_spend)

m2_t0 <- lm(rs_change_spend ~ election_year + lpr_1 + iso2c, data = sub_gov_liab_spend)

m3_t0 <- lm(rs_change_spend ~ election_year*lpr_1 + iso2c, 
                  data = sub_gov_liab_spend)

m4_t0 <- lm(rs_change_spend ~ election_year*lpr_1 + execrlc + polconiii + 
                fixed_exchange + iso2c, data = sub_gov_liab_spend)

# Liabilities
m5_t0 <- lm(rs_change_liab ~ election_year + lpr_1 + execrlc + polconiii + 
                fixed_exchange + iso2c, data = sub_gov_liab)

#### Post-Election Year ####
# Liabilities
m1_t1 <- lm(rs_change_liab ~ election_year_1 + iso2c, data = sub_gov_liab)

m2_t1 <- lm(rs_change_liab ~ election_year_1 + lpr + iso2c, data = sub_gov_liab)

m3_t1 <- lm(rs_change_liab ~ election_year_1*lpr + iso2c, data = sub_gov_liab)

m4_t1 <- lm(rs_change_liab ~ election_year_1*lpr + execrlc + polconiii +
                fixed_exchange + iso2c, 
            data = sub_gov_liab)

# Spending
m5_t1 <- lm(rs_change_spend ~ election_year_1 + lpr + execrlc + polconiii + 
                fixed_exchange + iso2c, data = sub_gov_liab_spend)
