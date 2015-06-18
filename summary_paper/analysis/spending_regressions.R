# ---------------------------------------------------------------------------- #
# Elections and Government Liabilities
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
setwd('/git_repositories/EIUCrisesMeasure/summary_paper/analysis')

# Load plot function
source_gist('d270ff55c2ca26286e90')

comb <- import('covariate_data/epfms_covariates.csv')

# Convert key varaiables to factors
comb$iso2c <- comb$iso2c %>% as.factor
comb$election_year <- comb$election_year %>% as.factor

# ------------------------------- Explore ------------------------------------ #
#### Correlate
cor.test(comb$mean_stress, comb$output_gap)
cor.test(comb$mean_stress, comb$gov_liabilities_gdp2005)
cor.test(comb$mean_stress, comb$gov_spend_gdp2005)

# Plot
ggplot(comb, aes(mean_stress, output_gap)) +
    geom_point() +
    stat_smooth(se = F, colour = '#ff4c4c') +
    theme_bw()

ggplot(comb, aes(mean_stress, gov_spend_gdp2005_change)) +
    geom_point() +
    stat_smooth(se = F, colour = '#ff4c4c') +
    theme_bw()

# ------------------------------ Create residuals ---------------------------- #
#### Create residuals ####
comb$output_change <- comb$output_gap_1 - comb$output_gap

m_r1 <- lm(gov_liabilities_gdp2005 ~ gov_liabilities_gdp2005_1 + 
               output_gap + output_gap_1 + iso2c,
           data = comb)
gov_output_residuals <- residuals(m_r1)
sub_gov_liab <- comb %>% DropNA('gov_liabilities_gdp2005_1')
sub_gov_liab$residuals_output <- gov_output_residuals

m_r2 <- lm(residuals_output ~ mean_stress_1 + iso2c, data = sub_gov_liab)
sub_gov_liab$residuals_stress <- residuals(m_r2)

# ----------------------------- Election Year (t0) --------------------------- #
#### Estimate effect on output residuals ####
m1_o <- lm(residuals_output ~ election_year + iso2c, data = sub_gov_liab)

m2_o <- lm(residuals_output ~ election_year*lpr_1 + iso2c, data = sub_gov_liab)

m3_o <- lm(residuals_output ~ election_year*lpr_1 + execrlc + polconiii + iso2c,
         data = sub_gov_liab)

# Plot results
plot_me(obj = m2_o, term1 = 'election_year1', term2 = 'lpr_1',
        fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 10)) +
    xlab('\nElectoral Loss Probability (lag 1)') +
    ylab('Marginal Effect of Election Year\n') +
    ggtitle('DV: Liabilities Above Output Gap Prediction\n')

#### Estimate effect on stress residuals ####
m1_s <- lm(residuals_stress ~ election_year + iso2c, data = sub_gov_liab)

m2_s <- lm(residuals_stress ~ election_year*lpr_1 + iso2c, data = sub_gov_liab)

m3_s <- lm(residuals_stress ~ election_year*lpr_1 + execrlc + polconiii + iso2c,
           data = sub_gov_liab)

# Plot results
plot_me(obj = m2_s, term1 = 'election_year1', term2 = 'lpr_1',
        fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 10)) +
    xlab('\nElectoral Loss Probability (lag 1)') +
    ylab('Marginal Effect of Election Year\n') +
    ggtitle('DV: Liabilities Above Output Gap and EPFMS Predictions\n')

#------------------------------- Post-election Year (t1) ----------------------#
#### Output Gap ####
m1_o <- lm(residuals_output ~ election_year_1 + iso2c, data = sub_gov_liab)

m2_o <- lm(residuals_output ~ election_year_1*lpr + iso2c, data = sub_gov_liab)

m3_o <- lm(residuals_output ~ election_year*lpr + execrlc + polconiii + iso2c,
           data = sub_gov_liab)

# Plot results
plot_me(obj = m2_o, term1 = 'election_year_1', term2 = 'lpr',
        fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 10)) +
    xlab('\nElectoral Loss Probability') +
    ylab('Marginal Effect of Post-Election Year\n') +
    ggtitle('DV: Liabilities Above Output Gap Prediction\n')

#### Financial Stress #### 
m1_s <- lm(residuals_stress ~ election_year_1 + iso2c, data = sub_gov_liab)

m2_s <- lm(residuals_stress ~ election_year_1*lpr + iso2c, data = sub_gov_liab)

# Remove extreme outlier
m2_1_s <- lm(residuals_stress ~ election_year_1*lpr + iso2c, 
             data = subset(sub_gov_liab, country != 'Iceland'))

m3_s <- lm(residuals_stress ~ election_year_1*lpr + execrlc + polconiii + iso2c,
           data = sub_gov_liab)

# Plot results
plot_me(obj = m2_s, term1 = 'election_year_1', term2 = 'lpr',
        fitted2 = seq(0, 0.75, by = 0.05)) +
    scale_y_continuous(limits = c(-10, 10)) +
    xlab('\nElectoral Loss Probability') +
    ylab('Marginal Effect of Post-Election Year\n') +
    ggtitle('DV: Liabilities Above Output Gap and EPFMS Predictions\n')

# ------------------------- Robustness Compare to Econ Spending -------------- #
#### Create Total Spending Residuals ####
m_r1_spend <- lm(gov_spend_gdp2005 ~ gov_spend_gdp2005_1 + 
                     output_gap + output_gap_1 + iso2c,
             data = comb)
gov_spend_residuals <- residuals(m_r1_spend)
sub_gov_liab_spend <- comb %>% DropNA(c('gov_spend', 'gov_spend_1', 
                                        'output_gap', 'mean_stress_1'))
sub_gov_liab_spend$residuals_output <- gov_spend_residuals

m_r2_spend <- lm(residuals_output ~ mean_stress_1 + iso2c, 
             data = sub_gov_liab_spend)
sub_gov_liab_spend$residuals_stress <- residuals(m_r2_spend)

m2_spend <- lm(residuals_stress ~ election_year*lpr_1 + iso2c, 
           data = sub_gov_liab_spend)

#### Create Economic Affairs Spending Residuals ####
m_r1_econ <- lm(gov_econ_spend_gdp2005 ~ gov_econ_spend_gdp2005_1 + 
                 output_gap + output_gap_1 + iso2c,
             data = comb)

gov_spend_residuals <- residuals(m_r1_econ)
sub_gov_liab_spend <- comb %>% DropNA(c('gov_econ_spend', 'gov_econ_spend_1', 
                                        'output_gap', 'mean_stress_1'))
sub_gov_liab_spend$residuals_output <- gov_spend_residuals

m_r2_econ <- lm(residuals_output ~ mean_stress_1 + iso2c, 
                 data = sub_gov_liab_spend)
sub_gov_liab_spend$residuals_stress <- residuals(m_r2_spend)

m2_econ <- lm(residuals_stress ~ election_year*lpr_1 + iso2c, 
               data = sub_gov_liab_spend)

