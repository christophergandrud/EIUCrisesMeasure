# ------------------------------------------------------------------------------
# Regression model comparisions LV vs FinStress
# Christopher Gandrud
# MIT License
# ------------------------------------------------------------------------------

# Load required packages
library(simpleSetup)

pkgs <- c('rio', 'repmis', 'tidyverse', 'stargazer')
library_install(pkgs)

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
simpleSetup::set_valid_wd(possible_dir)


# Source Electoral Volatility regressions --------------------------------------
source('summary_paper/analysis/elect_volatility_crisis.R')


# Gandrud and Hallerberg Eurostat Revisions ------------------------------------
# Function to reverse the direction of the election timing variable
reverser <- function(x) max(x, na.rm = T) - x

# Import and final clean --------------
comb <- import('https://raw.githubusercontent.com/christophergandrud/eurostat_revisions/master/eurostat_replication_material/data_cleaning/main_merged.csv')

comb$endog_3 <- factor(comb$endog_3,
                       levels = c(1:3),
                       labels = c('Unscheduled', 'Scheduled', 'No election'))

comb$endog_3 <- relevel(comb$endog_3, ref = 'No election')

comb$from_2010 <- 0
comb$from_2010[comb$year >= 2010] <- 1

comb$yrcurnt_corrected <- reverser(comb$yrcurnt_corrected)

# Put FinStress on a more easily interpretable scale
comb$finstress_mean <- comb$finstress_mean * 100

## Estimate models
# Debt revisions ---------
debt <- comb %>% filter(component == 'debt')
FindDups(debt, c('country', 'year', 'version'))

# Keep only the final cumulative revision, if in the 3rd year from the original
# figure was published
debt <- debt[!duplicated(debt[, c('country', 'year')], fromLast = TRUE), ]
debt <- subset(debt, years_since_original == 3)

debt <- dplyr::rename(debt, FinStress = finstress_mean)
debt <- dplyr::rename(debt, lv_crisis1 = lv_crisis)


# Run comparable regressions
m1_7 <- lm(cum_revision ~
               FinStress * endog_3 +
               as.factor(country),
           data = debt)

m1_7_lv <- lm(cum_revision ~
               lv_crisis1 * endog_3 +
               as.factor(country),
           data = debt)

## Create a combined table -----------------------------------------------------
covar_labs <- c('FinStress', 'FinStress$\\^2$', 'Laeven/Valencia Crisis',
                'Unscheduled Elec.', 'Scheduled Elec.', 'FinStress * Unscheduled',
                'FinStress * Scheduled', 'LV * Unscheduled', 'LV * Scheduled',
                'Constant')

stargazer(m1_fs_poly, m1_lv, m1_7, m1_7_lv, 
          omit = 'as.factor*',
          add.lines = list(c('Fixed Effects?', 'No', 'No', 'Yes', 'Yes')),
          dep.var.labels = c('Electoral Volatility', 'Cum. Debt Revisions'),
          covariate.labels = covar_labs,
          type = 'latex',
          font.size = 'tiny',
          title = " Comparing FinStress and LV Binary Measure Model Fit",
          label = 'finlvregcompare',
          out = 'summary_paper/tables/finstress_lv_regression_compare.tex')
