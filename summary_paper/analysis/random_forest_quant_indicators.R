# ---------------------------------------------------------------------------- #
# Random Forest Regressions with Andrianova et al. (2015) Financial Fragility 
# Indicators and GFDD indicators
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(repmis)
library(randomForestSRC)
library(dplyr)
library(ggplot2)

# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)

comb <- import('data/alternative_measures/cleaned/fin_frag_gfdd.csv')

# Variables to include as predictors
comp_vars <- c('Equity', 'log_imploans', 'Costs', 'ROAA', 'NetLoans', 'Liquid',
               'NCO', 'stock_price_volatility', 'stock_returns', 
               'private_credit_log', 'capital_asset_ratio', 
               'credit_deposits_ratio', 'reg_capital_assets', 
               'provisions_to_npls')

comb_no_na_all <- comb %>% DropNA(c('mean_stress', comp_vars))

comb_no_na_oecd <- comb_no_na_all %>% filter(income == 'High income: OECD')


form_all_vars <- paste('mean_stress ~', paste(comp_vars, collapse = ' + ')) %>%
                       as.formula

rf_1 <- rfsrc(form_all_vars, data = comb_no_na_all)
rf_2 <- rfsrc(form_all_vars, data = comb_no_na_oecd)

plot.rfsrc(rf_1, plots.one.page = F)
plot.rfsrc(rf_2, plots.one.page = F)

# Save results to a data frame for cleaner plotting
extract_importance <- function(x){
    imp <- x %>% as.data.frame
    imp$stem <- row.names(imp)
    names(imp) <- c('variable_importance', 'variable')
    imp <- imp %>% select(variable, variable_importance)
    return(imp)
}

imp_1 <- extract_importance(rf_1$importance)

ggplot(imp_1, aes())

plot.variable(rf_1, plots.per.page = 3)
plot.variable(rf_2)

