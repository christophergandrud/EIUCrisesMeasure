# ---------------------------------------------------------------------------- #
# Random Forest Regressions with Andrianova et al. (2015) Financial Fragility 
# Indicators and GFDD indicators
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(repmis)
library(DataCombine)
library(randomForestSRC)
library(dplyr)
library(ggplot2)

# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)

comb <- import('data/alternative_measures/cleaned/fin_frag_gfdd.csv')

# Variables to include as predictors
comp_vars <- c('log_imploans', 'ROAA', 'provisions_to_npls', 
               'Costs', 'private_credit_log', 'stock_price_volatility', 
               'Equity', 'reg_capital_assets', 'Z', 'capital_asset_ratio',
               'NetLoans', 'Liquid', 'credit_deposits_ratio', 'NCO', 
               'stock_returns' #, 'exchange_rate_change'
                )

var_labels <- c('Imp. Loans', 'ROAA', 'Provisions/NPLs', 'Manag. Eff.',
                'Private Credit', 'Stock Volatility', 'Equity', 
                'Reg. Capital/Assets', 'Z-Score', 'Capital/Assets', 
                'Net Loans/Assets',
                'Liquid Assets/Assets', 'Credit/Deposits', 'Net Change-Offs',
                'Stock Returns' #, 'Exchange Rate Change'
                )

comb <- comb[, c('mean_stress', comp_vars, 'income')]
names(comb) <- c('mean_stress', var_labels, 'income')

comb_no_na_all <- comb %>% DropNA(c('mean_stress', var_labels))

comb_no_na_oecd <- comb_no_na_all %>% filter(income == 'High income: OECD')

form_all_vars <- paste('mean_stress ~', paste0("`", var_labels, "`",
                                               collapse = ' + ')) %>%
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

imp <- extract_importance(rf_1$importance)
imp <- imp %>% arrange(desc(variable_importance))
imp$variable_importance <- imp$variable_importance * 100

# Plot variable importance
var_imp <- ggplot(imp, aes(variable_importance, 
                y = reorder(variable, variable_importance))) +
    geom_point() +
    ylab('') + xlab('\n% MSE Increase') +
    theme_bw()

ggsave(var_imp, filename = 'summary_paper/figures/rf_variable_imp.pdf')

# Plot 
pdf(file = 'summary_paper/figures/rf_partial_dependence.pdf', 
    width = 17, height = 15)
    plot.variable(rf_1, plots.per.page = 5)
dev.off()

