# ---------------------------------------------------------------------------- #
# Compare to Andrianova et al. (2015) Financial Fragility indicators and
# GFDD indicators
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(simpleSetup)

pkgs <- c('rio', 'repmis', 'dplyr', 'lubridate', 'countrycode', 'corrplot',
          'DataCombine', 'WDI', 'gridExtra', 'ggplot2')
library_install(pkgs)

# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)

# Load and combine data ---------------------------------------------------

# Rescale function
range01 <- function(x, na.rm = T){
    (x - min(x, na.rm = na.rm)) /
        (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

# Combined sig. tests function
# From https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
            uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}

# Import FinStress
FinStress <- import('data/FinStress.csv')

FinStress$iso2c <- countrycode(FinStress$iso3c, origin = 'iso3c', 
                               destination = 'iso2c')
FinStress <- dplyr::select(FinStress, -iso3c)

ff <- import('data/alternative_measures/Financial Fragility Database Stata.dta') %>%
    select(-countryname) %>% 
    dplyr::rename(iso2c = countrycode)

# Convert FinStress to annual so that it is on a comparable annual scale
FinStress$year <- FinStress$date %>% year
FinStress_sum <- FinStress %>% group_by(iso2c, year) %>%
    summarise(mean_stress = mean(FinStress, na.rm = T))

# Merge data series together
comb <- merge(FinStress_sum, ff, by = c('iso2c', 'year'), all.x = T)
comb$country <- countrycode(comb$iso2c, origin = 'iso2c',
                            destination = 'country.name')
comb <- comb %>% MoveFront('country')

# GFDD -------------------------------------------------------------------------
wdi <- WDI(indicator = c('GFDD.SI.03', 'GFDD.SI.04', 'GFDD.SI.02', 'GFDD.SI.05',
                         'GFDD.SI.07',
    
    'PA.NUS.FCRF', 'GFDD.SM.01','GFDD.OM.02', 'GFDD.DI.12'), 
           start = 2000, end = 2013, extra = T) %>%
    rename(capital_asset_ratio = GFDD.SI.03) %>%
    rename(credit_deposits_ratio = GFDD.SI.04) %>%
    rename(npl_ratio = GFDD.SI.02) %>% 
    rename(reg_capital_assets = GFDD.SI.05) %>%
    rename(provisions_to_npls = GFDD.SI.07) %>%
    rename(exchange_rate_usd = PA.NUS.FCRF) %>%
    rename(stock_price_volatility = GFDD.SM.01) %>%
    rename(stock_returns = GFDD.OM.02) %>%
    rename(private_credit = GFDD.DI.12) %>%
    select(-country, -iso3c, -region, -capital, -longitude, -latitude, -lending)

# Create annual percentage change in exchange rates and private credit
wdi <- change(wdi, GroupVar = 'iso2c',
                  Var = 'exchange_rate_usd', NewVar = 'exchange_rate_change')
wdi <- change(wdi, GroupVar = 'iso2c', 
                  Var = 'private_credit', NewVar = 'private_credit_change')

comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T)

# Log highly skewed variables
comb$log_imploans <- log(comb$ImpLoans)
comb$exchange_rate_usd_log <- log(comb$exchange_rate_usd)
comb$private_credit_log <- log(comb$private_credit)

# Save data
export(comb, file = 'data/alternative_measures/cleaned/fin_frag_gfdd.csv')

# Plotting function -------------------------------------------------------

comp_plot <- function(data = comb, ff_var, ff_label) {
    temp <- comb[, c('mean_stress', ff_var)]
    names(temp) <- c('mean_stress', 'comp_temp')
    if (missing(ff_label)) ff_label = ff_var
    ggplot(temp, aes(mean_stress, comp_temp)) +
        geom_point(alpha = 0.3) +
        stat_smooth(method = 'lm', se = F) +
        xlab('\nFinStress') + ylab(paste0(ff_label, '\n')) +
        theme_bw()
}

# Correlate FinStress with impared and plot -------------------------------

# Variables to compare to and labels 
comp_vars <- c('Equity', 'log_imploans', 'Costs', 'ROAA', 'NetLoans', 'Liquid',
               'NCO')
comp_vars_labels <- c('Equity to\n Total Assets', 
                      'Impaired Assets\n to Gross Loans (log)', 
                      'Manegerial\n Efficiency', 'Return on\n Average Assets', 
                      'Net Loans\nto Total Assets',
                      'Liquid Assets\nto Total Assets', 
                      'Net Charge-offs\nto total loans')

cor_for_loop <- function(x) {
    out <- cor.test(comb$mean_stress, comb[, x])
    message(x)
    print(out)
}

for (i in comp_vars) cor_for_loop(i)

cor_matrix <- comb[, c('mean_stress', comp_vars)] %>% DropNA(., names(.))
names(cor_matrix) <- c('FinStress', comp_vars_labels)
corred <- cor(cor_matrix)
cor_sig <- cor.mtest(corred, 0.95)

pdf(file = 'summary_paper/figures/ff_corr_matrix.pdf', width = 10, height = 10)
    corrplot(corred, method = 'number', type = 'lower', diag = F)
dev.off()

to_comp <- data.frame(comp_vars, comp_vars_labels, stringsAsFactors = F)

comp_list <- list()
for (i in 1:nrow(to_comp)) {
    message(to_comp[i, 1])
    comp_list[[i]] <- comp_plot(ff_var = to_comp[i, 1], 
                                ff_label = to_comp[i, 2])
}

# Save Impared Loans and Liquid Liabilities
ggsave(comp_list[[2]], file = 'summary_paper/figures/compare_impared.pdf')
ggsave(comp_list[[6]], file = 'summary_paper/figures/compare_liquid.pdf')


# Save all together
pdf(file = 'summary_paper/figures/fin_fragility_compare.pdf', width = 11, 
    height = 14)
    do.call(grid.arrange, comp_list)
dev.off()

# Find impared assets/FinStress outliers ---------------------------------------
test <- comb %>% filter(log_imploans < 0, mean_stress > 0.6)

