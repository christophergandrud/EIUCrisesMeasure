# ---------------------------------------------------------------------------- #
# Compare to Andrianova et al. (2015) Financial Fragility indicators
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(rio)
library(repmis)
library(dplyr)
library(lubridate)
library(countrycode)
library(DataCombine)
library(ggplot2)
library(gridExtra)

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

# Import FinStress
FinStress <- import('http://bit.ly/1LFEnhM', format = 'csv')

ff <- import('data/alternative_measures/Financial Fragility Database Stata.dta') %>%
    select(-countryname) %>% 
    dplyr::rename(iso2c = countrycode)

# Convert FinStress to annual so that it is on a comparable annual scale
FinStress$year <- FinStress$date %>% year
FinStress_sum <- FinStress %>% group_by(iso2c, year) %>%
    summarise(mean_stress = mean(C1_ma, na.rm = T))

# Merge data series together
comb <- merge(FinStress_sum, ff, by = c('iso2c', 'year'), all.x = T)
comb$country <- countrycode(comb$iso2c, origin = 'iso2c',
                            destination = 'country.name')
comb <- comb %>% MoveFront('country')

# Plotting function -------------------------------------------------------

comp_plot <- function(data = comb, ff_var, ff_label) {
    temp <- comb[, c('mean_stress', ff_var)]
    names(temp) <- c('mean_stress', 'comp_temp')
    ggplot(temp, aes(mean_stress, comp_temp)) +
        geom_point(alpha = 0.3) +
        stat_smooth(method = 'lm', se = F) +
        xlab('\nFinStress') + ylab(paste0(ff_label, '\n')) +
        theme_bw()
}

# Correlate FinStress with impared and plot -------------------------------
comb$log_imploans <- log(comb$ImpLoans)

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

pdf(file = 'summary_paper/figures/fin_fragility_compare.pdf', width = 11, 
    height = 14)
    do.call(grid.arrange, comp_list)
dev.off()

# Find impared assets/FinStress outliers ---------------------------------------
test <- comb %>% filter(log_imploans < 0, mean_stress > 0.6)



# Explore devations in reporting vs. FinStress ----------------------------

plot(log(comb$ImpLoans), log(comb$ImpLoansH))

comb$imp_deviate <- (comb$ImpLoans - comb$ImpLoansH5) / comb$ImpLoans

sub_0 <- comb %>% filter(round(imp_deviate, digits = 2) != 0)

plot(sub_0$mean_stress, sub_0$imp_deviate)

# Create deviation function 
deviates <- function(data, var) {
    
}
