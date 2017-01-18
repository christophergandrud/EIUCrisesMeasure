#########################################
# Compare KPCA with 3, 4, 5, and 6 character kernals and Pre-crisis sample
# Christopher Gandrud
# MIT License
#########################################

# Load packages
library(repmis)
library(rio)
library(DataCombine)
library(gridExtra)
library(ggplot2)
theme_set(theme_bw())

# Set working directory
pos <- c('/git_repositories/EIUCrisesMeasure/',
         '~/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos)

# Load data
load_flip <- function(path, length_spec) {
    temp <- import(path)
    temp$C1_ma <- 1 - temp$C1_ma
    temp <- temp[, c(2:3, ncol(temp))]
    names(temp) <- c('iso3c', 'date', sprintf('C1_ma_%s', length_spec)) 
    return(temp)
}

kpca3 <- load_flip('source/pca_kpca/raw_data_output/non_5_strings/results_kpca_3_rescaled.csv',
                   3)
kpca4 <- load_flip('source/pca_kpca/raw_data_output/non_5_strings/results_kpca_4_rescaled.csv',
                   4)
kpca5 <- load_flip('source/pca_kpca/raw_data_output/5_strings/results_kpca_5_rescaled.csv',
                   5)
kpca6 <- load_flip('source/pca_kpca/raw_data_output/non_5_strings/results_kpca_6_rescaled.csv',
                   6)
pre_crisis <- load_flip('source/pca_kpca/raw_data_output/pre_crisis/results_kpca_5_rescaled.csv',
                        'pre_crisis')

comb <- merge(kpca3, kpca4, by = c('iso3c', 'date'), all = TRUE)
comb <- merge(comb, kpca5, by = c('iso3c', 'date'), all = TRUE)
comb <- merge(comb, kpca6, by = c('iso3c', 'date'), all = TRUE)
comb <- merge(comb, pre_crisis, by = c('iso3c', 'date'), all.x = TRUE)


# Plot bivariate_relationships
p3 <- ggplot(comb, aes(C1_ma_5, C1_ma_3)) + geom_point(alpha = 0.2) +
        xlab('') + ylab('KPCA 3 Character Kernals\n')
p4 <- ggplot(comb, aes(C1_ma_5, C1_ma_4)) + geom_point(alpha = 0.2) +
        xlab('') + ylab('KPCA 4 Character Kernals\n')
p6 <- ggplot(comb, aes(C1_ma_5, C1_ma_6)) + geom_point(alpha = 0.2) +
        xlab('') + ylab('KPCA 6 Character Kernals\n')
p_pre <- ggplot(comb, aes(C1_ma_5, C1_ma_pre_crisis)) + geom_point(alpha = 0.2) +
        xlab('') + ylab('\nPre-2008 Sample (5 chr. kernals)')

grid.arrange(p3, p4, p6, p_pre, bottom = '\nFinStress (5 chr. kernals)')



