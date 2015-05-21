# ---------------------------------------------------------------------------- #
# EIU Descriptive plots for the paper
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

#### Load pacakges ####
library(rio)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

#### Set working directory ####
setwd('/git_repositories/EIUCrisesMeasure/')

#### Scree plot ####
eigen <- import('data/kpca_eigen_2015_03_18.csv')

ggplot(eigen, aes(components, eigenvalues)) +
    geom_line() +
    geom_point() +
    xlab('\nNumber of Components') + ylab('Eigenvalues\n') +
    theme_bw()

ggsave('summary_paper/analysis/figures/scree_plot.pdf')


#### Set Up ####
# Load data
kpca_results <- import('data/results_kpca_rescaled.csv')

#Convert date from character to date
kpca_results$date <- ymd(kpca_results$date)

#### Plot a selection of countries ####
# Function to plot individual country scores
kpca_plotter <- function(indvidual, data = kpca_results){
    temp_data <- subset(data, country == indvidual)
    indv <- ggplot(temp_data, aes(date, C1_ma, group = country)) +
                geom_line(alpha = 0.3) +
                stat_smooth(se = F, colour = 'black') +
                geom_hline(yintercept = 0, linetype = 'dotted') +
                scale_y_continuous(limits = c(0, 1),
                                   breaks = c(0, 0.25, 0.5, 0.75, 1)) +
                xlab('') + ggtitle(indvidual) +
                ylab('') +
                theme_bw()
    return(indv)
}

country_vector <- unique(kpca_results$country)
kpca_list <- list()
for (i in country_vector) {
    message(i)
    kpca_list[[i]] <- suppressMessages(kpca_plotter(indvidual = i))
}

# Plot selection
select_countries <- c('Australia', 'Austria', 'Belgium', 'Brazil', 'China',
                    'Iceland', 'India', 'Ireland', 'United Kingdom', 'United States'
                    )
pdf(file = 'summary_paper/analysis/figures/select_scores.pdf', width = 15)
plots <- do.call(grid.arrange, kpca_list[select_countries])
dev.off()
