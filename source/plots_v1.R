##################
# Plot selected countries' MIFMS
# Christopher Gandrud
# MIT License
##################


# Set working directory of parsed texts. Change as needed.
setwd('/git_repositories/EIUCrisesMeasure/')

# Load required packages
library(rio)
library(lubridate)
library(ggplot2)
library(gridExtra)

# Load data
results_kpca <- import('data/results_kpca.csv')
results_kpca$date <- ymd(results_kpca$date)

# List of countries to plot
countries_global <- c('Australia', 'Argentina', 'Belgium', 'Brazil', 
               'Canada', 'China', 'Denmark', 'Germany', 
               'Greece', 'Iceland', 'Ireland', 'Italy', 
               'Japan', 'South Korea', 'United Kingdom', 'United States' 
               )

countries_seAsia <- c('Bangladesh', 'India', 'Indonesia', 'Malaysia', 
                      'Pakistan', 'Singapore', 'Sri Lanka', 'Thailand' 'Vietnam')

# Plot results
kpca_plotter <- function(indvidual, data = results_kpca){
    temp_data <- subset(data, country == indvidual)
    indv <- ggplot(temp_data, aes(date, C1*-1, group = country)) +
        geom_line(alpha = 0.3) +
        stat_smooth(se = F, colour = 'black') +
        geom_hline(yintercept = 0, linetype = 'dotted') +
        scale_y_continuous(limits = c(-0.3, 0.2), 
                           breaks = c(-0.25, -0.1, 0, 0.1)) +
        # scale_color_brewer(palette = 'Set1', name = '') +
        xlab('') + ggtitle(indvidual) +
        ylab('') +
        theme_bw()
    return(indv)
}

kpca_list <- list()
for (i in countries_seAsia){
    message(i)
    kpca_list[[i]] <- suppressMessages(kpca_plotter(indvidual = i))
}

do.call(grid.arrange, kpca_list)
