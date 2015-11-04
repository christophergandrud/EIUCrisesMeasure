# ---------------------------------------------------------------------------- #
# Find and print texts for minimum and maximum FinStress values
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(repmis)
library(rio)
library(dplyr)

# Set working directory of parsed texts. Change as needed.
pos_directs <- c('~/Desktop/eiu/eiu_extracted',
                 '/Volumes/Gandrud1TB/eiu/eiu_extracted/')

set_valid_wd(pos_directs)

# Directory for saving texts to
dir <- '/git_repositories/EIUCrisesMeasure/summary_paper/sample_min_max_texts/'

# Find high and low FinStress texts for a sample of countries

# Import FinStress
FinStress <- import('http://bit.ly/1LFEnhM', format = 'csv')

# Countries
countries <- c('Brazil', 'Latvia', 'Iceland', 'Ireland')

sub <- FinStress[FinStress$country %in% countries,]

sub_summary <- sub %>% group_by(country) %>% 
    mutate(min_finstress = min(C1_ma, na.rm = T),
              max_finstress = max(C1_ma, na.rm = T)
    )

min_dates <- sub_summary %>% filter(min_finstress == C1_ma) %>% select(country, date) %>% as.data.frame
max_dates <- sub_summary %>% filter(max_finstress == C1_ma) %>% select(country, date) %>% as.data.frame



gath_cat_texts <- function(data, type) {
    list_temp_text <- list()
    for (i in 1:nrow(data)){
        date_value <- data[i, 'date']
        country_value <- data[i, 'country']
        country_searcher <- gsub(pattern = ' ', replacement = '-', country_value)
        message(paste(country_value, date_value))
        temp <- readLines(sprintf('%s_%s.txt', date_value, country_searcher)) 

        cat(sprintf('## %s (%s)\n', country_value, date_value),
            temp, 
            sep = '\n',
            file = sprintf('%s%s_%s_%s.md', 
                           dir, type, country_searcher, date_value)
        )
    }
} 

gath_cat_texts(min_dates, type = 'min')
gath_cat_texts(max_dates, type = 'max')



