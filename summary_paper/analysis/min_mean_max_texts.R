# ---------------------------------------------------------------------------- #
# Find and print texts for minimum and maximum FinStress values
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(setupPkg)

pkgs <- c('repmis', 'rio', 'dplyr')
library_install(pkgs)

# Set working directory of parsed texts. Change as needed.
pos_directs <- c('~/Desktop/eiu/eiu_extracted',
                 '/Volumes/Gandrud1TB/eiu/eiu_extracted/')

set_valid_wd(pos_directs)

# Directory for saving texts to
head <- '~/git_repositories/EIUCrisesMeasure/'
dir <- sprintf('%ssummary_paper/sample_min_max_texts/', head)

# Find high and low FinStress texts for a sample of countries ------------------

# Import FinStress
FinStress <- import(sprintf('%sdata/FinStress.csv', head))

# Countries
countries <- c('Brazil', 'Latvia', 'Iceland', 'Ireland')

sub <- FinStress[FinStress$country %in% countries,]

sub_summary <- sub %>% group_by(country) %>% 
    mutate(min_finstress = min(FinStress, na.rm = T),
              max_finstress = max(FinStress, na.rm = T)
    )

min_dates <- sub_summary %>% filter(min_finstress == FinStress) %>% 
                    select(country, date) %>% as.data.frame
max_dates <- sub_summary %>% filter(max_finstress == FinStress) %>% 
                    select(country, date) %>% as.data.frame



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
