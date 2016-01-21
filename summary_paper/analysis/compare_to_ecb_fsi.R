# --------------------------------------------------------------------------- #
# Compare FinStress to Klaus et al. (2015)
# Downloaded from https://sites.google.com/site/thibautduprey/research/crisesdating
# MIT LICENSE
# --------------------------------------------------------------------------- #


library(rio)
library(repmis)
library(tidyr)
library(dplyr)
library(countrycode)
library(lubridate)
library(ggplot2)
library(DataCombine)

possibles <- '/git_repositories/EIUCrisesMeasure/'
set_valid_wd(possibles)

# FSI from Klaus et al. (2015) -----
fsi <- rio::import('data/alternative_measures/raw/Duprey et al - Crises Dating - dataset.xls',
                    sheet = 3)

countries_count <- ncol(fsi)
fsi <- fsi %>% gather(iso2c, fsi, 2:countries_count)

fsi$iso2c <- gsub('_fsi', '', fsi$iso2c)
fsi$iso2c <- countrycode(fsi$iso2c, origin = 'iso2c', destination = 'iso2c', 
                         warn = T)
names(fsi) <- c('date', 'iso2c', 'fsi')
fsi$date <- ymd(fsi$date)
fsi$fsi[fsi$fsi == -1] <- NA

# FinStress -------
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- rio::import(URL)
finstress_index$date <- ymd(finstress_index$date)

comb <- merge(finstress_index, fsi, by = c('iso2c', 'date'))

cor.test(comb$FinStress, comb$fsi)

comb_plotable <- comb %>% gather(measure, value, 4:5)

## Plot --------
ggplot(comb_plotable, aes(date, value, group = measure, colour = measure)) +
    geom_line() +
    facet_wrap(~ country) +
    xlab('') + ylab('') + 
    theme_bw()


# First differences ------
comb_plotable_gb <- comb_plotable %>% group_by(iso2c, measure) %>%
    mutate(first_diff = value - dplyr::lag(value))

ggplot(comb_plotable_gb, aes(date, first_diff, group = measure, 
                             colour = measure)) +
    geom_line(alpha = 0.5) +
    facet_wrap(~ country) +
    xlab('') + ylab('') + 
    theme_bw()
