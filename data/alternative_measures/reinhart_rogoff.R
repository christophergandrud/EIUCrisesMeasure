# ---------------------------------------------------------------------------- #
# Download and clean Reinhart and Rogoff
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(simpleSetup)
pkgs <- c('rio', 'dplyr', 'psData', 'DataCombine', 'lubridate')
library_install(pkgs)

# Set working directory
pos <- c('/git_repositories/EIUCrisesMeasure/', 
         '~/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos)

# cbind_fill function
cbind_fill <- function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function(x)
        rbind(x, matrix(, n - nrow(x), ncol(x)))))
}

# Download data
cleaned_data <- list.files('data/alternative_measures/cleaned/')
if (!('reinhart_rogoff.csv' %in% cleaned_data)) {
    rein_rog <- RRCrisisGet()
    rein_rog_sub <- rein_rog %>% select(iso2c, year, RR_BankingCrisis,
                                        RR_StockMarketCrash, RR_CurrencyCrisis)
    rein_rog_sub$date <- sprintf('%s-06-01', rein_rog_sub$year)
    rein_rog_sub$date <- ymd(rein_rog_sub$date)
    rein_rog_sub <- rein_rog_sub %>% select(-year)

    export(rein_rog_sub,
           file = 'data/alternative_measures/cleaned/reinhart_rogoff.csv')
} else if (('reinhart_rogoff.csv' %in% cleaned_data)) {
    rein_rog_sub <- import('data/alternative_measures/cleaned/reinhart_rogoff.csv')
    rein_rog_sub$date <- ymd(rein_rog_sub$date)
}

# Keep only data from 1970
rr_spell <- rein_rog_sub %>% filter(date >= '1970-01-01')

# Function to find Spells
rr_spell_finder <- function(x) {
    # Find crisis start and stop
    rr_spell <- StartEnd(rr_spell, SpellVar = x,
                         GroupVar = 'iso2c', SpellValue = 1)

    # Find start and stop dates
    rr_spell <- rr_spell %>% select(iso2c, date, Spell_Start, Spell_End) %>%
        filter(Spell_Start == 1 | Spell_End == 1)

    rr_Start <- rr_spell %>% select(iso2c, date, Spell_Start) %>%
                    filter(Spell_Start == 1)

    rr_End <- rr_spell %>% select(iso2c, date, Spell_End) %>%
                filter(Spell_End == 1)

    rr_comb <- data.frame()
    for (i in unique(rr_spell$iso2c)) {
        temp_start <- subset(rr_Start, iso2c == i) %>% select(iso2c, date)
        names(temp_start) <- c('iso2c', sprintf('%s_start', x))

        temp_end <- subset(rr_End, iso2c == i)  %>% select(date)
        end_name <- sprintf('%s_end', x)
        names(temp_end) <- end_name

        temp_comb <- cbind_fill(temp_start, temp_end)

        rr_comb <- rbind(rr_comb, temp_comb)
        rr_comb[, 3] <- rr_comb[, 3] %>% as.character
        rr_comb[, 3][is.na(rr_comb[, 3])] <- '2009-12-30' # Assume end 2009
    }

    rr_comb[, 'iso2c'] <- rr_comb[, 'iso2c'] %>% as.character
    rr_comb[, 2] <- rr_comb[, 2] %>% ymd
    rr_comb[, 3] <- rr_comb[, 3] %>% ymd

    rr_comb <- DropNA(rr_comb, 'iso2c')
    return(rr_comb)
}

rr_bc <- rr_spell_finder('RR_BankingCrisis')
rr_sc <- rr_spell_finder('RR_StockMarketCrash')
rr_cc <- rr_spell_finder('RR_CurrencyCrisis')
