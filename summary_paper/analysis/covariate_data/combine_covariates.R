# ---------------------------------------------------------------------------- #
# Combine Data Sets for Regressions on Gov Liabilities/Spending
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(tidyr)
library(countrycode)
library(lubridate)
library(DataCombine)
library(psData)

setwd('/git_repositories/EIUCrisesMeasure/summary_paper/analysis/covariate_data/')

# ------------------------------ Load and Clean Data ------------------------- #

#### OECD ####
# Import OECD Output Gap data
output_gap <- import('raw/oecd_output_gap.csv', header = T)

output_gap <- output_gap %>%
                gather(year, output_gap, 2:ncol(output_gap)) %>%
                arrange(country, year)

# Import OECD General gov. gross financial liabilities (% of GDP)
gov_liab <- import('raw/oecd_gov_liabilities.csv')

# Import OECD Total Gov Spending
gov_total_spend <- import('raw/oecd_total_gov_spend.csv', header = T)
    
gov_total_spend <- gov_total_spend %>% 
                    gather(year, gov_spend, 2:ncol(gov_total_spend)) %>%
                    arrange(country, year)
gov_total_spend$year <- gov_total_spend$year %>% as.character %>% as.numeric

# Import OECD Government Spending on Economic Affairs
gov_econ_spend <- import('raw/oecd_gov_spend_econ.csv', header = T)

gov_econ_spend <- gov_econ_spend %>% 
                    gather(year, gov_econ_spend, 2:ncol(gov_econ_spend)) %>%
                    arrange(country, year)
gov_econ_spend$year <- gov_econ_spend$year %>% as.character %>% as.numeric

# Import OECD GDP billions of 2005 USD
gdp <- import('raw/oecd_gdp.csv')
gdp$gdp_billions <- gsub(',', '', gdp$gdp_billions) %>% as.numeric

# Clean up Output Gap and Gov. Liabilities
iso_oecd <- function(df) {
    df$iso2c <- countrycode(df$country, origin = 'country.name',
                                  destination = 'iso2c')
    df <- df %>% MoveFront('iso2c') %>% select(-country)
    return(df)
}

output_gap <- output_gap %>% iso_oecd
gov_liab <- gov_liab %>% iso_oecd
gov_total_spend <- gov_total_spend %>% iso_oecd
gov_econ_spend <- gov_econ_spend %>% iso_oecd
gdp <- gdp %>% iso_oecd

# Find raw government liabilities

fix_gdp <- function(data, var, fix_year = 2005) {
    data <- merge(data, gdp, by = c('iso2c', 'year'))
    
    raw_var <- sprintf('%s_raw', var)
    var_2005 <- sprintf('%s_gdp%s', var, fix_year)
    var_change <- sprintf('%s_change', var_2005)
    
    data$temp_prop <- data[, var] / 100
    data[, raw_var] <- data$temp_prop * data$gdp_billions
    
    # As a percent of 2005 GDP
    gdp_2005 <- gdp %>% filter(year == fix_year) %>% select(iso2c, gdp_billions) %>%
                    rename(gdp_2005 = gdp_billions)
    
    data <- merge(data, gdp_2005)
    data[, var_2005] <- (data[, raw_var] / data$gdp_2005) * 100
    data <- PercChange(data, Var = var_2005,
                           GroupVar = 'iso2c', NewVar = var_change)
    data <- data %>% select(-temp_prop)
    return(data)
}

gov_liab <- fix_gdp(data = gov_liab, var = 'gov_liabilities')
gov_total_spend <- fix_gdp(data = gov_total_spend, var = 'gov_spend') %>% 
                    select(-gov_spend_raw, -gdp_2005, -gdp_billions)
gov_econ_spend <- fix_gdp(data = gov_econ_spend, var = 'gov_econ_spend') %>% 
    select(-gov_econ_spend_raw, -gdp_2005, -gdp_billions)

# Merge
oecd <- merge(gov_liab, output_gap, by = c('iso2c', 'year'), all = T)
oecd <- merge(oecd, gov_total_spend, by = c('iso2c', 'year'), all = T)
oecd <- merge(oecd, gov_econ_spend, by = c('iso2c', 'year'), all = T)

#### Import DPI ####
dpi <- DpiGet(vars = c('execrlc', 'yrcurnt')) %>%
        select(iso2c, year, execrlc, yrcurnt)
dpi$execrlc[dpi$execrlc == -999] <- NA
dpi$yrcurnt[dpi$yrcurnt == -999] <- NA

# Import election timing
eu_election <- import('https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_original_corrected.csv') %>%
                select(iso2c, year, yrcurnt_corrected)

##### Import Kayser Lin loss probability variable ####
loss_prob <- import('raw/TheLossProbVariable.csv') %>%
            select(isocode, elecyr, lpr, lpr2) %>%
            rename(iso2c = isocode) %>%
            rename(year = elecyr)
loss_prob$iso2c[loss_prob$iso2c == 'AUL'] <- 'AUS'
loss_prob$election_year <- 1

loss_prob$iso2c <- countrycode(loss_prob$iso2c, origin = 'iso3c',
                               destination = 'iso2c')

##### Import EPFMS ####
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/results_kpca_rescaled.csv'
epfms_index <- import(URL)

# Convert EPFMS to annual so that it is comparable with the FRT
epfms_index$year <- epfms_index$date %>% year
epfms_sum <- epfms_index %>% group_by(iso2c, year) %>%
    summarise(mean_stress = mean(C1_ma, na.rm = T))

#### Henisz Political Constraints ####
constraints <- import('raw/polcon2012.dta') %>%
                    select(polity_country, year, polconiii, polconv) %>%
                    rename(country = polity_country)

constraints <- constraints %>% iso_oecd

#### Merge All ###
comb <- merge(epfms_sum, oecd, by = c('iso2c', 'year'))
comb <- merge(comb, dpi, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, eu_election, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, loss_prob, by = c('iso2c', 'year'), all = T)
comb <- merge(comb, constraints, by = c('iso2c', 'year'), all.x = T)

comb$election_year[is.na(comb$election_year)] <- 0

comb <- FillDown(comb, 'lpr')
comb <- FillDown(comb, 'lpr2')

comb$country <- countrycode(comb$iso2c, origin = 'iso2c',
                            destination = 'country.name')
comb <- comb %>% MoveFront('country')

# Create year lags
vars_to_lag <- c('mean_stress', 'output_gap', 'gov_liabilities',
                 'gov_liabilities_gdp2005', 'gov_liabilities_gdp2005_change', 
                 'gov_spend', 'gov_spend_gdp2005', 'gov_spend_gdp2005_change',
                 'gov_econ_spend', 'gov_econ_spend_gdp2005', 
                 'gov_econ_spend_gdp2005_change',
                 'lpr', 'lpr2', 'election_year')

lagger <- function(var) {
    newvar <- sprintf('%s_1', var)
    out <- slide(comb, Var = var, TimeVar = 'year', GroupVar = 'iso2c',
                 NewVar = newvar)
    return(out)
}

for (i in vars_to_lag) comb <- lagger(i)


# Save data 
export(comb, file = 'epfms_covariates.csv')
