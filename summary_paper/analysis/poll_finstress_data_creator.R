# ---------------------------------------------------------------------------- #
# FinStress and Jennings/Wlezien data set creator
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(rio)
library(DataCombine)
library(WDI)
library(dplyr)
library(lubridate)
library(countrycode)
library(WDI)

# Set working directory
setwd('/Volumes/SAMSUNG128/data/Jennings_Wlezien_2016/')

# Load poll data from Jennings and Wlezien
poll <- import('LONG_MI.dta')
# Recode Finland Green League error
poll$gov_[poll$partyid == 5 & poll$gov_ == 7] <- 1

country_id <- import('other_covars/countryid_list.csv')
country_id$iso2c <- countrycode(country_id$country, origin = 'country.name',
                                destination = 'iso2c')
poll <- left_join(poll, country_id, by = 'countryid')

polled_iso2c <- unique(poll$iso2c)

# Load FinStress Data ----------------------------------------------------------
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- rio::import(URL)

finstress_index$date <- ymd(finstress_index$date)
finstress_index <- subset(finstress_index, iso2c %in% polled_iso2c)

# Change from 12 month moving average
finstress_index <- slideMA(finstress_index, Var = 'FinStress',
                           GroupVar = 'country', periodBound = -11,
                           NewVar = 'fs_ma12')
finstress_index$fs_change_ma12 <- finstress_index$FinStress - 
    finstress_index$fs_ma12

# Find change from 12 months before
finstress_index <- change(finstress_index, Var = 'FinStress', 
                          GroupVar = 'country', slideBy = -11,
                          NewVar = 'fs_change12', type = 'absolute')

finstress_index <- finstress_index %>% rename(month_year = date) %>% 
    dplyr::select(-country)

# Laeven and Valencia banking crisis measure ----------------------------------- 
wdi <- WDI(indicator = c('NY.GDP.MKTP.KD.ZG', 'FP.CPI.TOTL.ZG', 
                         'SL.UEM.TOTL.ZS',
                         'GFDD.OI.19', 'GFDD.DI.14'), 
           start = 1960, end = 2015)

wdi$year <- sprintf('%s-01-01', wdi$year) %>% ymd

wdi <- wdi %>% dplyr::select(-country) %>% 
    rename(gdp_growth = NY.GDP.MKTP.KD.ZG) %>%
    rename(inflation = FP.CPI.TOTL.ZG) %>%
    rename(unemployment = SL.UEM.TOTL.ZS) %>%
    rename(lv_crisis = GFDD.OI.19) %>% 
    rename(domestic_credit_to_private = GFDD.DI.14)

wdi_crisis <- wdi %>% dplyr::select(iso2c, year, lv_crisis, gdp_growth,
                                    inflation, unemployment, 
                                    domestic_credit_to_private)


# OECD Growth and Unemployment data --------------------------------------------
## Downloaded from https://data.oecd.org
oecd_growth <- import('other_covars/oecd_gdp_growth.csv')
oecd_unemployment <- import('other_covars/oecd_unemployment.csv')

# Keep desired frequency 
oecd_growth <- oecd_growth %>% filter(FREQUENCY == 'Q' & MEASURE == 'PC_CHGPY') 
oecd_growth <- oecd_growth[, c(1, 6, 7)] %>% 
    setNames(c('country3', 'quarter', 'oecd_gdp_growth'))
oecd_growth$quarter <- gsub('-Q', '.', oecd_growth$quarter) %>% as.numeric
oecd_growth$iso2c <- countrycode(oecd_growth$country3, origin = 'iso3c',
                                 destination = 'iso2c')
oecd_growth <- oecd_growth %>% select(-country3)
oecd_growth <- oecd_growth %>% filter(!is.na(iso2c))

oecd_unemployment <- oecd_unemployment %>% filter(FREQUENCY == 'M')
oecd_unemployment <- oecd_unemployment[, c(1, 6, 7)] %>% 
    setNames(c('country3', 'month_year', 
               'oecd_unemployment'))
oecd_unemployment$month_year <- paste0(oecd_unemployment$month_year, '-01') %>%
    ymd
oecd_unemployment$iso2c <- countrycode(oecd_unemployment$country3, 
                                       origin = 'iso3c', destination = 'iso2c')
oecd_unemployment <- oecd_unemployment %>% select(-country3)

# Change from 12 month moving average
oecd_unemployment <- slideMA(oecd_unemployment, Var = 'oecd_unemployment',
                             GroupVar = 'iso2c', periodBound = -11,
                             NewVar = 'unemp_ma12')
oecd_unemployment$unemp_change_ma12 <- oecd_unemployment$oecd_unemployment - 
    oecd_unemployment$unemp_ma12

# Find change from 12 months before
oecd_unemployment <- change(oecd_unemployment, Var = 'oecd_unemployment', 
                            GroupVar = 'iso2c', slideBy = -11,
                            NewVar = 'unemp_change12', type = 'absolute')

# Merge with Load/merge Jennings and Wlezien ----------------------------------------------------
# Fixed effect variables
poll$parXcou_ <- poll$countryid + (poll$partyid/100)
poll$elecXcou_ <- paste0(poll$countryid, '_', poll$elecdate)
poll$party_election <- paste0(poll$parXcou_, '_', poll$elecdate)

# Find change from previous poll (per month)
poll$month_year <- round_date(poll$polldate, unit = 'month')
poll_monthly <- poll %>% group_by(party_election, month_year) %>% 
    summarise(poll_month_mean = mean(poll_, na.rm = TRUE),
              ipoll_month_mean = mean(ipoll_, na.rm = TRUE))
poll_monthly <- as.data.frame(poll_monthly)
for (i in names(poll_monthly)) poll_monthly[, i][is.nan(poll_monthly[, i])] <- NA

poll_monthly <- change(poll_monthly, Var = 'poll_month_mean', 
                       GroupVar = 'party_election', TimeVar = 'month_year', 
                       NewVar = 'poll_mean_change', type = 'absolute')
poll_monthly <- change(poll_monthly, Var = 'ipoll_month_mean', 
                       GroupVar = 'party_election', TimeVar = 'month_year', 
                       NewVar = 'ipoll_mean_change', type = 'absolute')

# Create variable of months since last poll
poll_m_sub <- poll_monthly[, c('party_election', 'month_year', 'poll_month_mean')]
poll_m_sub <- DropNA(poll_m_sub, 'poll_month_mean')
poll_m_sub <- slide(poll_m_sub, Var = 'month_year', GroupVar = 'party_election',
                    TimeVar = 'month_year', NewVar = 'month_yearlag1')
poll_m_sub$month_yearlag1 <- as.Date(poll_m_sub$month_yearlag1, 
                                     origin = '1970-01-01')
poll_m_sub$months_since_poll  <- ((difftime(poll_m_sub$month_year, 
                                            poll_m_sub$month_yearlag1, 
                                            units = 'weeks')) / 4) %>% 
    round(digits = 0) %>% as.numeric
poll_m_sub <- poll_m_sub %>% select(-poll_month_mean, -month_yearlag1)

poll_monthly <- merge(poll_monthly, poll_m_sub, by = c('party_election', 
                                                       'month_year'))

poll <- left_join(poll, poll_monthly, by = c('party_election', 'month_year'))

poll$monthsbeforeED <- (difftime(poll$elecdate, poll$polldate,
                                 units = 'weeks') / 4) %>% 
    round(digits = 0) %>% as.numeric

# Find coalition parliamentary governments
coal_sub <- poll %>% filter(system == 'Parliamentary') %>%
    select(country, partyid, elecdate, gov_, inc_)
coal_sub <- FindDups(coal_sub, c('country', 'partyid', 'elecdate'), 
                     NotDups = TRUE)

coal_sub <- coal_sub %>% group_by(country, elecdate) %>% 
    summarise(parties_in_gov = sum(gov_))

coal_sub$coalition <- 0
coal_sub$coalition[coal_sub$parties_in_gov > 1] <- 1

# Fix missing party data 
# Needs more work
coal_sub$coalition[coal_sub$country == 'Denmark'] <- 1
coal_sub$coalition[coal_sub$country == 'Germany' & 
                       coal_sub$elecdate == '2013-09-22'] <- 1
coal_sub$coalition[coal_sub$country == 'Ireland' & 
                       coal_sub$elecdate > '2007-01-01'] <- 1

coal_sub <- coal_sub %>% select(-parties_in_gov)

poll <- left_join(poll, coal_sub, by = c('country', 'elecdate'))

# Merge poll and other data based on poll month/year ---------------------------
poll$year <- round_date(poll$polldate, unit = 'year')
poll$quarter <- quarter(poll$polldate, with_year = TRUE)
poll <- left_join(poll, finstress_index, by = c('iso2c', 'month_year'))
poll <- left_join(poll, wdi_crisis, by = c('iso2c', 'year')) 
poll <- left_join(poll, oecd_growth, by = c('iso2c', 'quarter'))
poll <- left_join(poll, oecd_unemployment, by = c('iso2c', 'month_year'))
poll <- poll %>% rename(poll_finstress = FinStress) %>% 
    rename(poll_fs_change12 = fs_change12) %>%
    rename(poll_fs_change_ma12 = fs_change_ma12) %>%
    rename(poll_fs_ma12 = fs_ma12) %>%
    rename(poll_lv_crisis = lv_crisis) %>%
    rename(poll_wdi_gdp_growth = gdp_growth) %>%
    rename(poll_inflation = inflation) %>%
    rename(poll_wdi_unemployment = unemployment) %>%
    rename(poll_oecd_gdp_growth = oecd_gdp_growth) %>%
    rename(poll_oecd_unemployment = oecd_unemployment) %>%
    rename(poll_unemp_change_ma12 = unemp_change_ma12) %>%
    rename(poll_unemp_change12 = unemp_change12) %>%
    rename(poll_unemp_ma12 = unemp_ma12) %>%
    rename(poll_domestic_credit_to_private = domestic_credit_to_private) %>%
    dplyr::select(-month_year, -year, -quarter)

# Merge based on election month/year
poll$month_year <- round_date(poll$elecdate, unit = 'month')
poll$year <- round_date(poll$elecdate, unit = 'year')
poll$quarter <- quarter(poll$polldate, with_year = TRUE)
poll <- left_join(poll, finstress_index, by = c('iso2c', 'month_year'))
poll <- left_join(poll, wdi_crisis, by = c('iso2c', 'year')) 
poll <- left_join(poll, oecd_growth, by = c('iso2c', 'quarter'))
poll <- left_join(poll, oecd_unemployment, by = c('iso2c', 'month_year'))
poll <- poll %>% rename(elect_finstress = FinStress) %>% 
    rename(elect_fs_change12 = fs_change12) %>%
    rename(elect_fs_change_ma12 = fs_change_ma12) %>%
    rename(elect_fs_ma12 = fs_ma12) %>%
    rename(elect_lv_crisis = lv_crisis) %>%
    rename(elect_wdi_gdp_growth = gdp_growth) %>%
    rename(elect_inflation = inflation) %>%
    rename(elect_wdi_unemployment = unemployment) %>%
    rename(elect_oecd_gdp_growth = oecd_gdp_growth) %>%
    rename(elect_oecd_unemployment = oecd_unemployment) %>%
    rename(elect_unemp_change_ma12 = unemp_change_ma12) %>%
    rename(elect_unemp_change12 = unemp_change12) %>%
    rename(elect_unemp_ma12 = unemp_ma12) %>%
    rename(elect_domestic_credit_to_private = domestic_credit_to_private) %>%
    dplyr::select(-month_year -quarter)

# Create new variables ---------------
# Find change in FinStress to election month from the poll month
poll$finstress_election_poll_chng <- poll$elect_finstress - poll$poll_finstress

# Find poll error
poll$poll_error <- poll$vote_ - poll$poll_

# Create variable of whether a part is the largest or not largest party in gov.
poll$not_largest_inc_ <- 0
poll$not_largest_inc_[poll$gov_ == 1 & poll$inc_ == 0] <- 1

poll$in_gov <- 0
poll$in_gov[poll$not_largest_inc_ == 1] <- 1
poll$in_gov[poll$inc_ == 1] <- 2
poll$in_gov <- factor(poll$in_gov, labels = c('Out', 'Junior', 'Senior'))

# Final cleanup
poll <- MoveFront(poll, c('country', 'iso2c', 'countryid', 'partyid', 'polldate', 
                          'elecdate', 'months_since_poll', 'monthsbeforeED',
                          'poll_', 'ipoll_', 
                          'poll_mean_change', 'ipoll_mean_change',
                          'gov_', 'inc_', 'in_gov', 'coalition',
                          'poll_finstress', 'elect_finstress', 
                          'poll_fs_change12', 'elect_fs_change12',
                          'finstress_election_poll_chng', 'poll_lv_crisis',
                          'elect_lv_crisis'))

poll <- poll %>% arrange(country, partyid, polldate)

#FindDups(poll, c('country', 'partyid', 'polldate'))

# Remove imputation numbers
dont_keep <- grep('random', names(poll))
dont_keep <- c(dont_keep, grep('_[1-9][0-9]_', names(poll)))
dont_keep <- c(dont_keep, grep('_[1-9]_', names(poll)))

poll <- poll[, !(1:ncol(poll) %in% dont_keep)]

rmExcept('poll')
# Save for analysis in stata
#export(poll, 'LONG_MI_finstress.csv')

