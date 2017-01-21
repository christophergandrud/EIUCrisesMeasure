# ---------------------------------------------------------------------------- #
# Electoral volatility and FinStress
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

devtools::install_github('christophergandrud/SurvSetup')

library(setupPkg)

pkgs <- c('rio', 'tidyverse', 'DataCombine', 'lubridate', 'WDI', 'gridExtra',
          'countrycode', 'SurvSetup', 'repmis', 'countrycode')
library_install(pkgs)

# Set working directory. Change as needed.
possible_dir <- c('/git_repositories/EIUCrisesMeasure/',
                  '~/git_repositories/EIUCrisesMeasure/')
repmis::set_valid_wd(possible_dir)


# Load electoral volatility data ----------------------------------------------- 
# Downloaded from: http://cise.luiss.it/cise/dataset-of-electoral-volatility-and-its-internal-components-in-western-europe-1945-2015/
# May 2016
vol <- import('summary_paper/analysis/other_data/Dataset-of-Electoral-Volatility-and-its-internal-components-in-Western-Europe-1945-2015.xlsx')

# Cleanup
vol <- vol[, 1:7]
vol$Election_date <- vol$Election_date %>% ymd
vol$year_month <- vol$Election_date %>% round_date(unit = 'month')
vol <- vol %>% rename(year = Election_Year)

vol$iso3c <- countrycode(vol$Country, origin = 'country.name', 
                         destination = 'iso3c')
vol <- vol %>% dplyr::select(-Country)


# Load Finstress --------------------------------------------------------
PATH <- 'data/FinStress.csv'
finstress_index <- rio::import(PATH) %>% rename(year_month = date)
finstress_index$year_month <- finstress_index$year_month %>% ymd
finstress_index$year <- finstress_index$year_month %>% year

finstress_index <- subset(finstress_index, iso3c %in% unique(vol$iso3c))

# Find change from previous 6 month moving average
finstress_index <- slideMA(finstress_index, Var = 'FinStress', 
                           GroupVar = 'country', periodBound = -5,
                           NewVar = 'fs_ma6')

finstress_index$fs_change_ma6 <- finstress_index$FinStress - 
                                        finstress_index$fs_ma6

finstress_index <- slideMA(finstress_index, Var = 'FinStress', 
                           GroupVar = 'country', periodBound = -11,
                           NewVar = 'fs_ma12')

finstress_index$fs_change_ma6 <- finstress_index$FinStress - 
    finstress_index$fs_ma6

finstress_index$fs_change_ma12 <- finstress_index$FinStress - 
    finstress_index$fs_ma12

finstress_index <- change(finstress_index, Var = 'FinStress', 
                          GroupVar = 'country', slideBy = -3,
                          NewVar = 'fs_change3', type = 'absolute')

finstress_index <- change(finstress_index, Var = 'FinStress', 
                          GroupVar = 'country', slideBy = -6,
                          NewVar = 'fs_change6', type = 'absolute')

finstress_index <- change(finstress_index, Var = 'FinStress', 
                           GroupVar = 'country', slideBy = -12,
                           NewVar = 'fs_change12', type = 'absolute')

finstress_index <- finstress_index %>% dplyr::select(-year)

# Economic variables from WDI -------------------------------------------
wdi <- WDI(indicator = c('NY.GDP.MKTP.KD.ZG', 'FP.CPI.TOTL.ZG', 
                         'SL.UEM.TOTL.ZS',
                         'GFDD.OI.19', 'GFDD.DI.14'), 
           start = 2000, end = 2014)

wdi$iso3c <- countrycode(wdi$iso2c, origin = 'iso2c', destination = 'iso3c')

wdi <- wdi %>% dplyr::select(-country, -iso3c) %>% 
    rename(gdp_growth = NY.GDP.MKTP.KD.ZG) %>%
    rename(inflation = FP.CPI.TOTL.ZG) %>%
    rename(unemployment = SL.UEM.TOTL.ZS) %>%
    rename(lv_crisis = GFDD.OI.19) %>% 
    rename(domestic_credit_to_private = GFDD.DI.14)

wdi <- DropNA(wdi, 'lv_crisis')


# Create crisis start binary variable
wdi <- wdi %>% group_by(iso3c) %>% mutate(lv_crisis_start = spell_new(lv_crisis))
wdi$lv_crisis_start[wdi$lv_crisis == 0] <- 0
# drop if not at risk
wdi$lv_crisis_start[wdi$lv_crisis == 1 & wdi$lv_crisis_start == 0] <- NA

# Regression data set
reg_data <- merge(finstress_index, vol, by = c('iso3c', 'year_month'))
reg_data <- merge(reg_data, wdi, by = c('iso3c', 'year'))

# LV start-end dates ------------------------------------------------------
lv_se <- import('data/alternative_measures/cleaned/laeven_valencia_start_end.csv')
lv_se$Start <- ymd(lv_se$Start) %>% floor_date(unit = 'year')
lv_se$End <- ymd(lv_se$End) %>% ceiling_date(unit = 'year')
lv_se$year_month <- lv_se$Start
lv_se <- lv_se %>% filter(End >= '2003-01-01')

lv_se$country <- countrycode(lv_se$iso3c, origin = 'iso3c', 
                           destination = 'country.name')
lv_se <- subset(lv_se, country %in% unique(reg_data$country))

lv_se <- merge(lv_se, finstress_index[, -2], by = c('year_month', 'iso3c'), 
               all.x = T)

# Descriptive plot of crisis, election timing ----------------------------------
finstress_included <- subset(finstress_index, 
                             country %in% unique(reg_data$country))

ggplot(finstress_included, aes(year_month, FinStress, group = country)) +
        facet_wrap(~country) +
        geom_line() +
        geom_vline(data = reg_data, aes(xintercept = as.numeric(year_month)),
                   linetype = 'dashed', colour = '#c73017') +
        geom_rect(data = lv_se, aes(xmin = Start, xmax = End,
                                     ymin = -Inf, ymax = Inf),
                  alpha = 0.4, fill = '#D8B70A') +
    
        xlab('') + ylab('FinStress\n') +
        theme_bw()

ggsave('summary_paper/figures/election_timing_crisis_comp.pdf', width = 12,
       height = 7)

# ----------------------------- Parametric Models ---------------------------- #
# FinStress models -------------------------------------------------------------
m1_fs <- lm(TV ~ FinStress, data = reg_data)
m1_fs_poly <- lm(TV ~ FinStress + I(FinStress^2), data = reg_data)
m1_ma <- lm(TV ~ fs_change_ma12, data = reg_data)

# Components of total volatility
m2_fs <- lm(RegV ~ FinStress, data = reg_data)
m3_fs <- lm(AltV ~ FinStress, data = reg_data)

# Plot predicted effects ----------------------------------------------------- #
# Common y-axis limits
common_limits <- c(-1, 30)

# FinStress predictions from linear model ------------------------
predict_fs <- predict(m1_fs, interval = 'confidence') %>% as.data.frame
predict_fs <- cbind(reg_data$FinStress, predict_fs)

plot_fs_linear <- ggplot(predict_fs, aes(`reg_data$FinStress`, fit)) + 
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) +
    scale_y_continuous(limits = common_limits) +
    ylab('Predicted Electoral Volatility\n') + xlab('') +
    ggtitle('Linear: Finstress (level)') +
    theme_bw()

# FinStress predictions from quadradic polynomial model ------------------------
predict_fs_poly <- predict(m1_fs_poly, interval = 'confidence') %>% as.data.frame
predict_fs_poly <- cbind(reg_data$FinStress, predict_fs_poly)

plot_fs_poly <- ggplot(predict_fs_poly, aes(`reg_data$FinStress`, fit)) + 
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) +
    scale_y_continuous(limits = common_limits) +
    ylab('Predicted Electoral Volatility\n') + xlab('') +
    ggtitle('Quadradic: FinStress (level)') +
    theme_bw()

# FinStress change from previous 12 month moving average ----------------
reg_sub <- DropNA(reg_data, 'fs_change_ma12')
predict_fs_ma <- predict(m1_ma, interval = 'confidence') %>% as.data.frame
predict_fs_ma <- cbind(reg_sub$fs_change_ma12, predict_fs_ma)

plot_fs_ma_linear <- ggplot(predict_fs_ma, aes(`reg_sub$fs_change_ma12`, fit)) + 
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) +
    scale_y_continuous(limits = common_limits) +
    ylab('Predicted Electoral Volatility\n') + 
    xlab('') +
    ggtitle('Linear: FinStress (change 12 mnth moving mean)') +
    theme_bw()


# Laeven and Valencia models ---------------------------------------------------
reg_data$lv_crisis <- as.factor(reg_data$lv_crisis)
reg_data$lv_crisis_start <- as.factor(reg_data$lv_crisis_start)

m1_lv <- lm(TV ~ lv_crisis, data = reg_data)
m1_lv_start <- lm(TV ~ lv_crisis_start, data = reg_data)

m2_lv <- lm(RegV ~ lv_crisis, data = reg_data)
m3_lv <- lm(AltV ~ lv_crisis, data = reg_data)

newdata_lv <- data.frame(lv_crisis = as.factor(c(0, 1)))
predict_lv <- predict(m1_lv, interval = 'confidence', newdata = newdata_lv) %>% 
    as.data.frame
predict_lv <- cbind(lv_crisis = c(0, 1), predict_lv)

plot_lv_linear <- ggplot(predict_lv, aes(lv_crisis, fit)) + 
    geom_line() +
    geom_pointrange(aes(ymin = lwr, ymax = upr)) +
    scale_y_continuous(limits = common_limits) +
    scale_x_continuous(breaks = c(0, 1), labels = c('No Crisis', 'Crisis')) +
    xlab('') +
    ylab('Predicted Electoral Volatility\n') + 
    ggtitle('Linear: Laeven & Valencia Banking Crisis') +
    theme_bw()

newdata_lv_start <- data.frame(lv_crisis_start = as.factor(c(0, 1)))
predict_lv_start <- predict(m1_lv_start, interval = 'confidence', 
                      newdata = newdata_lv_start) %>% as.data.frame
predict_lv_start <- cbind(lv_crisis_start = c(0, 1), predict_lv_start)

plot_lv_linear_start <- ggplot(predict_lv_start, aes(lv_crisis_start, fit)) + 
    geom_line() +
    geom_pointrange(aes(ymin = lwr, ymax = upr)) +
    scale_y_continuous(limits = common_limits) +
    scale_x_continuous(breaks = c(0, 1), 
                       labels = c('Not Start Year', 'Start Year')) +
    ylab('Predicted Electoral Volatility\n') + 
    xlab('') +
    ggtitle('Linear: Laeven & Valencia Crisis Start') +
    theme_bw()

pdf(file = 'summary_paper/figures/elect_vol_predict.pdf', width = 10, 
    height = 14)
    grid.arrange(plot_fs_linear, plot_lv_linear, 
                 plot_fs_ma_linear, plot_lv_linear_start,
                 plot_fs_poly, 
                 ncol = 2, nrow = 3)
dev.off()
