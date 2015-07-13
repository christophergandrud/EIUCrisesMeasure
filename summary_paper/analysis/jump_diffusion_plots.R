# ---------------------------------------------------------------------------- #
# Find nonparametric jump and diffusion parameters. Compare to LV and RR
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load required packages
library(repmis)
library(rio)
library(dplyr)
library(countrycode)
library(lubridate)
library(tidyr)
library(earlywarnings)
library(ggplot2)
library(gridExtra)
library(WDI)


# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)

# Function to rescale between 0 and 1
range01 <- function(x){(x - min(x))/(max(x) - min(x))}

# Find diffusion and jump for one country
sub_ddj <- function(x) {
    comb <- data.frame()
    for (i in unique(x$country)) {
        message(i)
        temp <- subset(x, country == i)
        temp_sub <- temp[-1, 'C1_ma'] %>% as.data.frame
        temp_results <- ddjnonparam_ews(temp_sub, logtransform = T)

        if (length(temp_results$Diff2.t) == length(temp[-1, 'date'])) {
        temp_comb <- data.frame(country = i,
                           date = temp[-1, 'date'],
                           diffusion = temp_results$Diff2.t,
                           jump = temp_results$Lamda.t,
                           total_variance = temp_results$TotVar.t)
        comb <- rbind(comb, temp_comb)
        }
    }
    return(comb)
}

#### Load KPCA results ####
kpca <- import('data/results_kpca_rescaled.csv')
kpca$date <- ymd(kpca$date)

# Remove problem countries
dj_kpca <- sub_ddj(kpca)

comb_continuous <- gather(dj_kpca, jump_diffusion, value, 3:5)

## Load Reinhart and Rogoff
source('data/alternative_measures/reinhart_rogoff.R')
rr_bc <- rr_bc %>% filter(RR_BankingCrisis_start >= '2003-01-01')

rr_bc_start <- rr_bc %>% select(iso2c, RR_BankingCrisis_start) %>%
                rename(start = RR_BankingCrisis_start)
rr_bc_end <- rr_bc %>% select(RR_BankingCrisis_end) %>%
                rename(end = RR_BankingCrisis_end)

rr_bc <- cbind(rr_bc_start, rr_bc_end)
rr_bc$Source <- 'Reinhart/Rogoff'

## Laeven and Valencia
lv_se <- import('data/alternative_measures/cleaned/laeven_valencia_start_end.csv')
lv_se$Start <- ymd(lv_se$Start)
lv_se$End <- ymd(lv_se$End)
lv_se <- lv_se %>% filter(Start >= '2003-01-01')

lv_se_start <- lv_se %>% select(iso2c, Start) %>%
            rename(start = Start)
lv_se_end <- lv_se %>% select(End) %>%
                rename(end = End)

lv_se <- cbind(lv_se_start, lv_se_end)
lv_se$Source <- 'Laeven/Valencia'

comb_se <- rbind(rr_bc, lv_se)

comb_se$country <- countrycode(comb_se$iso2c, origin = 'iso2c',
                                 destination = 'country.name')

#### Compare to LV ####
compare_to_dummy <- function(data_cont, data_dummy, id, 
                             jd = 'diffusion') {
    temp_cont <- subset(data_cont, country == id)
    temp_cont <- subset(temp_cont, jump_diffusion == jd)
    temp_dummy <- subset(data_dummy, country == id)

    if (nrow(temp_dummy) == 0) {
        ggplot(data = temp_cont, aes(date, value)) +
            geom_line(alpha = 0.6) +
            scale_linetype_manual(values = c('solid', 'dashed', 'dotted')) +
            ggtitle(id) + xlab('') + ylab(sprintf('%s\n', jd)) +
            theme_bw() +
            theme(legend.position = "none")
    } else if (nrow(temp_dummy)) {
        ggplot() +
            geom_line(data = temp_cont, aes(date, value), alpha = 0.6) +
            geom_rect(data = temp_dummy, aes(xmin = start, xmax = end,
                                             ymin = -Inf, ymax = Inf,
                                             fill = Source),
                   alpha = 0.4) +
            scale_fill_manual(values = c("#D8B70A", "#972D15")) +
            scale_linetype_manual(values = c('solid', 'dashed', 'dotted')) +
            ggtitle(id) + xlab('') + ylab(sprintf('%s\n', jd)) +
            theme_bw() +
            theme(legend.position = "none")
    }
}

country_vector <- unique(kpca$country)
kpca_list <- list()
for (i in country_vector) {
 message(i)
 kpca_list[[i]] <- suppressMessages(
         compare_to_dummy(data_cont = comb_continuous,
                          data_dummy = comb_se,
                          id = i, jd = 'jump'))
}

# Plot selection (1)
select_countries_1 <- c('Argentina', 'Australia', 'Austria', 'Belgium',
                      'Brazil','Bulgaria', 'Canada', 'China',
                      'Czech Republic', 'Denmark', 'Estonia', 'France',
                      'Germany', 'Greece','Hungary', 'Iceland',
                      'India', 'Ireland', 'Italy', 'Japan'
                      )

select_countries_2 <- c('Kazakhstan', 'Latvia', 'Lithuania', 'Luxembourg',
                        'Netherlands', 'Nigeria', 'Portugal', 'Russian Federation',
                        'Singapore', 'Slovenia', 'South Africa', 'Spain',
                        'Switzerland', 'Ukraine', 'United Kingdom', 'United States'
                        )

do.call(grid.arrange, kpca_list[select_countries_1])

do.call(grid.arrange, kpca_list[select_countries_2])

# ---------------------------------------------------------------------------- # 
#### Compare distributions of DDJ parameters for crisis and non-crisis      ####

dj_kpca$iso2c <- countrycode(dj_kpca$country, origin = 'country.name',
                             destination = 'iso2c')
dj_kpca$year <- year(dj_kpca$date)

## Laeven and Valencia
lv <- import('data/alternative_measures/cleaned/laeven_valencia_banking_crisis.csv')

comb <- merge(dj_kpca, lv, by = c('iso2c', 'year'))

#### Kolmogorov-Smirnov test of equality of distributions in crisis and non-crisis
crisis <- comb %>% filter(lv_bank_crisis == 1)
non_crisis <- comb %>% filter(lv_bank_crisis == 0)

ks.test(crisis$diffusion, non_crisis$diffusion, alternative = 'less')
ks.test(crisis$jump, non_crisis$jump, alternative = 'less')
ks.test(crisis$total_variance, non_crisis$total_variance, 
        alternative = 'greater')

#### Plot density comparisions ####
comb_gathered <- comb %>% gather(measure, value, 5:7)
comb_gathered$lv_bank_crisis <- factor(comb_gathered$lv_bank_crisis, 
                                       labels = c('No Crisis', 'Crisis'))
comb_gathered$measure <- factor(comb_gathered$measure, 
                                        levels = c('jump', 'diffusion', 
                                                   'total_variance'),
                                        labels = c('Jump', 'Diffusion', 
                                                  'Total Variance'))

ggplot(comb_gathered, aes(value, colour = lv_bank_crisis)) +
    geom_density(aes(linetype = lv_bank_crisis)) +
    facet_wrap(~measure, scales = "free", ncol = 1) +
    scale_colour_manual(values = c("#D8B70A", "#972D15")) +
    ylab('Density\n') + xlab('') +
    theme_bw() +
    guides(color = guide_legend(title = 'Laeven/Valencia'),
           linetype = guide_legend(title = 'Laeven/Valencia'))

ggsave('summary_paper/analysis/figures/compare_jump_diffusion_basic.pdf')

#### Compare with WDI income groups
# Gather and clean income groupd data
income <- WDI(indicator = 'NY.GDP.MKTP.KD', extra = T, start = 2003)

income <- income %>% filter(income != 'Aggregates')
income <- income %>% filter(!is.na(income$income))

income$high_income <- 0
income$high_income[income$income == 'High income: OECD'] <- 1
income$high_income[income$income == 'High income: nonOECD'] <- 1

income <- income %>% select(iso2c, year, high_income) %>% arrange(iso2c, year)

# Merge
comb_income <- merge(comb_gathered, income, by = c('iso2c', 'year'))


temp_sub <- comb_income %>% filter(measure == 'Diffusion')
ggplot(temp_sub, aes(value, color = lv_bank_crisis)) +
    geom_density() +
    scale_colour_manual(values = c("#D8B70A", "#972D15")) +
    facet_wrap(~high_income, scales = 'free') +
    theme_bw()
