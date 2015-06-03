library(repmis)
library(rio)
library(dplyr)
library(lubridate)
library(earlywarnings)

#### Download KPCA results ####
# Set working directory of kpca project. Change as needed.
pos_directs <- c('~/git_repositories/EIUCrisesMeasure/',
                 '/git_repositories/EIUCrisesMeasure/')

set_valid_wd(pos_directs)
kpca <- import('data/results_kpca_rescaled.csv')
kpca$date <- ymd(kpca$date)

# Find diffusion and jump for one country
sub_ddj <- function(data, id) {
    temp <- subset(data, country == id) 
    temp_sub <- temp[-1, 'C1_ma'] %>% as.data.frame
    results <- ddjnonparam_ews(temp_sub, logtransform = T)
    
    comb <- data.frame(country = id, 
                       date = temp[-1, 'date'], 
                       diffusion = results$Diff2.t,
                       jump = results$Lamda.t)
    return(comb)
}

uk <- sub_ddj(kpca, 'United Kingdom')


sub_ch <- function(data, id) {
    temp <- subset(data, country == id) 
    temp_sub <- temp[-1, 'C1_ma'] %>% as.data.frame
    results <- ch_ews(temp_sub)
}
