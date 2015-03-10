# ---------------------------------------------------------------------------- #
# Gather EIU texts
# Christopher Gandrud
# 10 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

library(RCurl)
library(dplyr)

setwd('~/Desktop/eui_raw/')

base_url <- 'http://nationallizenzen.zbw.eu/handle/10836/'

for (i in 1:22999){
    Sys.sleep(1)
    # Download page to find country-time link
    temp <- paste0(base_url, i)  %>% getURL() %>% as.data.frame()

    # Extract country-time link
    if (grepl('.*?zbwhtml(.*?)html.*', temp[1, 1])){
        temp_sub <- sub('.*?zbwhtml(.*?)html.*', "\\1", temp[1, 1])

        # Create download link and extract county-month-year file name
        new_url <- paste0('http://nationallizenzen.zbw.eu/zbwhtml', temp_sub,
                        'html?page=full')

        temp_file_name <- temp_sub %>%
            gsub(paste0('/10836/', i, '/Country%20Report%20'), '', .) %>%
            gsub('%20', '_', .) %>%
            paste0(., 'html')

        download.file(new_url, temp_file_name)
    }
}
