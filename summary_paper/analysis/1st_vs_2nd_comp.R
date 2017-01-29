# ----------------------------------------------------------------------------
# Compare cleaned FinStress to similarly cleaned 2nd component
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

pkgs <- c('ggplot2', 'tidyr', 'rio', 'lubridate')
library_install()
theme_set(theme_bw())

# Set working directory
possibles <- c("/git_repositories/EIUCrisesMeasure/",
               "~/git_repositories/EIUCrisesMeasure/")

set_valid_wd(possibles)


# Load data
finstress <- import("data/FinStress.csv") %>% select(-iso3c)
c2 <- import("data/kpca_5_c2_cleaned.csv") %>% select(-iso3c)

comb <- merge(finstress, c2, by = c('country', 'date'))

comb <- gather(comb, component, value, 3:4)

comb$date <- ymd(comb$date)

comb$component <- factor(comb$component,
                         levels = c("FinStress", "c2_finstress_clean"),
                         labels = c('1st Component\n(FinStress)', '2nd Component'))


# Plot for set of countries examined in the main text
select_countries_short <- c('Argentina', 'Australia', 'Brazil',
    'Canada', 'China',  'France',
    'Germany', 'Greece','Hungary', 'Iceland',
    'India', 'Ireland', 'Italy', 'Japan',
    'Kazakhstan', 'Netherlands', 'Spain',
    'Switzerland', 'United Kingdom', 'United States'
)

comb <- subset(comb, country %in% select_countries_short)

ggplot(comb, aes(date, value, group = component, linetype = component)) +
    facet_wrap(~country) +
    scale_linetype_discrete(name = '') +
    xlab('') + ylab('') +
    geom_line()

ggsave('summary_paper/figures/1st_vs_2nd_component_selected.pdf',
       width = 15, height = 15)

