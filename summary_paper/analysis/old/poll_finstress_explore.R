# ---------------------------------------------------------------------------- #
# The impact of FinStress changes on vote intention changes
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(rio)
library(DataCombine)
library(dplyr)
library(lubridate)
library(plotMElm)
library(ggplot2)
library(gridExtra)
library(visreg)
library(stargazer)


# Set working directory
setwd('/Volumes/SAMSUNG128/data/Jennings_Wlezien_2016/')

# Import merged data set
poll_full <- import('LONG_MI_finstress.csv')

# Relevel
poll_full$in_gov <- factor(poll_full$in_gov, 
                           levels = c('Out', 'Junior', 'Senior'))

# Subsamples -----------------
# Subset to only be 14 months before the election
poll_sub <- subset(poll_full, monthsbeforeED <= 14)

# Polls within 1 month of last poll
poll_sub <- subset(poll_sub, months_since_poll <= 1)

# Government type sub-samples
poll_parl <- subset(poll_sub, system == 'Parliamentary')
poll_parl_coal <- subset(poll_sub, system == 'Parliamentary' & 
                             coalition == 1)
poll_parl_not_coal <- subset(poll_sub, system == 'Parliamentary' & 
                                 coalition == 0)

# Estimate poll error Models-------------------------------------------------- #
# Effect of stress change, being in gov. participation type on poll change -----
## No interaction
poll_change_gov <- poll_mean_change ~ poll_fs_change_ma12 +
                        poll_oecd_gdp_growth + gov_ +
                        monthsbeforeED +
                        as.factor(parXcou_)

mc_1 <- lm(poll_change_gov, data = poll_parl)

## Interaction
poll_change_gov_in <- poll_mean_change ~ poll_fs_change_ma12 * gov_ + 
                        poll_oecd_gdp_growth +
                        monthsbeforeED +
                        as.factor(parXcou_)
poll_change_gdp <- poll_mean_change ~ poll_fs_change_ma12 * gov_ + 
                        poll_oecd_gdp_growth * gov_ + 
                        monthsbeforeED +
                        as.factor(parXcou_)

# All parliaments, only FinStress interaction
mc_2 <- lm(poll_change_gov_in, data = poll_parl)

# All parliaments, FinStress and GDP growth interaction
mc_2_1 <- lm(poll_change_gdp, data = poll_parl)

pmc_2_1_1 <- plot_me(mc_1_2, 'poll_oecd_gdp_growth', 'gov_') +
    ggtitle('All Parliamentary') +
    scale_x_continuous(breaks = c(0, 1), labels = c('No', 'Yes')) +
    #scale_y_continuous(limits = parl_limits) +
    xlab('Incumbent Governing Party?') + 
    ylab('Marginal Effect of GDP Growth (qt) at Poll Time\non Party Poll Change\n')
pmc_2_1_2 <- plot_me(mc_1_2, 'poll_fs_change_ma12', 'gov_') +
    ggtitle('All Parliamentary') +
    scale_x_continuous(breaks = c(0, 1), labels = c('No', 'Yes')) +
    #scale_y_continuous(limits = parl_limits) +
    xlab('Incumbent Governing Party?') + 
    ylab('Marginal Effect of FinStress Change at Poll Time\non Party Poll Change\n')

pdf(file = 'figures/me_growth_finstress.pdf', width = 10, height = 5)
    grid.arrange(pmc_2_1_1, pmc_2_1_2, nrow = 1)
dev.off()

mc_3 <- lm(poll_change_gdp, data = poll_parl_coal)
mc_4 <- lm(poll_change_gdp, data = poll_parl_not_coal)

# Plot marginal effects
parl_limits <- c(-1.5, 1.5)
pmc_2 <- plot_me(mc_2, 'poll_fs_change_ma12', 'gov_') + 
    ggtitle('All Parliamentary Goverments') +
    scale_x_continuous(breaks = c(0, 1), labels = c('No', 'Yes')) +
    #scale_y_continuous(limits = parl_limits) +
    xlab('Incumbent Governing Party?') + 
    ylab('Marginal Effect of FinStress Change at Poll Time\non Party Poll Change\n')
pmc_3 <- plot_me(mc_3, 'poll_fs_change_ma12', 'gov_') + 
    ggtitle('Parliamentary (coalition gov.)') +
    scale_x_continuous(breaks = c(0, 1), labels = c('No', 'Yes')) +
   # scale_y_continuous(limits = parl_limits) +
    xlab('Incumbent Governing Party?') + 
    ylab('Marginal Effect of FinStress Change at Poll Time\non Party Poll Change\n')
pmc_4 <- plot_me(mc_4, 'poll_fs_change_ma12', 'gov_') + 
    ggtitle('Parliamentary (single party)') +
    scale_x_continuous(breaks = c(0, 1), labels = c('No', 'Yes')) +
   # scale_y_continuous(limits = parl_limits) +
    xlab('Incumbent Governing Party?') + 
    ylab('Marginal Effect of FinStress Change at Poll Time\non Party Poll Change\n')

grid.arrange(pmc_2, pmc_3, pmc_4, nrow = 1, ncol = 3)

# Effect of stress change, being in gov. participation type on poll change -----
poll_change_in <- poll_mean_change ~ poll_fs_change_ma12 * in_gov + 
                    poll_oecd_gdp_growth * in_gov +
                    monthsbeforeED + as.factor(parXcou_)

mcT_1 <- lm(poll_change_in, data = poll_parl_coal)

pmcT_1 <- plot_me(mcT_1, 'poll_oecd_gdp_growth', 'in_gov') + 
    ggtitle('Parliamentary (Coalitions Only)') +
    xlab('\nCoalition Status vs. Not in Coalition') + 
    ylab('Marginal Effect of GDP Growth (qt) at Poll Time\non Party Poll Change\n')

pmcT_2 <- plot_me(mcT_1, 'poll_fs_change_ma12', 'in_gov') + 
            ggtitle('Parliamentary (Coalitions Only)') +
    xlab('\nCoalition Status vs. Not in Coalition') + 
    ylab('Marginal Effect of FinStress Change at Poll Time\non Party Poll Change\n')

pdf(file = 'figures/me_poll_overall.pdf', width = 10, height = 10)
    grid.arrange(pmc_2_1_1, pmc_2_1_2, pmcT_1, pmcT_2, nrow = 2, ncol = 2)
dev.off()
    
### 3-way interaction
poll_change_in3 <- poll_mean_change ~ 
                        poll_fs_change_ma12 * in_gov * monthsbeforeED +
                        poll_oecd_gdp_growth * in_gov * monthsbeforeED +
                        + as.factor(parXcou_) 

mcT_2 <- lm(poll_change_in3, data = poll_parl_coal)

pdf(file = 'figures/predicted_coal_time.pdf', height = 5, width = 10)
par(mfrow = c(1, 2))
visreg2d(mcT_2, 'monthsbeforeED', 'poll_fs_change_ma12', 
         cond = list(in_gov = 'Junior'), plot.type = "persp", 
         main = 'Junior Partner', xlab = 'Months Before Election',
         ylab = '\nFinStress Change', zlab = '\nPoll Change',
         zlim = c(-1, 1.3)
         )
visreg2d(mcT_2, 'monthsbeforeED', 'poll_fs_change_ma12', 
         cond = list(in_gov = 'Senior'), plot.type = "persp", 
         main = 'Senior Partner', xlab = 'Months Before Election',
         ylab = '\nFinStress Change', zlab = '\nPoll Change',
        zlim = c(-1, 1.3)
    )
dev.off()


# Results table
cov_labels <- c('FinStress $\\Delta$', 
                'Coalition: Junior', 'Coalition: Senior', 
                'GDP Growth (qt)', 
                'In Gov.', 
                'Months Before Elect.', 
                'FinStress $\\Delta$ * In Gov.', 
                'GDP Growth (qt) * In Gov.', 
                'FinStress $\\Delta$ * Junior',
                'FinStress $\\Delta$ * Senior',
                'FinStress $\\Delta$ * Months Before',
                'Junior * Months Before',
                'Senior * Months Before',
                'Junior * GDP Growth (qt)',
                'Senior * GDP Growth (qt)',
                'GDP Growth (qt) * Months Before',
                'FinStress $\\Delta$ * Junior * Months Before',
                'FinStress $\\Delta$ * Senior * Months Before',
                'GDP Growth (qt) * Junior * Months Before',
                'GDP Growth (qt) * Senior * Months Before',
                'Constant'
                )

stargazer(mc_1, mc_2, mc_2_1, mc_4, mc_3, mcT_1,  mcT_2,
    type ='latex',
  #  header = FALSE,
    dep.var.labels = 'Mean Party 1-Month Poll $\\Delta$',
    covariate.labels = cov_labels,
    column.labels = c('All Parl.', 'All Parl.', 'All Parl.', 
                      'Single Party Parl.', 
                      'Coalition', 'Coalition', 'Coalition'),
    omit = 'as.factor',
    omit.stat = 'f',
    omit.labels = 'Party FE',
    title = 'Estimating Party Vote Intention Poll Changes',
    label = 'poll_models',
    font.size = 'tiny',
    out = 'tables/poll_results.tex'
)











# Effect of stress change, being in gov. participation type on poll change -----
poll_change_gov3 <- poll_mean_change ~ 
    poll_fs_change_ma12 * gov_ * monthsbeforeED +
    poll_oecd_gdp_growth + as.factor(parXcou_)

mc_3 <- lm(poll_change_gov3, data = poll_parl_not_coal)
visreg2d(mc_3, 'monthsbeforeED', 'poll_fs_change_ma12', 
         cond = list(gov_ = 1), plot.type = "persp", 
         main = 'In Single-Party Government', xlab = 'Months Before Election',
         ylab = '\nFinStress Change', zlab = '\nPoll Change from Previous Month')
visreg2d(mc_3, 'monthsbeforeED', 'poll_fs_change_ma12', 
         cond = list(gov_ = 0), plot.type = "persp", 
         main = 'Out of Government', xlab = 'Months Before Election',
         ylab = '\nFinStress Change', zlab = '\nPoll Change from Previous Month')

#mc_4 <- lm(poll_change_gov3, data = poll_pres)
#    visreg2d(mc_4, 'monthsbeforeED', 'poll_fs_change12', 
#         cond = list(gov_ = 1), plot.type = "persp", main = 'In Government')




# FinStress Level at Election---------------------------------------------------
# Non-interactive model
ml_1 <- lm(poll_error ~ elect_finstress + gov_ + daysbeforeED + 
               as.factor(parXcou_), data = poll_sub)

# Interact FinStress and party in government status
ml_2 <- lm(poll_error ~ elect_finstress * gov_ + daysbeforeED + 
               elect_gdp_growth + elect_unemployment +
               as.factor(parXcou_), data = poll_sub)


fl1 <- plot_me(ml_2, 'elect_finstress', 'gov_') + 
   # scale_y_continuous(limits = c(-0.42, 0.23)) +
    scale_x_continuous(breaks = c(0, 1), labels = c('No', 'Yes')) +
    xlab('Incumbent Governing Party?') + 
    ylab('Marginal Effect of FinStress Level at ElectionTime\non Party Vote Gains from Poll\n')

# Interact FinStress level and party in government status (junior or largest member)
ml_3 <- lm(poll_error ~ elect_finstress * in_gov + daysbeforeED + 
               elect_gdp_growth + elect_unemployment +
               as.factor(parXcou_), data = poll_sub)

fl2 <- plot_me(ml_3, 'elect_finstress', 'in_gov') +
    xlab('Government Party Status') + 
    ylab('Marginal Effect of FinStress Level at Election Time\non Party Vote Gains from Poll\n')

# Election occurs in an L-V crisis year ----------------------------------------
mlv_1 <- lm(poll_error ~ elect_lv_crisis * gov_ + daysbeforeED + 
                as.factor(parXcou_), data = poll_sub)

lv1 <- plot_me(mlv_1, 'elect_lv_crisis', 'gov_') +
    scale_x_continuous(breaks = c(0, 1), labels = c('No', 'Yes')) +
    xlab('Incumbent Governing Party?') + 
    ylab('Marginal Effect of L-V Crisis Year\non Party Vote Gains from Poll\n')

mlv_2 <- lm(poll_error ~ elect_lv_crisis * in_gov + daysbeforeED + 
                elect_gdp_growth + elect_unemployment +
                as.factor(parXcou_), data = poll_sub)

lv2 <- plot_me(mlv_2, 'elect_lv_crisis', 'in_gov') +
    xlab('Government Party Status') + 
    ylab('Marginal Effect of L-V Crisis Year\non Party Vote Gains from Poll\n')

grid.arrange(fl1, fl2,
    lv1, lv2, nrow = 2, ncol = 2)

