library(data.table)
library(dplyr)

source('code/helpers/helpers_read_xls.R')
source('code/helpers/helpers_prep_data.R')

# Define Fiscal Year
FY <- 2017

get_budget_est(FY)

save(budget_est, no_main, no_assum,
     file = paste0('data/cache/budget_est_',FY,'.RData'))
