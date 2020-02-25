# Compute statistics for Table 2

# (See `bivariate_analysis_n623_wsvydesign.R`
   #in `/project2/khanna7/Projects/UConnect/UConnect_PrEP/Regressions` 
   #for Aditya's last weighting analysis)

rm(list=ls())


# Load libraries ---------------------------

library(survey)
library(dplyr)


# Read data ---------------------------

load(file="unweighted-desriptives.RData")
rds.dt <- as.data.frame(readRDS("rds.dt.RDS"))


# Compute design matrix ---------------------------

dt.svydesign <- svydesign(data = rds.dt,
                          ids = ~caseid,
                          weights = ~gile.wt)


# Compute weighted summaries (full sample) ---------------------------

svymean(x = ~age_w1, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_hpv_any, na.rm = TRUE, design = dt.svydesign)
svymean(x=~num_hr_hpv, na.rm = TRUE, design = dt.svydesign)
svymean(x=~mult_hr_type, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hpv_16_and_18, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hpv_16_or_18, na.rm = TRUE, design = dt.svydesign)
svymean(x=~nine.val.vac, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_16, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_18, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_31, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_33, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_35, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_39, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_45, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_51, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_52, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_58, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_59, na.rm = TRUE, design = dt.svydesign)
svymean(x=~hr_68, na.rm = TRUE, design = dt.svydesign)
