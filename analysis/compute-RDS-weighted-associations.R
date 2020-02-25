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
