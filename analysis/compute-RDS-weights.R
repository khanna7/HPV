# Compute statistics for Tables 1 and 2
# (See `/Volumes/cche-lab/UAthens/Ben/rds-weighting-analysis` for Aditya's last weighting analysis)

rm(list=ls())


# Load libraries ---------------------------

library(RDS)
library(dplyr)


# Load data ---------------------------

load(file="unweighted-desriptives.RData")

# Data transformations -----

dt$Interview_ID <- dt$caseid #get subject IDs

recruiter.id <- dt$referred_by
recruiter.id[is.na(dt$referred_by)] <- "seed"
recruiter.id <- recode(recruiter.id, "1042" = "seed") #recruiter.ID 1042 does not appear in the case ID
dt$recruiter.id <- recruiter.id


dt$smallnet_w1[which(is.na(dt$smallnet_w1))] <- median(dt$smallnet_w1, na.rm = T) #replace missing value with median


# Create RDS data frame -----

rds.dt <- as.rds.data.frame(
  dt,
  id = "caseid",
  recruiter.id = "recruiter.id",
  network.size = "smallnet_w1"
)


# Compute weights -----

gile.wt <- compute.weights(rds.dt, N=10000,
                           weight.type = "Gile's SS")

vh.wt <- compute.weights(rds.dt,
                         weight.type = "RDS-II")


# Assign to data table -----

rds.dt$gile.wt <- gile.wt
rds.dt$vh.wt <- vh.wt


# Save RDS data frames -----

saveRDS(object = rds.dt, file = "rds.dt.RDS")


