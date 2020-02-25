# Compute statistics for Tables 1 and 2
# (See `/Volumes/cche-lab/UAthens/Ben/rds-weighting-analysis` for Aditya's last weighting analysis)

rm(list=ls())


# Load libraries ---------------------------

library(RDS)
library(dplyr)


# Read data ---------------------------

data_path <- "../sent_to_Aditya_02092020/"
dyad <- read.csv(paste0(data_path, "HPV_dyad_2019-12-16.csv"))
dt <- read.csv(paste0(data_path, "aditya_hpv_final_v3_attributes_referred_by_final.csv"), as.is = T)


# Data transformations -----

dt$Interview_ID <- dt$caseid #get subject IDs

recruiter.id <- dt$referred_by
recruiter.id[is.na(dt$referred_by)] <- "seed"
recruiter.id <- recode(recruiter.id, "1042" = "seed") #recruiter.ID 1042 does not appear in the case ID
dt$recruiter.id <- recruiter.id


dt$smallnet_w1[which(is.na(dt$smallnet_w1))] <- median(dt$smallnet_w1, na.rm = T) #replace missing value with median

# Create RDS data frame
rds.dt <- as.rds.data.frame(
  dt,
  id = "caseid",
  recruiter.id = "recruiter.id",
  network.size = "smallnet_w1"
)
