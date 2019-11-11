# Compute descriptive statistics for Table 1

rm(list=ls())

# Load libraries ---------------------------

library(dplyr)


# Read data ---------------------------

dt <- read.csv("/Volumes/akhanna/bulkstorage_projects_bsd_computer/HPV-Chicago-Fujimoto/Aditya_11032019/dataset_used for create_dataset_HPV1 & extract_dyad/houston_hpv.csv", as.is = T)


# Compute descriptive statistics ---------------------------

# age
age_groups <- dt %>% 
  mutate("18-20" = (age_w1 >= 18 & age_w1 < 21),
         "21-25" = (age_w1 >= 21 & age_w1 < 26),
         "26-29" = (age_w1 >= 26 & age_w1 < 30 )) 

table(age_groups[["18-20"]])
table(age_groups[["18-20"]])/nrow(age_groups)   

table(age_groups[["21-25"]])
table(age_groups[["21-25"]])/nrow(age_groups)   

table(age_groups[["26-29"]])
table(age_groups[["26-29"]])/nrow(age_groups)

# race
race.cat <-
  dt %>% 
  select(race.x) %>% 
  pull()

race.cat <- recode(race.cat, 
                   "1" = "Hispanic", 
                   "2" = "White", 
                   "3" = "Black", 
                   "4" = "Other")

table(race.cat)
table(race.cat)/length(race.cat)
