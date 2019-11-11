# Compute descriptive statistics for Table 1

rm(list=ls())


# Load libraries ---------------------------

library(dplyr)


# Read data ---------------------------

dt <- read.csv("/Volumes/home/bulkstorage_projects_bsd_computer/HPV-Chicago-Fujimoto/Aditya_11032019/dataset_used for create_dataset_HPV1 & extract_dyad/houston_hpv.csv", as.is = T)


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

# sexual identity
sex.id <-
  dt %>% 
  select(sexual_identity_w1) %>% 
  pull()

sex.id.cat <- 
  recode(sex.id,
         "1" = "Gay",
         "2" = "Straight (or heterosexual)",
         "3" = "Bisexual",
         "4" = "Other"
  )

table(sex.id.cat)
table(sex.id.cat)/length(sex.id.cat)

## education
educ <- 
  dt %>% 
  select(education_w1) %>% 
  pull()

educ.cat <- 
  recode(educ,
         "1" = "Grade K-12",
         "2" = "High School or GED",
         "3" = "High School or GED",
         "4" = "High School or GED",
         "5" = "High School or GED",
         "6" = "High School or GED",
  )

table(educ.cat)
table(educ.cat)/length(educ.cat)

## homeless
table(dt$past12m_homeless_w1, exclude = NULL)
table(dt$past12m_homeless_w1, exclude = NULL)/length(dt$past12m_homeless_w1)

##  Number of sex partners where ego's position is receptive and inconsistent condom use with alters
table(dt$num_condomless_anal_sex_receptive_w1, exclude = NULL)

length(which(dt$num_condomless_anal_sex_receptive_w1 < 2))
length(which(dt$num_condomless_anal_sex_receptive_w1 < 2))/nrow(dt)

length(which(dt$num_condomless_anal_sex_receptive_w1 >= 2))
length(which(dt$num_condomless_anal_sex_receptive_w1 >= 2))/nrow(dt)

## hiv status
table(dt$hiv_w1, exclude = NULL)
table(dt$hiv_w1, exclude = NULL)/nrow(dt)

## syphilis (fta)
table(dt$fta_w1, exclude = NULL)
table(dt$fta_w1, exclude = NULL)/nrow(dt)
