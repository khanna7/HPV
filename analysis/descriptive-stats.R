# Compute statistics for Tables 1 and 2
# (See `/Volumes/cche-lab/UAthens/Ben/rds-weighting-analysis` for Aditya's last weighting analysis)

rm(list=ls())


# Load libraries ---------------------------

library(dplyr)


# Read data ---------------------------

data_path <- "../sent_to_Aditya_02092020/"
dyad <- read.csv(paste0(data_path, "HPV_dyad_2019-12-16.csv"))
dt <- read.csv(paste0(data_path, "aditya_hpv_final_v3_attributes_referred_by_final.csv"), as.is = T)


#complete path name: 
 #/Volumes/akhanna/bulkstorage_projects_bsd_computer/HPV-Chicago-Fujimoto/
 # Aditya_11032019/dataset_used for create_dataset_HPV1 & extract_dyad/houston_hpv.csv
                    
# specifying the whole path leads to errors in reading (likely because of the network connection)


# Compute descriptive statistics ---------------------------

# age
age_groups <- dt %>% 
  mutate("18-20" = (age_w1 >= 18 & age_w1 < 21),
         "21-25" = (age_w1 >= 21 & age_w1 < 26),
         "26-29" = (age_w1 >= 26 & age_w1 < 30 )) 

table(age_groups[["18-20"]], exclude = NULL)
table(age_groups[["18-20"]], exclude = NULL)/nrow(age_groups)   

table(age_groups[["21-25"]], exclude = NULL)
table(age_groups[["21-25"]], exclude = NULL)/nrow(age_groups)   

table(age_groups[["26-29"]], exclude = NULL)
table(age_groups[["26-29"]], exclude = NULL)/nrow(age_groups)

# race
race.cat <-
  dt %>% 
  select(race) %>% 
  pull()

race.cat <- recode(race.cat, 
                   "1" = "Hispanic", 
                   "2" = "White", 
                   "3" = "Black", 
                   "4" = "Other")

table(race.cat, exclude = NULL)
table(race.cat, exclude = NULL)/length(race.cat)

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

table(sex.id.cat, exclude = NULL)
table(sex.id.cat, exclude = NULL)/length(sex.id.cat)

# education
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

table(educ.cat, exclude = NULL)
table(educ.cat, exclude = NULL)/length(educ.cat)

# homeless
table(dt$past12m_homeless_w1, exclude = NULL)
table(dt$past12m_homeless_w1, exclude = NULL)/length(dt$past12m_homeless_w1)

#  Number of sex partners where ego's position is receptive 
table(dt$num_anal_sex_receptive_2_w1, exclude = NULL)

length(which(dt$num_anal_sex_receptive_2_w1 < 2))
length(which(dt$num_anal_sex_receptive_2_w1 < 2))/nrow(dt)

length(which(dt$num_anal_sex_receptive_2_w1 >= 2))
length(which(dt$num_anal_sex_receptive_2_w1 >= 2))/nrow(dt)

# hiv status
table(dt$hiv_w1, exclude = NULL)
table(dt$hiv_w1, exclude = NULL)/nrow(dt)

# syphilis (fta)
table(dt$fta_w1, exclude = NULL)
table(dt$fta_w1, exclude = NULL)/nrow(dt)

# high risk hpv
## define new hr_hpv_any = 1 if any of the following are = 1
dt <-
  dt %>% 
  mutate(
    hr_hpv_any = if_else(
      (
        hr_16 == 1 | 
          hr_18 == 1 |
          hr_31 == 1 |                                     
          hr_33 == 1 |                                       
          hr_35 == 1 |                                     
          hr_39 == 1 |                                       
          hr_45 == 1 |                                     
          hr_51 == 1 |                                       
          hr_52 == 1 |                                     
          hr_56 == 1 |                                       
          hr_58 == 1 |                                       
          hr_59 == 1 |                                     
          hr_68 == 1
       ), 
      1, 0)
    )
table(dt[["hr_hpv_any"]], exclude=NULL)
  
# xtab HIV-infection with hr HPV
xtabs(~factor(hr_hpv_any, exclude = NULL)+
        factor(hiv_w1, exclude = NULL), data=dt)

# compute number of high risk types

dt <-
  dt %>% 
  mutate(
    num_hr_hpv = 
      hr_16 + 
      hr_18 +
      hr_31+                                     
      hr_33+                                       
      hr_35+                                     
      hr_39+                                       
      hr_45+                                     
      hr_51+                                       
      hr_52+                                     
      hr_56+                                       
      hr_58+                                       
      hr_59+                                     
      hr_68 
  )
summary(dt$num_hr_hpv)

# compute number of high risk types for HIV+ and HIV-

dt %>% #HIV-infected
  filter(hiv_w1 == 1) %>%
  summary(num_hr_hpv) 

dt %>% #HIV-uninfected
  filter(hiv_w1 == 0) %>%
  summary(num_hr_hpv) 

# multiple hr types: summary

dt <-
  dt %>% 
  mutate(
    mult_hr_type = if_else(num_hr_hpv > 1, 1, 0)
  )
table(dt$mult_hr_type, exclude = NULL)

dt %>% # by HIV status
  filter(mult_hr_type == 1) %>%
  group_by(hiv_w1) %>%
  summarise(n=n()) 

# both HPV-16 and HPV-18

dt <-
  dt %>% 
  mutate(
    hpv_16_and_18 = if_else((hr_16 == 1 & hr_18 == 1), 1, 0)
  )
table(dt$hpv_16_and_18, exclude=NULL)

dt %>% # by HIV status
  filter(hpv_16_and_18 == 1) %>%
  group_by(hiv_w1) %>%
  summarise(n=n()) 

# either hpv-16 or hpv-18

dt <-
  dt %>% 
  mutate(
    hpv_16_or_18 = if_else((hr_16 == 1 | hr_18 == 1), 1, 0)
  )
table(dt$hpv_16_or_18, exclude=NULL)

dt %>% # by HIV status
  filter(hpv_16_or_18 == 1) %>%
  group_by(hiv_w1) %>%
  summarise(n=n()) 

# 9-valent vaccine type: 6, 11, 16, 18, 31, 33, 45, 52, and 58;

dt <-
  dt %>% 
  mutate(
    nine.val.vac = if_else(
      (
        #hr_6 == 1 | 
          lr_11 == 1 |
          hr_16 == 1 |                                     
          hr_18 == 1 |                                       
          hr_31 == 1 |                                     
          hr_33 == 1 |                                       
          hr_45 == 1 |                                     
          hr_52 == 1 |                                       
          hr_58 == 1 
      ), 
      1, 0)
  )
table(dt$nine.val.vac, exclude = NULL)

dt %>% # by HIV status
  filter(nine.val.vac == 1) %>%
  group_by(hiv_w1) %>%
  summarise(n=n()) 

# prevalence of HPV types (any and high risk)

## any type
dt %>% 
  filter(any_type == 1) %>% 
  summarise(n=n())

dt %>% 
  filter(any_type == 1) %>% 
  group_by(hiv_w1) %>%
  summarise(n=n()) 

## hr 16
dt %>% 
  filter(hr_16 == 1) %>%
  summarise(n=n()) %>%
  mutate(prev = n/nrow(dt))
  
dt %>% 
  filter(hr_16 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n()) 


## hr 18
dt %>% 
  filter(hr_18 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_18 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n()) 

## hr 31
dt %>% 
  filter(hr_31 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_31 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n()) 

## hr 33
dt %>% 
  filter(hr_33 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_33 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 35
dt %>% 
  filter(hr_35 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_35 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 39
dt %>% 
  filter(hr_39 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_39 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 45
dt %>% 
  filter(hr_45 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_45 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 51
dt %>% 
  filter(hr_51 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_51 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 52
dt %>% 
  filter(hr_52 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_52 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 58
dt %>% 
  filter(hr_58 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_58 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 59
dt %>% 
  filter(hr_59 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_59 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## hr 68
dt %>% 
  filter(hr_68 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(hr_68 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())


# Save image ---------------------------

save.image(file="unweighted-desriptives.RData")
