# Compute statistics for Tables 1 and 2

rm(list=ls())


# Load libraries ---------------------------

library(dplyr)


# Read data ---------------------------

dt <- read.csv("../Aditya_11032019/dataset_used for create_dataset_HPV1 & extract_dyad/houston_hpv.csv", as.is = T)

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

table(educ.cat)
table(educ.cat)/length(educ.cat)

# homeless
table(dt$past12m_homeless_w1, exclude = NULL)
table(dt$past12m_homeless_w1, exclude = NULL)/length(dt$past12m_homeless_w1)

#  Number of sex partners where ego's position is receptive and inconsistent condom use with alters
table(dt$num_condomless_anal_sex_receptive_w1, exclude = NULL)

length(which(dt$num_condomless_anal_sex_receptive_w1 < 2))
length(which(dt$num_condomless_anal_sex_receptive_w1 < 2))/nrow(dt)

length(which(dt$num_condomless_anal_sex_receptive_w1 >= 2))
length(which(dt$num_condomless_anal_sex_receptive_w1 >= 2))/nrow(dt)

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
        HR_16 == 1 | 
          HR_18 == 1 |
          HR_31 == 1 |                                     
          HR_33 == 1 |                                       
          HR_35 == 1 |                                     
          HR_39 == 1 |                                       
          HR_45 == 1 |                                     
          HR_51 == 1 |                                       
          HR_52 == 1 |                                     
          HR_56 == 1 |                                       
          HR_58 == 1 |                                       
          HR_59 == 1 |                                     
          HR_68 == 1
       ), 
      1, 0)
    )
table(dt[["hr_hpv_any"]], exclude=NULL)
  
# xtab HIV-infection with HR HPV
xtabs(~factor(hr_hpv_any, exclude = NULL)+
        factor(hiv_w1, exclude = NULL), data=dt)

# compute number of high risk types

dt <-
  dt %>% 
  mutate(
    num_hr_hpv = 
      HR_16 + 
      HR_18 +
      HR_31+                                     
      HR_33+                                       
      HR_35+                                     
      HR_39+                                       
      HR_45+                                     
      HR_51+                                       
      HR_52+                                     
      HR_56+                                       
      HR_58+                                       
      HR_59+                                     
      HR_68 
  )
summary(dt$num_hr_hpv)

# compute number of high risk types for HIV+ and HIV-

dt %>% #HIV-infected
  filter(hiv_w1 == 1) %>%
  summary(num_hr_hpv) 

dt %>% #HIV-uninfected
  filter(hiv_w1 == 0) %>%
  summary(num_hr_hpv) 

# multiple HR types: summary

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
    hpv_16_and_18 = if_else((HR_16 == 1 & HR_18 == 1), 1, 0)
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
    hpv_16_or_18 = if_else((HR_16 == 1 | HR_18 == 1), 1, 0)
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
        #HR_6 == 1 | 
          LR_11 == 1 |
          HR_16 == 1 |                                     
          HR_18 == 1 |                                       
          HR_31 == 1 |                                     
          HR_33 == 1 |                                       
          HR_45 == 1 |                                     
          HR_52 == 1 |                                       
          HR_58 == 1 
      ), 
      1, 0)
  )
table(dt$nine.val.vac, exclude = NULL)

dt %>% # by HIV status
  filter(nine.val.vac == 1) %>%
  group_by(hiv_w1) %>%
  summarise(n=n()) 

# bivariate logistic regressions

## HR 16
dt %>% 
  filter(HR_16 == 1) %>%
  summarise(n=n()) 
  
dt %>% 
  filter(HR_16 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n()) 

## HR 18
dt %>% 
  filter(HR_18 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_18 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n()) 

## HR 31
dt %>% 
  filter(HR_31 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_31 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n()) 

## HR 33
dt %>% 
  filter(HR_33 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_33 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 35
dt %>% 
  filter(HR_35 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_35 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 39
dt %>% 
  filter(HR_39 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_39 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 45
dt %>% 
  filter(HR_45 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_45 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 51
dt %>% 
  filter(HR_51 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_51 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 52
dt %>% 
  filter(HR_52 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_52 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 58
dt %>% 
  filter(HR_58 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_58 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 59
dt %>% 
  filter(HR_59 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_59 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())

## HR 68
dt %>% 
  filter(HR_68 == 1) %>%
  summarise(n=n()) 

dt %>% 
  filter(HR_68 == 1) %>%
  group_by(hiv_w1) %>% 
  summarise(n=n())
