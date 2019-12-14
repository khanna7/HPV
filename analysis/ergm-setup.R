# Fit ERGMs

# N.B.  ---------------------------

# This file uses code from ERGM analysis first performed in Fall 2018: 
# https://github.com/khanna7/HPV/blob/master/analysis/HPV-analysis/analysis.R
# binary containing input data generated in "assortativity.R" at:
# https://github.com/khanna7/HPV/blob/master/analysis/HPV-analysis/analysis.R

# Libraries ---------------------------

library(dplyr)
library(ergm)


# Load data ---------------------------

load(file="assortativity.RData")


# Add needed attributes to network object ---------------------------

# dimensionality check

network.size(hpv_net) == nrow(dt)


# Add any HPV type: 
# any_type = 0	= not having HPV, 
            #1 =	having any type of HPV

# any type
hpv_net %v% "any_type" <- dt$any_type 

# age
hpv_net %v% "age" <- dt$age_w1 

#HR 18
hpv_net %v% "hr18" <- dt$HR_18

# mult hr type
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

dt <- #mult highrisk type
  dt %>% 
  mutate(
    mult_hr_type = if_else(num_hr_hpv > 1, 1, 0)
  )
table(dt$mult_hr_type, exclude = NULL)
hpv_net %v% "mult_hr_type" <- dt$mult_hr_type


# any HR type: "hr_hpv_any"
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

hpv_net %v% "hr_hpv_any" <- dt$hr_hpv_any


# sqrt of num_condomless_anal_sex_receptive_w1
hpv_net %v% "sqrt.num_condomless_anal_sex_receptive_w1" <- sqrt(hpv_net %v% 
                                                                  "num_condomless_anal_sex_receptive_w1")

# HIV_HPV45 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv45 = 
           case_when(hiv_w1 == 0 & HR_45 == 0 ~ 0,
                     hiv_w1 == 1 & HR_45 == 0 ~ 0,
                     hiv_w1 == 0 & HR_45 == 1 ~ 0,
                     hiv_w1 == 1 & HR_45 == 1 ~ 1)
  )
table(dt$hiv_hpv45, useNA = "always")
hpv_net %v% "hiv_hpv45" <- dt$hiv_hpv45

# HIV_HPV16 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv16 = 
           case_when(hiv_w1 == 0 & HR_16 == 0 ~ 0,
                     hiv_w1 == 1 & HR_16 == 0 ~ 0,
                     hiv_w1 == 0 & HR_16 == 1 ~ 0,
                     hiv_w1 == 1 & HR_16 == 1 ~ 1)
  )
table(dt$hiv_hpv16, useNA = "always")
hpv_net %v% "hiv_hpv16" <- dt$hiv_hpv16

# HIV_HPV16_and_18 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv16_and_18 = 
           case_when(hiv_w1 == 0 & HR_16_and_18 == 0 ~ 0,
                     hiv_w1 == 1 & HR_16_and_18 == 0 ~ 0,
                     hiv_w1 == 0 & HR_16_and_18 == 1 ~ 0,
                     hiv_w1 == 1 & HR_16_and_18 == 1 ~ 1)
  )
table(dt$hiv_hpv16_and_18, useNA = "always")
hpv_net %v% "hiv_hpv16_and_18" <- dt$hiv_hpv16_and_18

# HIV_HPV16_or_18 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv16_or_18 = 
           case_when(hiv_w1 == 0 & HR_16_or_18 == 0 ~ 0,
                     hiv_w1 == 1 & HR_16_or_18 == 0 ~ 0,
                     hiv_w1 == 0 & HR_16_or_18 == 1 ~ 0,
                     hiv_w1 == 1 & HR_16_or_18 == 1 ~ 1)
  )
table(dt$hiv_hpv16_or_18, useNA = "always")
hpv_net %v% "hiv_hpv16_or_18" <- dt$hiv_hpv16_or_18


# HIV_HPV16and18 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv18 = 
           case_when(hiv_w1 == 0 & HR_18 == 0 ~ 0,
                     hiv_w1 == 1 & HR_18 == 0 ~ 0,
                     hiv_w1 == 0 & HR_18 == 1 ~ 0,
                     hiv_w1 == 1 & HR_18 == 1 ~ 1)
  )
table(dt$hiv_hpv18, useNA = "always")
hpv_net %v% "hiv_hpv18" <- dt$hiv_hpv18



# categorize num_condomless_anal_sex_receptive_w1"

table(hpv_net %v% "num_condomless_anal_sex_receptive_w1", exclude=NULL)
dt <- 
  dt %>% 
  mutate(
    num_condomless_anal_sex_receptive.cat = if_else(dt$num_condomless_anal_sex_receptive_w1 > 0, 
                                                    1, 0)
  )
table(dt$num_condomless_anal_sex_receptive.cat, exclude = NULL)
hpv_net %v% "num_condomless_anal_sex_receptive.cat" <- dt$num_condomless_anal_sex_receptive.cat


# Save image ---------------------------

save.image(file="ergm-setup.RData")