# Fit ERGMs

rm(list=ls())


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

# Remove rows corresponding to removed nodes from `dt`
dt <- filter(dt, !row_number() %in% nodes.remove)
dim(dt)


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
hpv_net %v% "hr18" <- dt$hr_18

# mult hr type
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

hpv_net %v% "hr_hpv_any" <- dt$hr_hpv_any


# sqrt of num_condomless_anal_sex_receptive_w1
hpv_net %v% "sqrt.num_anal_sex_receptive_2_w1" <- sqrt(hpv_net %v% 
                                                                  "num_anal_sex_receptive_2_w1")

# HIV_HPV45 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv45 = 
           case_when(hiv_w1 == 0 & hr_45 == 0 ~ 0,
                     hiv_w1 == 1 & hr_45 == 0 ~ 0,
                     hiv_w1 == 0 & hr_45 == 1 ~ 0,
                     hiv_w1 == 1 & hr_45 == 1 ~ 1)
  )
table(dt$hiv_hpv45, useNA = "always")
hpv_net %v% "hiv_hpv45" <- dt$hiv_hpv45

# HIV_HPV16 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv16 = 
           case_when(hiv_w1 == 0 & hr_16 == 0 ~ 0,
                     hiv_w1 == 1 & hr_16 == 0 ~ 0,
                     hiv_w1 == 0 & hr_16 == 1 ~ 0,
                     hiv_w1 == 1 & hr_16 == 1 ~ 1)
  )
table(dt$hiv_hpv16, useNA = "always")
hpv_net %v% "hiv_hpv16" <- dt$hiv_hpv16

# HIV_HPV16_and_18 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv16_and_18 = 
           case_when(hiv_w1 == 0 & hr_16_and_18 == 0 ~ 0,
                     hiv_w1 == 1 & hr_16_and_18 == 0 ~ 0,
                     hiv_w1 == 0 & hr_16_and_18 == 1 ~ 0,
                     hiv_w1 == 1 & hr_16_and_18 == 1 ~ 1)
  )
table(dt$hiv_hpv16_and_18, useNA = "always")
hpv_net %v% "hiv_hpv16_and_18" <- dt$hiv_hpv16_and_18

# HIV_HPV16_or_18 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv16_or_18 = 
           case_when(hiv_w1 == 0 & hr_16_or_18 == 0 ~ 0,
                     hiv_w1 == 1 & hr_16_or_18 == 0 ~ 0,
                     hiv_w1 == 0 & hr_16_or_18 == 1 ~ 0,
                     hiv_w1 == 1 & hr_16_or_18 == 1 ~ 1)
  )
table(dt$hiv_hpv16_or_18, useNA = "always")
hpv_net %v% "hiv_hpv16_or_18" <- dt$hiv_hpv16_or_18


# HIV_HPV16and18 nodemix

dt <-
  dt %>% 
  mutate(hiv_hpv18 = 
           case_when(hiv_w1 == 0 & hr_18 == 0 ~ 0,
                     hiv_w1 == 1 & hr_18 == 0 ~ 0,
                     hiv_w1 == 0 & hr_18 == 1 ~ 0,
                     hiv_w1 == 1 & hr_18 == 1 ~ 1)
  )
table(dt$hiv_hpv18, useNA = "always")
hpv_net %v% "hiv_hpv18" <- dt$hiv_hpv18



# categorize num_anal_sex_receptive_w1"

table(hpv_net %v% "num_anal_sex_receptive_2_w1", exclude=NULL)
dt <- ## NEEDS TO BE CHECKED
  dt %>% 
  mutate(
    num_anal_sex_receptive_2_w1.cat = if_else(dt$num_anal_sex_receptive_2_w1 > 0, 
                                                    1, 0)
  )
table(dt$num_anal_sex_receptive_2_w1.cat, exclude = NULL)
hpv_net %v% "num_anal_sex_receptive_2_w1.cat" <- dt$num_anal_sex_receptive_2_w1.cat


# add sexual identity
hpv_net %v% "sexual_identity_w1" <- dt$sexual_identity_w1 
hpv_net %v% "sex.id.cat" <- dt$sex.id.cat

# Save image ---------------------------

save.image(file="ergm-setup.RData")
