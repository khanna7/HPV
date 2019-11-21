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


# Fit ERGM: Model 0_a ---------------------------

# The assortativity terms estimated in our Model 0-a were: 
# assortativity by any hrHPV infection, 
# assortativity by multiple hrHPV infection 
# assortativity by HIV status (binary variable coded as 1 for seropositive and 0 otherwise), 
# assortativity by syphilis infection status (fta), 
# assortativity by ages, 
# assortativity by education (dichotomous variable coded as 1 for high-school graduate or less and 0 otherwise), 
# assortativity by homelessness (dichotomous variable coded as 1 for experienced homeless and 0 otherwise), 
# and assortativity by sexual risk behavior (continuous variable of the square-root of the number of recent sexual partners). 
# + geometrically weighted edgewise shared partner statistics (GWESP)
# + degree 0 and degree 1 to control for having only one tie (degree1). 

model_0_a <- ergm(hpv_net ~ 
                  edges + 
                  absdiff("age") +
                  nodematch("any_type", diff=TRUE)+
                  nodematch("mult_hr_type")+
                  nodematch("HIV", diff=T)+
                  nodematch("fta", diff=T)+
                  nodematch("educ.cat", diff=T)+
                  nodefactor("past12m_homeless_w1", base=1)+
                  gwesp(1, fixed = T) + 
                  degree(0:1)
                
)

model_0_a
summary(model_0_a)
gof.model_0_a <- gof(model_0_a)
p.model_0_a <- plot(gof.model_0_a)

# Fit ERGM: Model 0_b ---------------------------

# Signifcant terms in 0_a are:
# edges, absdiff.age, nodematch.fta.1, gwesp.fixed.1, degree 0

# Other terms to control for: Any hrHPV infection 
# No hrHPV infection
# Number of hrHPV infection
# HIV-positive
# HIV-negative
# Syphilis-positive (FTA=1)
# Syphilis-negative (FTA=0)
# Age in years
#degree 0
# degree 1


model_0_b <- ergm(hpv_net ~ 
                    edges + 
                    absdiff("age") +
                    nodematch("any_type", diff=TRUE)+
                    nodematch("mult_hr_type")+
                    nodematch("HIV", diff=T)+
                    nodematch("fta", diff=T)+
                    gwesp(1, fixed=TRUE)+
                    degree(0)+
                    degree(1)
)

model_0_b
summary(model_0_b)
gof.model_0_b <- gof(model_0_b)
p.model_0_b <- plot(gof.model_0_b)


# Fit Model 1 ---------------------------

# Terms to include:
# HPV-16, HPV-18, HPV-31, HPV-33, HPV-35, HPV-45, HPV-51, HPV-52
# HPV-58, HPV-59, HPV-68, HIV, FTA, Number of receptive anal sex partners
# Age in years








