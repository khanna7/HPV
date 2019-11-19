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


# Add needed attributes to network object

# dimensionality check
network.size(hpv_net) == nrow(dt)

# Add any HPV type: any_type = 0	= not having HPV, 1 =	having any type of HPV
hpv_net %v% "any_type" <- dt$any_type
hpv_net %v% "age" <- dt$age_w1

# Fit ERGM: Model 0

# The assortativity terms estimated in our Model 0-a were: 
# assortativity by any hrHPV infection, 
# assortativity by multiple 
# hrHPV infection (binary variable coded as 1 for infected at least 
# 2 hrHPV types, and 0 otherwise), 
# assortativity by HIV status (binary variable coded as 1 for seropositive and 0 otherwise), 
# assortativity by syphilis infection status (fta), 
# assortativity by ages, 
# assortativity by education (dichotomous variable coded as 1 for high-school graduate or less and 0 otherwise), 
# assortativity by homelessness (dichotomous variable coded as 1 for experienced homeless and 0 otherwise), 
# and assortativity by sexual risk behavior (continuous variable of the square-root of the number of recent sexual partners). 
# The three structural terms specified in Model 0 were geometrically weighted edgewise shared partner statistics (GWESP)
# to represent shared partners, of degree 0 (degree0) 
# to control for isolates, and degree 1 to control for having only one tie (degree1). 

prelim.model <- ergm(hpv_net ~ 
                       edges + 
                       absdiff("age") +
                       nodematch("any_type", diff=TRUE)+
                       nodematch("HIV", diff=T)+
                       nodematch("fta", diff=T)+
                       nodematch("educ.cat", diff=T)
)
                       + 
                       #nodematch("hiv_status", diff = T) + 
                       nodecov("sqrt.A6MoSEXN") + 
                       nodefactor("high_risk_type", base=1)+
                       nodematch("high_risk_type", diff = T) + 
                       gwesp(1, fixed = T) + 
                       degree(0:1)
)
summary(prelim.model)
gof(prelim.model)
