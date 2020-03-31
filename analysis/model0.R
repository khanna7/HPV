# Fit ERGM Model 0
  ## (nice tutorial on the formula function, used below: 
  ## https://www.datacamp.com/community/tutorials/r-formula-tutorial)

rm(list=ls())


# N.B.  ---------------------------
  # This file uses setup information from "ergm-setup.R"


# Libraries ---------------------------

library(dplyr)
library(ergm)


# Load data ---------------------------

load(file="ergm-setup.RData")


# Create needed variables ---------------------------

# All needed variables are constructed in ergm-setup.R


# Fit ERGM: Model 0_a ---------------------------

# The assortativity terms estimated in our Model 0-a were: 
# assortativity by any hrHPV infection, 
# assortativity by multiple hrHPV infection 
# assortativity by HIV status (binary variable coded as 1 for seropositive and 0 otherwise), 
# assortativity by syphilis infection status (fta), 
# assortativity by ages, 
# assortativity by education (dichotomous variable coded as 1 for high-school graduate or less and 0 otherwise), 
# assortativity by homelessness (dichotomous variable coded as 1 for experienced homeless and 0 otherwise), 
# assortativity by sexual risk behavior (continuous variable of the square-root of the number of recent sexual partners). 
# assortativity by HIV-16 or 18 infection
# assortativity by HIV-16 and 18 infection
# + geometrically weighted edgewise shared partner statistics (GWESP)
# + degree 0 and degree 1 to control for having only one tie (degree1). 

factors <- c("edges", "degree(0:1)", "gwesp(1, fixed=TRUE)") #model with degree(1) only does not converge
form.model0 <- as.formula(paste0("hpv_net~", paste0(factors, collapse="+")))

model0.age <- update(form.model0, ~. +absdiff("age"))#specify models
model0.hr_hpv_any <- update(form.model0, ~. + nodematch("hr_hpv_any", diff=F)) #model with diff=F doesn't fit
model0.mult_hr_type <- update(form.model0, ~. + nodematch("mult_hr_type"))
model0.hr_16_or_18 <- update(form.model0, ~. + nodematch("hr_16_or_18", diff=F))
model0.hr_16_and_18 <- update(form.model0, ~. + nodematch("hr_16_and_18"))
model0.HIV <- update(form.model0, ~. + nodematch("HIV"))
model0.fta <- update(form.model0, ~. + nodematch("fta"))
model0.educ.cat <- update(form.model0, ~. + nodematch("educ.cat"))
model0.past12m_homeless_w1 <- update(form.model0, ~. + nodematch("past12m_homeless_w1"))
model0.sqrt.num_anal_sex_receptive_2_w1 <- update(form.model0, ~. + absdiff("sqrt.num_anal_sex_receptive_2_w1"))
model0.num_anal_sex_receptive_2_w1 <- update(form.model0, ~. + absdiff("num_anal_sex_receptive_2_w1"))
model0.sexid.cat <- update(form.model0, ~. +nodemix("sex.id.cat"))#sex.id.cat

model0.hr16 <- update(form.model0, ~. + nodematch("hr16", diff=F))
model0.hr18 <- update(form.model0, ~. + nodematch("hr18", diff=F))
model0.hr31 <- update(form.model0, ~. + nodematch("hr31", diff=F))
model0.hr33 <- update(form.model0, ~. + nodematch("hr33", diff=F))
model0.hr35 <- update(form.model0, ~. + nodematch("hr35", diff=F))
model0.hr39 <- update(form.model0, ~. + nodematch("hr39", diff=F))
model0.hr45 <- update(form.model0, ~. + nodematch("hr45", diff=F))
model0.hr51 <- update(form.model0, ~. + nodematch("hr51", diff=F))
model0.hr58 <- update(form.model0, ~. + nodematch("hr58", diff=F))
model0.hr59 <- update(form.model0, ~. + nodematch("hr59", diff=F))
model0.hr68 <- update(form.model0, ~. + nodematch("hr68", diff=F))

model0_a_age <- ergm(formula = model0.age, eval.loglik = F) #fit models
model0_a_hr_hpv_any <- ergm(formula = model0.hr_hpv_any, eval.loglik = F)
model0_a_hr_16_or_18 <- ergm(formula = model0.hr_16_or_18, eval.loglik = F)
model0_a_hr_16_and_18 <- ergm(formula = model0.hr_16_and_18, eval.loglik = F)
model0.mult_hr_type <- ergm(formula = model0.mult_hr_type, eval.loglik = F)
model0_a_fta <- ergm(formula = model0.fta, eval.loglik = F)
model0_a_educ.cat <- ergm(formula = model0.educ.cat, eval.loglik = F)
model0_a_past12m_homeless_w1 <- ergm(formula = model0.past12m_homeless_w1, eval.loglik = F)
model0_a_sqrt.num_anal_sex_receptive_2_w1 <- ergm(formula = model0.sqrt.num_anal_sex_receptive_2_w1, eval.loglik = F)
model0_num_anal_sex_receptive_2_w1 <- ergm(formula = model0.num_anal_sex_receptive_2_w1, eval.loglik = F)
model0_sexid.cat <- ergm(formula = model0.sexid.cat, eval.loglik = F)  

model0_hr16 <- ergm(formula = model0.hr16, eval.loglik = F)  
model0_hr18 <- ergm(formula = model0.hr18, eval.loglik = F)  
model0_hr31 <- ergm(formula = model0.hr31, eval.loglik = F) 
model0_hr33 <- ergm(formula = model0.hr33, eval.loglik = F) 
model0_hr35 <- ergm(formula = model0.hr35, eval.loglik = F) 
model0_hr39 <- ergm(formula = model0.hr39, eval.loglik = F) 
model0_dhr45 <- ergm(formula = model0.hr45, eval.loglik = F) 
model0_hr51 <- ergm(formula = model0.hr51, eval.loglik = F) 
model0_hr58 <- ergm(formula = model0.hr58, eval.loglik = F) 
model0_hr59 <- ergm(formula = model0.hr59, eval.loglik = F) 
model0_hr68 <- ergm(formula = model0.hr68, eval.loglik = F) 


summary(model0_a_age)  #obtain coefficients, *=significant term
summary(model0_a_hr_hpv_any)
summary(model0.mult_hr_type)
summary(model0_a_hr_16_or_18) #borderline significant
summary(model0_a_hr_16_and_18)
summary(model0_a_fta)
summary(model0_a_past12m_homeless_w1)
summary(model0_a_sqrt.num_anal_sex_receptive_2_w1)
summary(model0_num_anal_sex_receptive_2_w1)
summary(model0_sexid.cat)

summary(model0_hr16)
summary(model0_hr18)
summary(model0_hr31)
summary(model0_hr33)
summary(model0_hr35)
summary(model0_hr39)
summary(model0_hr45)
summary(model0_hr51)
summary(model0_hr58)
summary(model0_hr59)
summary(model0_hr68)


# Fit ERGM: Model 0_b ---------------------------

# Signifcant terms in 0_a are:
# edges, absdiff.age, 

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
                    nodematch("hr_16_or_18")+
                    gwesp(1, fixed=TRUE)+
                    degree(0:1),
                  eval.loglik = F
)

#model_0_b
summary(model_0_b)
gof.model_0_b <- gof(model_0_b)
p.model_0_b <- plot(gof.model_0_b)



