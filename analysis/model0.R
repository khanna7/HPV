# Fit ERGM Model 0
  ## (nice tutorial on the formula function, used below: 
  ## https://www.datacamp.com/community/tutorials/r-formula-tutorial)

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
model0.hr_hpv_any <- update(form.model0, ~. + nodematch("hr_hpv_any")) #model with diff=T doesn't fit
model0.mult_hr_type <- update(form.model0, ~. + nodematch("mult_hr_type"))
model0.HR_16_or_18 <- update(form.model0, ~. + nodematch("HR_16_or_18"))
model0.HR_16_and_18 <- update(form.model0, ~. + nodematch("HR_16_and_18"))
model0.HIV <- update(form.model0, ~. + nodematch("HIV"))
model0.fta <- update(form.model0, ~. + nodematch("fta"))
model0.educ.cat <- update(form.model0, ~. + nodematch("educ.cat"))
model0.past12m_homeless_w1 <- update(form.model0, ~. + nodematch("past12m_homeless_w1"))
model0.sqrt.num_condomless_anal_sex_receptive_w1 <- update(form.model0, ~. + absdiff("sqrt.num_condomless_anal_sex_receptive_w1"))


model0_a_age <- ergm(formula = model0.age, eval.loglik = F) #fit models
model0_a_hr_hpv_any <- ergm(formula = model0.hr_hpv_any, eval.loglik = F)
model0_a_HR_16_or_18 <- ergm(formula = model0.HR_16_or_18, eval.loglik = F)
model0_a_HR_16_and_18 <- ergm(formula = model0.HR_16_and_18, eval.loglik = F)
model0.mult_hr_type <- ergm(formula = model0.mult_hr_type, eval.loglik = F)
model0_a_fta <- ergm(formula = model0.fta, eval.loglik = F)
model0_a_educ.cat <- ergm(formula = model0.educ.cat, eval.loglik = F)
model0_a_past12m_homeless_w1 <- ergm(formula = model0.past12m_homeless_w1, eval.loglik = F)
model0_a_sqrt.num_condomless_anal_sex_receptive_w1 <- ergm(formula = model0.sqrt.num_condomless_anal_sex_receptive_w1, eval.loglik = F)
  

summary(model0_a_age)  #obtain coefficients, *=significant term
summary(model0_a_hr_hpv_any)
summary(model0.mult_hr_type)
summary(model0_a_HR_16_or_18) #borderline significant
summary(model0_a_HR_16_and_18)
summary(model0_a_fta)
summary(model0_a_past12m_homeless_w1)
summary(model0_a_sqrt.num_condomless_anal_sex_receptive_w1)


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
                    nodematch("HR_16_or_18")+
                    gwesp(1, fixed=TRUE)+
                    degree(0:1),
                  eval.loglik = F
)

#model_0_b
summary(model_0_b)
gof.model_0_b <- gof(model_0_b)
p.model_0_b <- plot(gof.model_0_b)



