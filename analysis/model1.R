# Fit ERGM Model 1

# N.B.  ---------------------------
# This file uses setup information from "ergm-setup.R"

rm(list=ls())


# Libraries ---------------------------

library(dplyr)
library(ergm)


# Load data ---------------------------

load(file="ergm-setup-data-delete-24-cases.RData")


# Fit Model 1 ---------------------------

factors <- c("edges", 
             #"absdiff('age')",
             "degree(0:1)", "gwesp(1, fixed=TRUE)",
             "nodematch('HIV')", "nodematch('fta')", 
             "nodematch('num_anal_sex_receptive_2_w1.cat', diff=TRUE)"
             ) #model with degree(1) only does not converge
form.model1 <- as.formula(paste0("hpv_net~", paste0(factors, collapse="+")))

# Terms to include:
# HPV-16, HPV-18, HPV-31, HPV-33, HPV-35, HPV-45, HPV-51, HPV-52
# HPV-58, HPV-59, HPV-68, HIV, FTA, Number of receptive anal sex partners
# Age in years

model_1.16 <- update(form.model1, ~. +nodematch("hr16", diff=T))
fit.model_1.16 <- ergm(model_1.16, eval.loglik = FALSE)  
summary(fit.model_1.16)
mcmc.diagnostics(fit.model_1.16)

model_1.18 <- update(form.model1, ~. +nodematch("hr18", diff=T))
fit.model_1.18 <- ergm(model_1.18, eval.loglik = FALSE)  
summary(fit.model_1.18)
mcmc.diagnostics(fit.model_1.18)

model_1.31 <- update(form.model1, ~. +nodematch("hr31", diff=T)) 
fit.model_1.31 <- ergm(model_1.31, eval.loglik = FALSE)  
summary(fit.model_1.31)
mcmc.diagnostics(fit.model_1.31)

model_1.33 <- update(form.model1, ~. +nodematch("hr33", diff=T))
fit.model_1.33 <- ergm(model_1.33, eval.loglik = FALSE)  
summary(fit.model_1.33)
mcmc.diagnostics(fit.model_1.33)
 
model_1.35 <- update(form.model1, ~. +nodematch("hr35", diff=T)) 
fit.model_1.35 <- ergm(model_1.35, eval.loglik = FALSE)  
summary(fit.model_1.35)
mcmc.diagnostics(fit.model_1.35)
 
model_1.39 <- update(form.model1, ~. +nodematch("hr39", diff=T))
fit.model_1.39 <- ergm(model_1.39, eval.loglik = FALSE)  
summary(fit.model_1.39)
mcmc.diagnostics(fit.model_1.39)
 
model_1.45 <- update(form.model1, ~. +nodematch("hr45", diff=F)) #diff=T not converging
fit.model_1.45 <- ergm(model_1.45, eval.loglik = FALSE)
summary(fit.model_1.45)
mcmc.diagnostics(fit.model_1.45) 

model_1.51 <- update(form.model1, ~. +nodematch("hr51", diff=T))
fit.model_1.51 <- ergm(model_1.51, eval.loglik = FALSE)  
summary(fit.model_1.51)
mcmc.diagnostics(fit.model_1.51)

model_1.52 <- update(form.model1, ~. +nodematch("hr52", diff=T))
fit.model_1.52 <- ergm(model_1.52, eval.loglik = FALSE)  
summary(fit.model_1.52)
mcmc.diagnostics(fit.model_1.52)

model_1.58 <- update(form.model1, ~. +nodematch("hr58", diff=T))
fit.model_1.58 <- ergm(model_1.58, eval.loglik = FALSE)  
summary(fit.model_1.58)
mcmc.diagnostics(fit.model_1.58)
 
model_1.59 <- update(form.model1, ~. +nodematch("hr59", diff=T))
fit.model_1.59 <- ergm(model_1.59, eval.loglik = FALSE)  
summary(fit.model_1.59)
mcmc.diagnostics(fit.model_1.59) 

model_1.68 <- update(form.model1, ~. +nodematch("hr68", diff=T))
fit.model_1.68 <- ergm(model_1.68, eval.loglik = FALSE)  
summary(fit.model_1.68)
mcmc.diagnostics(fit.model_1.68)

model_1.hr_16_and_18 <- update(form.model1, ~. +nodematch("hr_16_and_18", diff=T)) 
fit.model_1.hr_16_and_18 <- ergm(model_1.hr_16_and_18, eval.loglik = FALSE)
summary(fit.model_1.hr_16_and_18)
mcmc.diagnostics(fit.model_1.hr_16_and_18)
 
model_1.hr_16_or_18 <- update(form.model1, ~. +nodematch("hr_16_or_18", diff=T)) 
fit.model_1.hr_16_or_18 <- ergm(model_1.hr_16_or_18, eval.loglik = FALSE)
summary(fit.model_1.hr_16_or_18)
mcmc.diagnostics(fit.model_1.hr_16_or_18)
 
model_1.hiv<- update(form.model1, ~. +nodematch("HIV", diff=TRUE)) 
fit.model_1.hiv<- ergm(model_1.hiv, eval.loglik = FALSE)  
summary(fit.model_1.hiv)
mcmc.diagnostics(fit.model_1.hiv) 

model_1.fta<- update(form.model1, ~. +nodematch("fta", diff=T)) 
fit.model_1.fta<- ergm(model_1.fta, eval.loglik = FALSE)  
summary(fit.model_1.fta)
mcmc.diagnostics(fit.model_1.fta) 

model_1.hiv_hpv16<- update(form.model1, ~. +nodematch("hiv_hpv16", diff=T))
fit.model_1.hiv_hpv16<- ergm(model_1.hiv_hpv16, eval.loglik = FALSE)  
summary(fit.model_1.hiv_hpv16)
mcmc.diagnostics(fit.model_1.hiv_hpv16) 

model_1.hiv_hpv18<- update(form.model1, ~. +nodematch("hiv_hpv18", diff=T))
fit.model_1.hiv_hpv18<- ergm(model_1.hiv_hpv18, eval.loglik = FALSE)  
summary(fit.model_1.hiv_hpv18)
mcmc.diagnostics(fit.model_1.hiv_hpv18)

model_1.hiv_hpv16_and_18<- update(form.model1, ~. +nodematch("hiv_hpv16_and_18", diff=T))
fit.model_1.hiv_hpv16_and_18<- ergm(model_1.hiv_hpv16_and_18, eval.loglik = FALSE)  
summary(fit.model_1.hiv_hpv16_and_18)
mcmc.diagnostics(fit.model_1.hiv_hpv16_and_18)

model_1.hiv_hpv16_or_18<- update(form.model1, ~. +nodematch("hiv_hpv16_or_18", diff=T))
fit.model_1.hiv_hpv16_or_18<- ergm(model_1.hiv_hpv16_or_18, eval.loglik = FALSE)  
summary(fit.model_1.hiv_hpv16_or_18)
mcmc.diagnostics(fit.model_1.hiv_hpv16_or_18)

model_1.hiv_hpv45<- update(form.model1, ~. +nodematch("hiv_hpv45", diff=T))
fit.model_1.hiv_hpv45<- ergm(model_1.hiv_hpv45, eval.loglik = FALSE)  
summary(fit.model_1.hiv_hpv45)
mcmc.diagnostics(fit.model_1.hiv_hpv45) 
