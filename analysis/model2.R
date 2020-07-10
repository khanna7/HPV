# Fit ERGM Model 2


# N.B.  ---------------------------
# This file uses setup information from "ergm-setup.R"
rm(list=ls())

# Libraries ---------------------------

library(dplyr)
library(ergm)


# Load data ---------------------------
  
load(file="ergm-setup-data-delete-24-cases.RData")


# Fit Model 2 with HR 16 ---------------------------

model_2.16 <- ergm(hpv_net ~ 
                     edges +
                     #nodematch("hr16")+
                     nodematch("hiv_hpv16", diff=T)+
                     #nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_anal_sex_receptive_2_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1),
                   #control = control.ergm(MCMC.interval = 1e4,
                    #                      MCMC.samplesize = 1e4),
                   eval.loglik = F
)

summary(model_2.16)
gof.model_2.16 <- gof(model_2.16)
p.model_2.16 <- plot(gof.model_2.16)
mcmc.diagnostics(model_2.16)



# Fit Model 2 with HR 45 ---------------------------

# model_2.45 <- ergm(hpv_net ~ 
#                      edges +
#                      #nodematch("hr45")+
#                      nodematch("hiv_hpv45", diff=F)+
#                      #nodematch("HIV")+
#                      nodematch("fta")+
#                      nodefactor("num_anal_sex_receptive_2_w1")+
#                      gwesp(1, fixed=TRUE)+
#                      #degree(0)+
#                      degree(1),
#                    eval.loglik = F
# )
# 
# summary(model_2.45)
# gof.model_2.45 <- gof(model_2.45)
# p.model_2.45 <- plot(gof.model_2.45)