# Fit ERGMs

# N.B.  ---------------------------
# This file uses setup information from "ergm-setup.R"


# Libraries ---------------------------

library(dplyr)
library(ergm)

# Load data ---------------------------

load(file="ergm-setup.RData")


# Fit Model 1 ---------------------------

# Terms to include:
# HPV-16, HPV-18, HPV-31, HPV-33, HPV-35, HPV-45, HPV-51, HPV-52
# HPV-58, HPV-59, HPV-68, HIV, FTA, Number of receptive anal sex partners
# Age in years

model_1.16 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr16")+
                     # nodematch("hr18")+
                     # nodematch("hr31")+
                     # nodematch("hr33")+
                     # nodematch("hr35")+
                     # nodematch("hr45")+
                     # nodematch("hr51")+
                     # nodematch("hr52")+
                     # nodematch("hr58")+
                     # nodematch("hr59")+
                     # nodematch("hr68")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.16
summary(model_1.16)


model_1.18 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr18")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.18
summary(model_1.18)

model_1.31 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr31")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.31
summary(model_1.31)

model_1.33 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr33")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.33
summary(model_1.33)

model_1.35 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr35")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.35
summary(model_1.35)

model_1.45 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr45")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.45
summary(model_1.45)

model_1.51 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr51")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.18
summary(model_1.18)

model_1.52 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr52")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.52
summary(model_1.52)

model_1.58 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr58")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.58
summary(model_1.58)


model_1.59 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr59")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.59
summary(model_1.59)

model_1.68 <- ergm(hpv_net ~ 
                     edges +
                     nodematch("hr68")+
                     nodematch("HIV")+
                     nodematch("fta")+
                     nodefactor("num_condomless_anal_sex_receptive_w1")+
                     gwesp(1, fixed=TRUE)+
                     degree(0)+
                     degree(1)
)

#model_1.68
summary(model_1.68)

