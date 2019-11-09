## analysis file
## KAYO ASKED TO REMOVE ID 1036 FROM THE ANALYSIS BECAUSE THAT IS A TRANSGENDER PERSON. BUT
## NO PERSON WITH THAT ID IS IN THE DATASET.

# Clear slate ----------

rm(list=ls())


# Libraries ----------

library(dplyr)
library(ergm)


# Read data (& checks)----------

att_cb <- read.csv("../../HPV _attributes _codebook.csv")
att <- read.csv("../../HPV_attributes.csv")
cb <- read.csv("../../HPV_codebook.csv")
dyad <- read.csv("../../HPV_dyad.csv")

# glimpse(att_cb)
# glimpse(att)
# glimpse(cb)
# glimpse(dyad)


# Create network object ----------
dyad_el <- dyad[,c(1,2)]

dyad_el <- apply(dyad_el, c(1,2), "as.character")
dyad_net <- as.network(dyad_el, directed=FALSE)


# Explore network object

dyad_net
(dyad_net%v%"vertex.names")
list.vertex.attributes(dyad_net)

summary(dyad_net ~ edges)
degreedist(dyad_net)
summary(dyad_net ~ degree(0:10))
summary(dyad_net ~ esp(0:5))
summary(dyad_net ~ triangle)
#espartnerdist(dyad_net)
#summary.formula(dyad_net ~ esp)

plot(dyad_net)

# Add attributes to network object --------

## (replace missing values with median where necessary) 

vnames <- dyad_net %v% "vertex.names"
attid_in_network <- which(att$ID %in% vnames)
att_dt_of_int <- att[attid_in_network,]
att_dt_of_int$ID <- as.character(att_dt_of_int$ID)

## age 
dyad_net %v% "age_loc" <- att_dt_of_int$age_loc
summary(dyad_net %v% "age_loc")
age_loc_na <- which(is.na(dyad_net %v% "age_loc"))
tg.pers <- which(dyad_net %v% "vertex.names" == 1036)
(dyad_net %v% "vertex.names")[age_loc_na]

delete.vertices(dyad_net, vid = age_loc_na) #these nodes have missing data for many variables

att_dt_of_int <- att_dt_of_int[-age_loc_na,] #remove these nodes from attribute info of interest

# set.vertex.attribute(dyad_net, "age_loc",
#                      median(dyad_net %v% "age_loc", na.rm = T),
#                      age_loc_na)


## black race
dyad_net %v% "black" <- att_dt_of_int$race_loc___2
summary(dyad_net %v% "black")
table(dyad_net %v% "black")


## hiv status (use clearview_hivrh): 1=positive, 0=negative
table(att_dt_of_int$clearview_hivrh, useNA = "always")
att_dt_of_int <-
att_dt_of_int %>% 
  mutate(hiv_status := ifelse(clearview_hivrh == 1 | 
                               clearview_hivrh == 4, 1, 0)
  )
table(att_dt_of_int$hiv_status, useNA = "always")
dyad_net %v% "hiv_status" <- att_dt_of_int$hiv_status

##### do not use below for HIV status #####
table(att_dt_of_int$test_results, useNA = "always")
att_dt_of_int <- att_dt_of_int %>% 
  mutate(test_results_na_code = 
           ifelse(test_results < 0, NA, test_results)
         )
att_dt_of_int$test_results_na_rplcd <- 
  att_dt_of_int$test_results_na_code

test_results_na_rplcd_na <- which(is.na(att_dt_of_int$test_results_na_rplcd))
identical(test_results_na_rplcd_na, age_loc_na)
dyad_net %v% "test_results_na_rplcd" <- 
  att_dt_of_int$test_results_na_rplcd
set.vertex.attribute(dyad_net, "test_results_na_rplcd",
                     median(dyad_net %v% "test_results_na_rplcd", na.rm = T),
                     test_results_na_rplcd_na)
table(dyad_net %v% "test_results_na_rplcd", useNA = "always")
##### do not use above for HIV status #####


## high risk types
att_dt_of_int$high_risk_type
summary(att_dt_of_int$high_risk_type)
table(att_dt_of_int$high_risk_type, useNA = "always")
dyad_net %v% "high_risk_type" <- as.character(att_dt_of_int$high_risk_type)

dyad_net %v% "hr_16" <- att_dt_of_int$HR_16.x #.x and .y are identical
dyad_net %v% "hr_18" <- att_dt_of_int$HR_18.x
dyad_net %v% "hr_31" <- att_dt_of_int$HR_31.x
dyad_net %v% "hr_33" <-att_dt_of_int$HR_33.x
dyad_net %v% "hr_35" <- att_dt_of_int$HR_35.x
dyad_net %v% "hr_39" <- att_dt_of_int$HR_39.x
dyad_net %v% "hr_45" <- att_dt_of_int$HR_45.x
dyad_net %v% "hr_51" <- att_dt_of_int$HR_51.x
dyad_net %v% "hr_52" <- att_dt_of_int$HR_52.x
dyad_net %v% "hr_56" <- att_dt_of_int$HR_56.x
dyad_net %v% "hr_58" <- att_dt_of_int$HR_58.x
dyad_net %v% "hr_59" <- att_dt_of_int$HR_59.x
dyad_net %v% "hr_68" <-att_dt_of_int$HR_68.x


## number of sex partners
table(att_dt_of_int$A6MoSEXN, exclude = NULL)
summary(att_dt_of_int$A6MoSEXN)
dyad_net %v% "sqrt.A6MoSEXN" <- sqrt(att_dt_of_int$A6MoSEXN)

## HIV_HPV45 nodemix

table(att_dt_of_int$HR_45.x, useNA = "always")
table(att_dt_of_int$hiv_status, useNA = "always")
xtabs(~factor(att_dt_of_int$HR_45.x, exclude=NULL) + 
        factor(att_dt_of_int$hiv_status, exclude = NULL)
)

att_dt_of_int <-
  att_dt_of_int %>% 
  mutate(hiv_hpv45 = 
           case_when(hiv_status == 0 & HR_45.x == 0 ~ 0,
                     hiv_status == 1 & HR_45.x == 0 ~ 0,
                     hiv_status == 0 & HR_45.x == 1 ~ 0,
                     hiv_status == 1 & HR_45.x == 1 ~ 1)
  )
table(att_dt_of_int$hiv_hpv45, useNA = "always")
dyad_net %v% "hiv_hpv45" <- att_dt_of_int$hiv_hpv45

# Fit models -----

# m1 <- ergm(dyad_net ~ edges)
# summary(m1)
# 
# m2 <- ergm(dyad_net ~ edges + absdiff("age_loc"))
# summary(m2)
# 
# m3 <- ergm(dyad_net ~ edges + 
#              absdiff("age_loc") +
#              nodematch("black")
#              )
# summary(m3)
# 
# m4 <- ergm(dyad_net ~ edges + 
#              absdiff("age_loc") +
#              nodematch("test_results_na_rplcd")
#           )
# summary(m4)
# 
# m5 <- ergm(dyad_net ~ edges + 
#              absdiff("age_loc") +
#              nodematch("test_results_na_rplcd")+
#              nodematch("high_risk_type")
# )
# summary(m5)
# 
# m6 <- ergm(dyad_net ~ edges + 
#              absdiff("age_loc") +
#              #nodematch("black") +
#              nodematch("test_results_na_rplcd")+
#              nodematch("high_risk_type")+
#              nodecov("A6MoSEXN")
# )
# summary(m6)
# 
# m7 <- ergm(dyad_net ~ edges + 
#              absdiff("age_loc") +
#              #nodematch("black") +
#              nodematch("test_results_na_rplcd")+
#              nodematch("high_risk_type")+
#              nodecov("A6MoSEXN")+
#              gwesp(1, fixed=T),
#            eval.loglik = F
# )
# summary(m7)
# gof_m7 <- gof(m7)
# summary(gof_m7)
# 
# pdf(file="m7-alpha1-gof.pdf")
# plot(gof_m7)
# dev.off()
# 
# # m7b <- ergm(dyad_net ~ edges + 
# #              absdiff("age_loc") +
# #              nodematch("black") +
# #              nodematch("test_results_na_rplcd")+
# #              nodematch("high_risk_type_na_code")+
# #              nodecov("A6MoSEXN")+
# #              gwesp(alpha=2, fixed=T)
# # )
# 
# m8 <- ergm(dyad_net ~ edges + 
#              absdiff("age_loc") +
#              #nodematch("black") +
#              nodematch("test_results_na_rplcd")+
#              nodematch("high_risk_type")+
#              nodecov("A6MoSEXN")+
#              gwesp(1, fixed=T)+
#              degree(0:1),
#            eval.loglik = F
# )
# 
# summary(m8)
# gof_m8 <- gof(m8)
# summary(gof_m8)
# 
# pdf(file="m8-alpha1-gof.pdf")
# plot(gof_m8)
# dev.off()

# m9 <- ergm(dyad_net ~ edges + 
#              absdiff("age_loc") +
#              #nodematch("black") +
#              nodematch("test_results_na_rplcd")+
#              nodematch("high_risk_type")+
#              nodecov("A6MoSEXN")+
#              gwesp(1, fixed=T)+
#              degree(0:1),
#            eval.loglik = F
# )
# 
# summary(m9)
#gof_m9 <- gof(m9)
#summary(gof_m9)

# pdf(file="m9-alpha1-gof.pdf")
# plot(gof_m9)
# dev.off()

## NEXT models to be examined: 
## a) Including all high-risk types in the model.
## b) Including specific high risk types one by one
## c) Interaction between gwesp and HR type  

# all high risk types ----
#form.m9 <- m9$formula
#class(form.m9)

#form.m9_all_hr <- as.formula(paste0(form.m9, "+nodematch(high_risk_type)"))

# m9_all_hr <- 
#   ergm(dyad_net ~ edges + 
#          absdiff("age_loc") +
#          #nodematch("black") +
#          #nodematch("test_results_na_rplcd")+
#          nodematch("high_risk_type")+
#          nodecov("A6MoSEXN")+
#          gwesp(1, fixed=T)+
#          degree(0:1)+
#          nodematch("hr_16")+
#          nodematch("hr_18")+
#          nodematch("hr_31")+
#          nodematch("hr_33")+
#          nodematch("hr_35")+
#          nodematch("hr_39")+
#          nodematch("hr_45")+
#          nodematch("hr_52")+
#          nodematch("hr_56")+
#          nodematch("hr_58")+
#          nodematch("hr_59")+
#          nodematch("hr_68"),
#        eval.loglik = F
#   )
# summary(m9_all_hr) 
# 
# m9_hr_16 <- 
#   ergm(dyad_net ~ edges + 
#          absdiff("age_loc") +
#          #nodematch("black") +
#          #nodematch("test_results_na_rplcd")+
#          nodematch("high_risk_type")+
#          nodecov("A6MoSEXN")+
#          gwesp(1, fixed=T)+
#          degree(0:1)+
#          nodematch("hr_16"),
#        eval.loglik = F
#   )
# summary(m9_hr_16)
#   
# m9_hr_33 <- 
#   ergm(dyad_net ~ edges + 
#          absdiff("age_loc") +
#          #nodematch("black") +
#          #nodematch("test_results_na_rplcd")+
#          nodematch("high_risk_type")+
#          nodecov("A6MoSEXN")+
#          gwesp(1, fixed=T)+
#          degree(0:1)+
#          nodematch("hr_33"),
#        eval.loglik = F
#   )
# summary(m9_hr_33)
# 
# m9_hr_45 <- 
#   ergm(dyad_net ~ edges + 
#          absdiff("age_loc") +
#          #nodematch("black") +
#          #nodematch("test_results_na_rplcd")+
#          nodematch("high_risk_type")+
#          nodecov("A6MoSEXN")+
#          gwesp(1, fixed=T)+
#          degree(0:1)+
#          nodematch("hr_45"),
#        eval.loglik = F
#   )
# summary(m9_hr_45)

# m10 <- #as per kayo's email dated 04/13/2018
#   ergm(dyad_net ~ edges + 
#          absdiff("age_loc") +
#          nodematch("high_risk_type", diff=T)+
#          nodematch("hiv_status", diff=T)+
#          nodecov("sqrt.A6MoSEXN")+
#          nodematch("hr_45", diff=T)+
#          # nodematch("hiv_hpv45", diff=T)+
#          gwesp(1, fixed=T)+
#          degree(0:1),
#        eval.loglik = F
#   )
# 
# summary(m10)
# 
# plot(gof(m10 ~ model))
# plot(gof(m10 ~ esp))
# plot(gof(m10 ~ degree))
# plot(gof(m10 ~ distance))
# 
# m10.sim <- simulate(m10, nsim=100)
# m10.sim.ecount <- lapply(m10.sim, network.edgecount)
# summary(unlist(m10.sim.ecount))

m11.1 <- #as per kayo's email dated 04/13/2018
  ergm(dyad_net ~ edges + 
         absdiff("age_loc") +
         nodematch("high_risk_type", diff=T)+
         nodematch("hiv_status", diff=T)+
         nodecov("sqrt.A6MoSEXN")+
         nodematch("hr_45", diff=T)+
         nodematch("hiv_hpv45", diff=T)+
         gwesp(1, fixed=T)+
         degree(0:1),
       eval.loglik = F
  )
summary(m11.1)

m11.2 <- #as per kayo's email dated 04/13/2018
  ergm(dyad_net ~ edges + 
         absdiff("age_loc") +
         #nodematch("high_risk_type", diff=T)+
         nodematch("hiv_status", diff=T)+
         nodecov("sqrt.A6MoSEXN")+
         nodematch("hr_45", diff=T)+
         nodematch("hiv_hpv45", diff=T)+
         gwesp(1, fixed=T)+
         degree(0:1),
       eval.loglik = F
  )
summary(m11.2)

#m9_all_highrisk <- 

## control for seed dependence

## mixing between high risk and low risk (construct new "risk variable" with 0=low, 1=high)
## mixing on high-types: one-by-one for each
## mixing on low-types: one-by-one for each


# Save image ----------

save.image(file="hpv-analysis.RData")