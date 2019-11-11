
install.packages("dplyr")
install.packages("RDS")

#suppressPackageStartupMessages(library(RDS))
library(dplyr)
library(RDS)
###################################################################################

#----------does not need anymore if the recruiter.id is present-------------
#ref = read.table("C:/Users/Administrator/Desktop/refer_1.txt", header = T, row.names = 1, check.names = F)
#all(rownames(ref) == colnames(ref))
# 
#list_r = apply(ref, MARGIN = 2, function(c) rownames(ref)[which(c == 1)])
#recruiter.id = sapply(list_r, function(x) {ifelse(length(x)>0, x, "seed")})
#cbind(dat$wave, recruiter.id)
#---------------------------------------------------------------------------


#start from here

dat_a = read.csv("C:/work/PrEP_paper/co-infection_Syphilis_HIV/data/co-infection_all_755.csv", as.is = T)

(dname = colnames(dat_a))
colnames(dat_a)[dname == "participant_id"] <- "id"

colnames(dat_a)[dname == "referred_by"] <- "recruiter.id"
dat_a$recruiter.id[which(is.na(dat_a$recruiter.id))] = 0#"seed"

#dname
#dat_a$recruiter.id
#dim(dat_a)

self = dat_a$smallnet
#impute missing self reported degree
self[is.na(self)] = sample(na.omit(self), size = sum(is.na(self)), replace = T)

dat_a$network.size.variable = self
#colnames(dat)[dname == "SmallNet"] <- "network.size.variable"

#added
#dat_a$recruiter.id[dat_a$recruiter.id == "seed"] = 0

#convert to rds.data.frame
dat_1 = as.rds.data.frame(dat_a, max.coupons = 4)

#dat_1$bmsm == 1
#dat_1$miss == 0
#which(dat_1$miss == 0)
#which(dat_1$bmsm == 1)
#which(dat_1$bmsm == 1 & dat_1$miss == 0)

#Gile's SS estimate
#combined
est.ss.b = RDS.SS.estimates(dat_1, "hiv_status", subset = dat_1$bmsm == 1 & dat_1$miss == 0, N=5000)
#houston
est.ss.h = RDS.SS.estimates(dat_1, "hiv_status", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 1, N=5000)
#chicago
est.ss.c = RDS.SS.estimates(dat_1, "hiv_status", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 0, N=5000)

#combined
est.ss.b = RDS.SS.estimates(dat_1, "syphilis_active4", subset = dat_1$bmsm == 1 & dat_1$miss == 0, N=5000)
#houston
est.ss.h = RDS.SS.estimates(dat_1, "syphilis_active4", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 1, N=5000)
#chicago
est.ss.c = RDS.SS.estimates(dat_1, "syphilis_active4", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 0, N=5000)

#combined
est.ss.b = RDS.SS.estimates(dat_1, "co_hiv_syphilis4", subset = dat_1$bmsm == 1 & dat_1$miss == 0, N=5000)
#houston
est.ss.h = RDS.SS.estimates(dat_1, "co_hiv_syphilis4", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 1, N=5000)
#chicago
est.ss.c = RDS.SS.estimates(dat_1, "co_hiv_syphilis4", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 0, N=5000)

#combined
est.ss.b = RDS.SS.estimates(dat_1, "mono_hiv", subset = dat_1$bmsm == 1 & dat_1$miss == 0, N=5000)
#houston
est.ss.h = RDS.SS.estimates(dat_1, "mono_hiv", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 1, N=5000)
#chicago
est.ss.c = RDS.SS.estimates(dat_1, "mono_hiv", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 0, N=5000)

#combined
est.ss.b = RDS.SS.estimates(dat_1, "mono_syphilis4", subset = dat_1$bmsm == 1 & dat_1$miss == 0, N=5000)
#houston
est.ss.h = RDS.SS.estimates(dat_1, "mono_syphilis4", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 1, N=5000)
#chicago
est.ss.c = RDS.SS.estimates(dat_1, "mono_syphilis4", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 0, N=5000)

#combined
est.ss.b = RDS.SS.estimates(dat_1, "neither", subset = dat_1$bmsm == 1 & dat_1$miss == 0, N=5000)
#houston
est.ss.h = RDS.SS.estimates(dat_1, "neither", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 1, N=5000)
#chicago
est.ss.c = RDS.SS.estimates(dat_1, "neither", subset = dat_1$bmsm == 1 & dat_1$miss == 0 & dat_1$houston == 0, N=5000)


#######################################
colnames(dat_a)
#naive mean and confidence interval
#hiv infection
est = mean(dat_a[,"hiv_status"]) 
est
*SD: p(1-p)/n, just use the variance estimator for binomial.
est_cl=sqrt(est*(1-est)/sum(!is.na(dat_a[,"hiv_status"])))
est_cl
#95%CI
E = qnorm(.975)*est_cl;   
est
est + c(-E, E) 

#syphilis infection
est = mean(dat_a[,"syphilis_active4"]) 
est
*SD: p(1-p)/n, just use the variance estimator for binomial.
est_cl=sqrt(est*(1-est)/sum(!is.na(dat_a[,"syphilis_active4"])))
est_cl
#95%CI
E = qnorm(.975)*est_cl;   
est
est + c(-E, E) 

#co_hiv_syphilis4
est = mean(dat_a[,"co_hiv_syphilis4"]) 
est
*SD: p(1-p)/n, just use the variance estimator for binomial.
est_cl=sqrt(est*(1-est)/sum(!is.na(dat_a[,"co_hiv_syphilis4"])))
est_cl
#95%CI
E = qnorm(.975)*est_cl;   
est
est + c(-E, E) 

#mono_hiv
est = mean(dat_a[,"mono_hiv"]) 
est
*SD: p(1-p)/n, just use the variance estimator for binomial.
est_cl=sqrt(est*(1-est)/sum(!is.na(dat_a[,"mono_hiv"])))
est_cl
#95%CI
E = qnorm(.975)*est_cl;   
est
est + c(-E, E) 

#mono_syphilis4
est = mean(dat_a[,"mono_syphilis4"]) 
est
*SD: p(1-p)/n, just use the variance estimator for binomial.
est_cl=sqrt(est*(1-est)/sum(!is.na(dat_a[,"mono_syphilis4"])))
est_cl
#95%CI
E = qnorm(.975)*est_cl;   
est
est + c(-E, E) 

#neither
est = mean(dat_a[,"neither"]) 
est
*SD: p(1-p)/n, just use the variance estimator for binomial.
est_cl=sqrt(est*(1-est)/sum(!is.na(dat_a[,"neither"])))
est_cl
#95%CI
E = qnorm(.975)*est_cl;   
est
est + c(-E, E) 


#---------------------------------------------------------------------------------
*SD: p(1-p)/n, just use the variance estimator for binomial.
est_cl=sqrt(est*(1-est)/sum(!is.na(dat_1[,"hiv_status"])))
est_cl

#95%CI
E = qnorm(.975)*est_cl;   
est
est + c(-E, E) 
 
#V-H RDS-II estimate
#est.ii = RDS.II.estimates(dat_1, "hiv_status")
#est.ii

#Gile's SS estimate
est.ss = RDS.SS.estimates(dat_1, "hiv_status", N=5000)

#---------------------------------------------------------------------------------
*Ming codes
 sum(dat_1$bmsm == 1 && dat_1$houston == 1 && dat_1$miss == 0)
d = dat_1[dat_1$bmsm == 1, ]
dat_1 = as.rds.data.frame(dat, max.coupons = 4)
class(dat_1)
dat_1$houston
est.ss = RDS.SS.estimates(d, "hiv_status", N=5000)
#dname
dat$recruiter.id
dim(d)
