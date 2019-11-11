
install.packages("dplyr")
library(dplyr)
###################################################################################
dat = tbl_df(read.csv("C:/Users/Administrator/Desktop/houston.csv", as.is = T))

#----------does not need anymore if the recruiter.id is present-------------
ref = read.table("C:/Users/Administrator/Desktop/refer_1.txt", header = T, row.names = 1, check.names = F)
all(rownames(ref) == colnames(ref))
# 
list_r = apply(ref, MARGIN = 2, function(c) rownames(ref)[which(c == 1)])
recruiter.id = sapply(list_r, function(x) {ifelse(length(x)>0, x, "seed")})
cbind(dat$wave, recruiter.id)
#---------------------------------------------------------------------------

#rename columns
(dname = colnames(dat))
colnames(dat)[dname == "caseid"] <- "id"

dat$recruiter.id = recruiter.id

colnames(dat)[dname == "referred_by"] <- "recruiter.id"
dat$recruiter.id[which(is.na(dat$recruiter.id))] = "seed"

self = dat$SmallNet
#impute missing self reported degree
self[is.na(self)] = sample(na.omit(self), size = sum(is.na(self)), replace = T)

dat$network.size.variable = self
#colnames(dat)[dname == "SmallNet"] <- "network.size.variable"

#dat$has_insurance = ifelse(dat$insurance_type > 0, 1, 0)

#convert to rds.data.frame
d = as.rds.data.frame(dat, max.coupons = 4)

#conduct V-H estimate
est.ii = RDS.II.estimates(d, "any_type", N=5000)
est.ii
rds.I.weights(d, "any_type", N = 136)
write.table(dat,file="C:/Users/Administrator/Desktop/houston_3.csv")