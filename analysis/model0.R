# Fit ERGM Model 0

# N.B.  ---------------------------
  # This file uses setup information from "ergm-setup.R"


# Libraries ---------------------------

library(dplyr)
library(ergm)


# Load data ---------------------------

load(file="ergm-setup.RData")


# Create needed variables ---------------------------

# any HR type: "hr_hpv_any"
dt <-
  dt %>% 
  mutate(
    hr_hpv_any = if_else(
      (
        HR_16 == 1 | 
          HR_18 == 1 |
          HR_31 == 1 |                                     
          HR_33 == 1 |                                       
          HR_35 == 1 |                                     
          HR_39 == 1 |                                       
          HR_45 == 1 |                                     
          HR_51 == 1 |                                       
          HR_52 == 1 |                                     
          HR_56 == 1 |                                       
          HR_58 == 1 |                                       
          HR_59 == 1 |                                     
          HR_68 == 1
      ), 
      1, 0)
  )

table(dt[["hr_hpv_any"]], exclude=NULL)

hpv_net %v% "hr_hpv_any" <- dt$hr_hpv_any

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

factors <- c("edges", "degree(1)", "gwesp(1, fixed = T)")
form1 <- as.formula(paste0("hpv_net~", paste0(factors, collapse="+")))

age.form <- update(form1, ~. + absdiff("age"))

model0_a_age <- ergm(formula = age.form, eval.loglik = F)

model_0_age <- ergm(hpv_net ~ 
                      edges+
                      gwesp(1, fixed = T)+
                      degree(1)+
                      absdiff("age"),
                    eval.loglik = F
)
summary(model_0_age)

model_0_hr_hpv_any <- ergm(hpv_net ~ 
                             edges+
                             gwesp(1, fixed = T)+
                             degree(1)+
                             nodematch("hr_hpv_any", diff=T),
                           eval.loglik = F
)
summary(model_0_hr_hpv_any)



model_0 <- ergm(hpv_net ~ 
                    edges + 
                    absdiff("age") +
                    nodematch("any_type", diff=TRUE)+
                    nodematch("mult_hr_type")+
                    nodematch("HIV", diff=T)+
                    nodematch("fta", diff=T)+
                    nodematch("educ.cat", diff=T)+
                    nodefactor("past12m_homeless_w1", base=1)+
                    gwesp(1, fixed = T) + 
                    degree(0:1),
                eval.loglik = F
                  
)

#model_0_a
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

#model_0_b
summary(model_0_b)
gof.model_0_b <- gof(model_0_b)
p.model_0_b <- plot(gof.model_0_b)



