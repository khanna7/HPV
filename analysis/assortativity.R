# Compute assortativity coefficients

# To keep all 160 below see "TO KEEP ALL 160 CASES"
# default is to delete n=24 with indequate HPV test samples

rm(list=ls())


# Load libraries ---------------------------

library(dplyr)
library(network)
library(intergraph)
library(igraph)


# Read data ---------------------------

# TO KEEP ALL 160 CASES
# COMMENT FIRST LINE AND UNCOMMENT SECOND LINE BELOW 

####################################################
load(file="data-delete-24-cases.RData") #for deleting n=24
#load(file="data-keep-all-cases.RData") #for keeping all 160 cases
####################################################

# Compute assortativity coefficients ---------------------------

assortativity(hpv_ig, hpv_net %v% "hr16") #hpv_ig should have 160 nodes
assortativity(hpv_ig, hpv_net %v% "hr18")
assortativity(hpv_ig, hpv_net %v% "hr_16_or_18")
assortativity(hpv_ig, hpv_net %v% "hr_16_and_18")
assortativity(hpv_ig, hpv_net %v% "hr31")
assortativity(hpv_ig, hpv_net %v% "hr33")
assortativity(hpv_ig, hpv_net %v% "hr35")
assortativity(hpv_ig, hpv_net %v% "hr39")
assortativity(hpv_ig, hpv_net %v% "hr45")
assortativity(hpv_ig, hpv_net %v% "hr51")
assortativity(hpv_ig, hpv_net %v% "hr52")
assortativity(hpv_ig, hpv_net %v% "hr58")
assortativity(hpv_ig, hpv_net %v% "hr59")
assortativity(hpv_ig, hpv_net %v% "hr68")

assortativity(hpv_ig, hpv_net %v% "HIV")
assortativity(hpv_ig, hpv_net %v% "fta")

assortativity(hpv_ig, hpv_net %v% "num_anal_partners_w1")
assortativity(hpv_ig, hpv_net %v% "num_anal_sex_insertive_2_w1")
assortativity(hpv_ig, hpv_net %v% "num_anal_sex_receptive_2_w1")

assortativity(hpv_ig, hpv_net %v% "age.cat")
assortativity(hpv_ig, hpv_net %v% "sex.id.cat")
assortativity(hpv_ig, hpv_net %v% "educ.cat")
assortativity(hpv_ig, hpv_net %v% "past12m_homeless_w1")


# Compute assortativity coefficients for HIV-pos subgraph ---------------------------

# compute positive subgraph
hivpos.vid <- which(hpv_net %v% "HIV" == 1)
hpv_ig_hivpos <- induced_subgraph(hpv_ig, vids = hivpos.vid)

# investigate structure of above objects
ecount(hpv_ig_hivpos)
vcount(hpv_ig_hivpos)

assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr16"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr18"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr_16_or_18"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr_16_and_18"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr31"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr33"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr35"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr39"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr45"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr51"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr52"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr58"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr59"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "hr68"))

assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "HIV")) #all HIV are 1, assort. is therefore NaN
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "fta"))

assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "num_anal_partners_w1"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "num_anal_sex_insertive_2_w1"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "num_anal_sex_receptive_2_w1"))

assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "age.cat"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "sex.id.cat"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "educ.cat"))
assortativity(hpv_ig_hivpos, vertex_attr(hpv_ig_hivpos, "past12m_homeless_w1"))

# Compute assortativity coefficients for HIV-neg subgraph ---------------------------

# compute negative subgraph
hivneg.vid <- which(hpv_net %v% "HIV" == 0)
hpv_ig_hivneg <- induced_subgraph(hpv_ig, vids = hivneg.vid)

# investigate structure of above objects
ecount(hpv_ig_hivneg)
vcount(hpv_ig_hivneg)

assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr16"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr18"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr_16_or_18"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr_16_and_18"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr31"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr33"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr35"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr39"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr45"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr51"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr52"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr58"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr59"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "hr68"))

assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "HIV")) #all HIV are 0, assort. is therefore NaN
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "fta"))

assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "num_anal_partners_w1"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "num_anal_sex_insertive_2_w1"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "num_anal_sex_receptive_2_w1"))

assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "age.cat"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "sex.id.cat"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "educ.cat"))
assortativity(hpv_ig_hivneg, vertex_attr(hpv_ig_hivneg, "past12m_homeless_w1"))


# Save R-binary ---------------------------

# TO KEEP ALL 160 CASES
# COMMENT FIRST LINE AND UNCOMMENT SECOND LINE BELOW 

####################################################
save.image(file="assortativity-data-delete-24-cases.RData")
#save.image(file="assortativity-data-keep-all-cases.RData")

#done

