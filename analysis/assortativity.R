# Compute assortativity coefficients

rm(list=ls())

# Load libraries ---------------------------

library(dplyr)
library(network)
library(intergraph)
library(igraph)


# Read data ---------------------------

data_path <- "../Aditya_11032019/dataset_used for create_dataset_HPV1 & extract_dyad/"
dyad <- read.csv(paste0(data_path, "HPV_dyad.csv"))
dt <- read.csv(paste0(data_path, "houston_hpv.csv"), as.is = T)
att_data <- read.csv(paste0(data_path, "HPV_attribute.csv"))


# Replace missing values in each column by median

for(i in 3:ncol(dt)){
  # don't impute first two columns: "ID", "type_string" 
  dt[is.na(dt[,i]), i] <- median(dt[,i], na.rm = TRUE)
}


# Convert to network object ---------------------------

dyad_el <- as.data.frame(dyad[,c(1:2)])
dyad_el <- apply(dyad_el, c(1:2), "as.factor")
hpv_net <- network::as.network(dyad_el, directed=FALSE)

network.size(hpv_net)
network.edgecount(hpv_net)


# Check why attribute file has 143 rows but network object has 141 nodes ---------------------------

length(dt$ID) #n=143

length(att_data$caseid) #n=134

network.size(hpv_net) #n=141
hpv_net %v% "vertex.names"
length(unique(hpv_net %v% "vertex.names")) # All 141 are unique

idx.notin <- which(!(dt$ID) %in% (hpv_net %v% "vertex.names"))
(dt$ID)[idx.notin] #two participants are in the dt dataset but not in network


# Add isolates to the network object

hpv_net <- network::add.vertices(hpv_net, nv=length(idx.notin))
network::set.vertex.attribute(
  hpv_net, 
  "vertex.names",
  as.character((dt$ID)[idx.notin]),
  which(is.na(hpv_net %v% "vertex.names"))
)


# Reorder vertex ids to fill in attributes ---------------------------

new.order <- sort.int(hpv_net %v% "vertex.names", index.return = TRUE)
new.order$ix
permute.vertexIDs(hpv_net, new.order$ix)

hpv_net %v% "vertex.names"


# Add attributes to network object

hpv_net %v% "hr16" <- dt$HR_16
hpv_net %v% "hr18" <- dt$HR_18
hpv_net %v% "hr31" <- dt$HR_31
hpv_net %v% "hr33" <- dt$HR_33
hpv_net %v% "hr35" <- dt$HR_35
hpv_net %v% "hr39" <- dt$HR_39
hpv_net %v% "hr45" <- dt$HR_45
hpv_net %v% "hr51" <- dt$HR_51
hpv_net %v% "hr52" <- dt$HR_52
hpv_net %v% "hr58" <- dt$HR_58
hpv_net %v% "hr59" <- dt$HR_59
hpv_net %v% "hr68" <- dt$HR_68

hpv_net %v% "HIV" <- dt$hiv_w1
hpv_net %v% "fta" <- dt$fta_w1
hpv_net %v% "num_condomless_anal_sex_receptive_w1" <- dt$num_condomless_anal_sex_receptive_w1
hpv_net %v% "past12m_homeless_w1" <- dt$past12m_homeless_w1

# sexual identity
dt$sex.id.cat <- 
  recode(dt$sexual_identity_w1,
         "1" = "0", # Gay
         "3" = "0", # Gay
         "2" = "1", # not gay
         "4" = "1" # not gay
  )

hpv_net %v% "sex.id.cat" <- dt$sex.id.cat

# age 0 = 25 or less, 1 = 26 or more
dt$age.cat <- ifelse(dt$age_w1 < 26, 0, 1) 
hpv_net %v% "age.cat" <- dt$age.cat

# education
dt$educ.cat <- 
  recode(dt$education_w1,
         "1" = "0", #Grade K-12
         "2" = "1", #"High School or GED",
         "3" = "1", #High School or GED",
         "4" = "1", #High School or GED",
         "5" = "1", #High School or GED",
         "6" = "1", #High School or GED",
  )
hpv_net %v% "educ.cat" <- dt$educ.cat

# hr16 OR hr 18
dt$HR_16 + dt$HR_18


# delete vertices wth NA for HR attributes ----

nodes.remove <- which(
  is.na(dt$HR_16) | 
    is.na(dt$hiv_w1) | 
    is.na(dt$fta_w1) | 
    is.na(dt$num_condomless_anal_sex_receptive_w1)
)
  
hpv_net <- network::delete.vertices(hpv_net, nodes.remove)


# Convert to igraph -------------------------

hpv_ig <- asIgraph(hpv_net)
ecount(hpv_ig)
vcount(hpv_ig)


# Compute assortativity coefficients ---------------------------

dt <- dt[-c(nodes.remove),]
assortativity(hpv_ig, dt$HR_16)
assortativity(hpv_ig, dt$HR_18)
assortativity(hpv_ig, dt$HR_31)
assortativity(hpv_ig, dt$HR_33)
assortativity(hpv_ig, dt$HR_35)
assortativity(hpv_ig, dt$HR_39)
assortativity(hpv_ig, dt$HR_45)
assortativity(hpv_ig, dt$HR_51)
assortativity(hpv_ig, dt$HR_52)
assortativity(hpv_ig, dt$HR_58)
assortativity(hpv_ig, dt$HR_59)
assortativity(hpv_ig, dt$HR_68)

assortativity(hpv_ig, dt$hiv_w1)

assortativity(hpv_ig, dt$fta_w1)

assortativity(hpv_ig, dt$num_condomless_anal_sex_receptive_w1)

assortativity(hpv_ig, dt$age.cat)


assortativity(hpv_ig, dt$sex.id.cat)

assortativity(hpv_ig, dt$educ.cat)

assortativity(hpv_ig, dt$past12m_homeless_w1)


# Compute assortativity coefficients for HIV-pos subgraph ---------------------------

# compute positive subgraph
hivpos.vid <- which(dt$hiv_w1 == 1)
hpv_ig_hivpos <- induced_subgraph(hpv_ig, vids = hivpos.vid)

# filter dt for attributes of HIV+ individuals
dt.hivpos <-
  dt %>%
  filter(hiv_w1 == 1)

# investigate structure of above objects
ecount(hpv_ig_hivpos)
vcount(hpv_ig_hivpos)

dim(dt.hivpos)

assortativity(hpv_ig_hivpos, dt.hivpos$HR_16)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_18)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_31)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_33)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_35)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_39)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_45)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_51)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_52)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_58)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_59)
assortativity(hpv_ig_hivpos, dt.hivpos$HR_68)

assortativity(hpv_ig_hivpos, dt.hivpos$hiv_w1)

assortativity(hpv_ig_hivpos, dt.hivpos$fta_w1)

assortativity(hpv_ig_hivpos, dt.hivpos$num_condomless_anal_sex_receptive_w1)

assortativity(hpv_ig_hivpos, dt.hivpos$age.cat)


assortativity(hpv_ig_hivpos, dt.hivpos$sex.id.cat)

assortativity(hpv_ig_hivpos, dt.hivpos$educ.cat)

assortativity(hpv_ig_hivpos, dt.hivpos$past12m_homeless_w1)

# Compute assortativity coefficients for HIV-neg subgraph ---------------------------

# compute negative subgraph
hivneg.vid <- which(dt$hiv_w1 == 0)
hpv_ig_hivneg <- induced_subgraph(hpv_ig, vids = hivneg.vid)

# filter dt for attributes of HIV+ individuals
dt.hivneg <-
  dt %>%
  filter(hiv_w1 == 0)

# investigate structure of above objects
ecount(hpv_ig_hivneg)
vcount(hpv_ig_hivneg)

dim(dt.hivneg)

assortativity(hpv_ig_hivneg, dt.hivneg$HR_16)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_18)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_31)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_33)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_35)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_39)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_45)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_51)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_52)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_58)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_59)
assortativity(hpv_ig_hivneg, dt.hivneg$HR_68)

assortativity(hpv_ig_hivneg, dt.hivneg$hiv_w1)

assortativity(hpv_ig_hivneg, dt.hivneg$fta_w1)

assortativity(hpv_ig_hivneg, dt.hivneg$num_condomless_anal_sex_receptive_w1)

assortativity(hpv_ig_hivneg, dt.hivneg$age.cat)


assortativity(hpv_ig_hivneg, dt.hivneg$sex.id.cat)

assortativity(hpv_ig_hivneg, dt.hivneg$educ.cat)

assortativity(hpv_ig_hivneg, dt.hivneg$past12m_homeless_w1)


# Save R-binary ---------------------------

save.image(file="assortativity.RData")

#done