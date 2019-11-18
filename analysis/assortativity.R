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


# Add risk attributes to network object


hpv_net %v% "hr16" <- dt$HR_16
hpv_net %v% "hr31" <- dt$HR_18
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


# delete vertices wth NA for HR attributes ----

na.hr16 <- which(is.na(dt$HR_16))
hpv_net <- network::delete.vertices(hpv_net, na.hr16)


# Convert to igraph -------------------------

hpv_ig <- asIgraph(hpv_net)
ecount(hpv_ig)
vcount(hpv_ig)


# Compute assortativity coefficients ---------------------------

dt <- dt[-c(na.hr16),]
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


#done