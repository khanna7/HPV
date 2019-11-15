# Compute assortativity coefficients

# Load libraries ---------------------------

library(dplyr)
library(network)
library(intergraph)
library(igraph)


# Read data ---------------------------

data_path <- "../Aditya_11032019/dataset_used for create_dataset_HPV1 & extract_dyad/"
dyad <- read.csv(paste0(data_path, "HPV_dyad.csv"))


# Convert to network object ---------------------------

dyad_el <- as.data.frame(dyad[,c(1:2)])
dyad_el <- apply(dyad_el, c(1:2), "as.factor")
hpv_net <- network::as.network(dyad_el, directed=FALSE)

network.size(hpv_net)
network.edgecount(hpv_net)


# Check why attribute file has 143 rows but network object has 143 nodes ---------------------------



# Add risk attributes ---------------------------



# Convert to igraph ---------------------------




# Compute assortativity coefficients ---------------------------



# done
