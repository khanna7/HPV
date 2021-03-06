# Missing data imputation

# To keep all 160 below see "TO KEEP ALL 160 CASES"
# default is to delete n=24 with indequate HPV test samples

rm(list=ls()) 


# Load libraries ---------------------------

library(dplyr)
library(network)
library(intergraph)
library(igraph)


# Read data ---------------------------


load(file="unweighted-desriptives.RData")

# Replace missing values in each column by median

# UNCOMMENT BELOW TO KEEP ALL 160 CASES
# ####################################################
# for(i in 3:ncol(dt)){
# #don't impute first two columns: "ID", "type_string"
# dt[is.na(dt[,i]), i] <- median(dt[,i], na.rm = TRUE)
# }
# #####################################################

# Convert to network object ---------------------------

dyad_el <- as.data.frame(dyad[,c(1:2)])
dyad_mat <- as.matrix(dyad[,c(1:2)])
dyad_el <- apply(dyad_el, c(1:2), "as.factor")

attr(dyad_el, "n") <- length(dt$caseid)

hpv_net <- network::as.network(dyad_el, directed = FALSE)
vertex.names <- network.vertex.names(hpv_net)

isolates_ids <- dt$caseid[c( 
  ###code below reindexes so that isolates are currently ordered in the network,
  ### and degrees of all vertices are recorded correctly
  intersect(which(!dt$caseid %in% dyad[,1]), which(!dt$caseid %in% dyad[,2])))
  ]
n.isolates <- length(isolates_ids)

vertex.names[which(duplicated(vertex.names))] <- isolates_ids
network.vertex.names(hpv_net) <- vertex.names

network.size(hpv_net)
network.edgecount(hpv_net)

### odering done

# Check ordering of nodes and corresponding degrees ---------------------------

length(dt$caseid) #n=160
network.size(hpv_net) #n=160
hpv_net %v% "vertex.names"
length(unique(hpv_net %v% "vertex.names")) # All  are unique

new.order <- sort.int(hpv_net %v% "vertex.names", index.return = TRUE)
new.order$ix
permute.vertexIDs(hpv_net, new.order$ix)

hpv_net %v% "vertex.names"
sna::degree(hpv_net)

# Add attributes to network object

hpv_net %v% "hr16" <- dt$hr_16
hpv_net %v% "hr18" <- dt$hr_18
hpv_net %v% "hr31" <- dt$hr_31
hpv_net %v% "hr33" <- dt$hr_33
hpv_net %v% "hr35" <- dt$hr_35
hpv_net %v% "hr39" <- dt$hr_39
hpv_net %v% "hr45" <- dt$hr_45
hpv_net %v% "hr51" <- dt$hr_51
hpv_net %v% "hr52" <- dt$hr_52
hpv_net %v% "hr58" <- dt$hr_58
hpv_net %v% "hr59" <- dt$hr_59
hpv_net %v% "hr68" <- dt$hr_68

hpv_net %v% "HIV" <- dt$hiv_w1
hpv_net %v% "fta" <- dt$fta_w1

hpv_net %v% "num_anal_partners_w1" <- dt$num_anal_partners_w1 #anal sex behavior
hpv_net %v% "num_anal_sex_insertive_2_w1" <- dt$num_anal_sex_insertive_2_w1
hpv_net %v% "num_anal_sex_receptive_2_w1" <- dt$num_anal_sex_receptive_2_w1

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

# greater than one partner on various anal sex behaviors 
dt$greq1_num_anal_partners_w1  <- 
  ifelse(dt$num_anal_partners_w1  >= 1, 1, 0)

dt$greq1_num_anal_sex_insertive_2_w1 <- 
  ifelse(dt$num_anal_sex_insertive_2_w1 >= 1, 1, 0)

dt$greq1_num_anal_sex_receptive_2_w1 <- 
  ifelse(dt$num_anal_sex_receptive_2_w1 >= 1, 1, 0)


##hpv_net %v% "greq1_condomless_anal_sex_receptive_w1" <- 
##dt$greq1_condomless_anal_sex_receptive_w1 

# age 0 = 25 or less, 1 = 26 or more
dt$age.cat <- ifelse(dt$age_w1 < 26, 0, 1) 
hpv_net %v% "age.cat" <- dt$age.cat

# education
dt$educ.cat <- 
  # recoded to be consistent with definition in Alan's paper.
  recode(dt$education_w1,
         "1" = "0", #Grade K-12
         "2" = "0", #"High School or GED",
         "3" = "1", #High School or GED",
         "4" = "1", #High School or GED",
         "5" = "1", #High School or GED",
         "6" = "1", #High School or GED",
  )
hpv_net %v% "educ.cat" <- dt$educ.cat

# hr16 OR hr 18
hr16_or_18 <- dt$hr_16 + dt$hr_18
dt$hr_16_or_18 <- ifelse(hr16_or_18 > 0, 1, 0)

hpv_net %v% "hr_16_or_18" <- dt$hr_16_or_18 

# hr16 and hr 18
dt$hr_16_and_18 <- ifelse(hr16_or_18 == 2, 1, 0)
hpv_net %v% "hr_16_and_18" <- dt$hr_16_and_18 

# Delete nodes with missing values

# COMMENT BELOW TO KEEP ALL 160 CASES
####################################################

nodes.remove <- which(is.na(hpv_net %v% "hr16")) #remove only 24 cases

length(nodes.remove)

hpv_net <- network::delete.vertices(hpv_net, nodes.remove)

# Impute missing values for anly remaining vertex attribuets -------------------------
 
vertex.atts.list <- network::list.vertex.attributes(hpv_net)

atts.w.nas <- list() #identify which atts have missing values still
for (i in 1:(length(vertex.atts.list))){
  if(any(is.na(hpv_net %v% vertex.atts.list[i]))){
    atts.w.nas <- c(atts.w.nas, i)
  }
}
atts.w.nas <- unlist(atts.w.nas)  
vertex.atts.list[atts.w.nas]

for(i in atts.w.nas){
  
  att.name <- vertex.atts.list[i]
  att.vals <- as.numeric(hpv_net %v% att.name)
  
  if(any(is.na(att.vals))){
    
    id.nas <- which(is.na(att.vals))
    network::set.vertex.attribute(hpv_net,
                                  att.name,
                                  median(att.vals, na.rm = T),
                                  v = id.nas
    )
  }
}

####################################################

# Convert to igraph -------------------------

hpv_ig <- asIgraph(hpv_net)
ecount(hpv_ig)
vcount(hpv_ig)

# Save R-binary ---------------------------

# TO KEEP ALL 160 CASES
# COMMENT FIRST LINE AND UNCOMMENT SECOND LINE BELOW 

####################################################
save.image(file="data-delete-24-cases.RData")
#save.image(file="data-keep-all-cases.RData")
####################################################

#done