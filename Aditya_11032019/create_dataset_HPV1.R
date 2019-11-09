## hpv.R
## purpuse: create hpv dataset and visualization
## created: 10/31/2016
## modified: 10/31/2016

rm(list = ls())

setwd("C:/Users/Yucheng/OneDrive/Documents/UTH/YMAP/Data/Working_on/HPV")
setwd("C:/Users/zhaoy/OneDrive/Documents/UTH/YMAP/Data/Working_on/HPV")

# INIT -------------------------------------------------------------------------
## Parameters
city <- "houston"
ewidth <- 0.75
esize <- 0.50
cex <- 1.25

## Load packages
library(igraph)

## functions
autocurve.edges2 <-function (graph, start = 0.5) {
  cm <- count.multiple(graph)
  mut <-is.mutual(graph)  #are connections mutual?
  el <- apply(get.edgelist(graph, names = FALSE), 1, paste,
              collapse = ":")
  ord <- order(el)
  res <- numeric(length(ord))
  p <- 1
  while (p <= length(res)) {
    m <- cm[ord[p]]
    mut.obs <-mut[ord[p]] #are the connections mutual for this point?
    idx <- p:(p + m - 1)
    if (m == 1 & mut.obs==FALSE) { #no mutual conn = no curve
      r <- 0
    }
    else {
      r <- seq(-start, start, length = m)
    }
    res[ord[idx]] <- r
    p <- p + m
  }
  res
}
mystar <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
         FUN=function(x, y, bg, size, nor) {
           symbols(x=x, y=y, bg=bg,
                   stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
                   add=TRUE, inches=FALSE)
         })
}

# no clipping, edges will be below the vertices anyway
add_shape("star", 
          clip = shape_noclip,
          plot = mystar, 
          parameters = list(vertex.norays = 5))

dyadize <- function(ADJ) {
  temp <- which(ADJ == 1, arr.ind = T, useNames = F)
  temp[, 1] <- rownames(ADJ)[as.integer(temp[, 1])]
  temp[, 2] <- colnames(ADJ)[as.integer(temp[, 2])]
  t(temp)
}

# Load data --------------------------------------------------------------------
## participant attributes
DATA <- read.csv(paste0("../../Dataset/ming/", city, "/P2.csv"), as.is = T)

## participant networks
REFER <- read.table(paste0("../../Dataset/ming/", city, "/P2/refer.txt"), as.is = T)
SOCIAL <- read.table(paste0("../../Dataset/ming/", city, "/P2/social.txt"), as.is = T)
SEX <- read.table(paste0("../../Dataset/ming/", city, "/P2/sex.txt"), as.is = T)

## hpv data
HPV <- read.csv( "HPV_clean_2017-9-16.csv", as.is = T)


# Merge and clean dataset ------------------------------------------------------
## convert class
HPV$ID <- as.character(HPV$ID)
DATA$participant_id <- as.character(DATA$participant_id)

## assign proper colnames
colnames(REFER) <- rownames(REFER)
colnames(SOCIAL) <- rownames(SOCIAL)
colnames(SEX) <- rownames(SEX)

## subset networks
REFER <- REFER[HPV$ID, HPV$ID]
SOCIAL <- SOCIAL[HPV$ID, HPV$ID]
SEX <- SEX[HPV$ID, HPV$ID]

## clean
HPV[HPV < 0] <- NA
HPV$num_high_risk <- ifelse(HPV$high_risk_number == ".", 0, HPV$high_risk_number)

## merge
DATAh <- dplyr::left_join(HPV, DATA, by = c("ID" = "participant_id"))

### compute wave
g <- graph_from_adjacency_matrix(as.matrix(REFER))
seeds <- DATAh[DATAh$wave == 0, "ID"]
sprouts <- DATAh[DATAh$wave != 0, "ID"]
wave_matrix <- as.data.frame(distances(g, v = seeds, to = sprouts))
DATAh[DATAh$wave != 0, "wave"] <- purrr::map_dbl(wave_matrix, function(x) ifelse(min(x) == Inf, NA, min(x)))

### get rid of network exposure computations
exp_vars <- c("exp_adj_hiv", "exp_adj_syph", "exp_adj_coinf", "exp_afilr_hiv", 
              "exp_afilr_syph", "exp_afilr_coinf", "exp_afilh_hiv", "exp_afilh_syph", 
              "exp_afilh_coinf") 
DATAh <- DATAh[, !colnames(DATAh) %in% exp_vars]

### number of people in a referral chain at the individual level: 
### So, if actor 1 belongs to a referral chain that is composed of 5 people, put 5.
g_clusters <- clusters(g)
DATAh$cluster_size <- sapply(g_clusters$membership, function(x) g_clusters$csize[x])

### indicator of unique referral chain cluster id: 
DATAh$cluster_id <- g_clusters$membership

## export dataset
write.csv(DATAh, paste0("../Dataset/hpv/", city, "_hpv.csv"), row.names = F)


# Build participant network ----------------------------------------------------
g <- graph.empty() + vertex(DATAh$ID)
g <- g + 
  edges(dyadize(REFER), color = "black", type = 1) + 
  edges(dyadize(SOCIAL), color = "green", type = 2) + 
  edges(dyadize(SEX), color = "orange", type = 3)


# Visualize participant network ------------------------------------------------
## layout
layout <- layout_with_fr(g, niter = 10000)

## node size: number of risk types
num_high_risk <- as.integer(DATAh$num_high_risk)
V(g)$size <- ifelse(is.na(num_high_risk), 3, 
                    ifelse(num_high_risk == 0, 2, (num_high_risk * 0.4) + 2))

## node color: HIV infection status

#V(g)$hr <- DATAh$high_risk_type
#V(g)$color <- ifelse(is.na(V(g)$hr) , "grey",
#              ifelse(V(g)$hr == 0, "forestgreen", 
#              ifelse(V(g)$hr == 1, "firebrick","blue")))

V(g)$shape <- rep("circle", length(V(g)$color))


## edge color: 
ecolor <- c("black", "green", "orange", "gray")
E(g)$color <- ifelse(is.na(E(g)$type), ecolor[4], 
                     ifelse(E(g)$type == 1, ecolor[1], 
                            ifelse(E(g)$type == 2, ecolor[2], 
                                   ifelse(E(g)$type == 3, ecolor[3], "violet"))))


vars1 <- c(16,18,31,33,35,39,45,51,52,56,58,59,68)
vars <- paste0("HR_",vars1)

for (var in vars){
  V(g)$hr <- DATAh[, var]
  V(g)$color <- ifelse(is.na(V(g)$hr) , "grey",
                       ifelse(V(g)$hr == 0, "green", "firebrick")) 
  
  
  
  ## plot
  png(paste0(city, "_hpv1_",var,".png"), width = 1618, height = 1000)
  plot(g,
       layout = layout,
       edge.width = ewidth,
       edge.arrow.size = esize,
       edge.curved = autocurve.edges2(g),
       vertex.label.dist = 0.25)
  
  ### legend - node size (hpv high risk)
  size_vec <- as.numeric(names(table(V(g)$size))[-1])
  scaled <- 1.5 + ((2 - 1) * (size_vec - min(size_vec)) / (max(size_vec) - min(size_vec)))
  legend(1.15, 0.3,
         y.intersp = 1,
         legend = paste0(" ", 0:7),
         col = "black",
         pch = 19,
         pt.cex = scaled, #c(1.5, 2, 2.5, 3, 3.5, 4),
         lty = 1,
         cex = 1.4,
         title = "Number of HPV Risk Types")

  legend(1.15, -0.25, 
         c(paste0("Not having High Risk ", strsplit(var,"_")[[1]][2] ," (", sum(V(g)$hr == 0, na.rm = T), ")"), 
           paste0("High Risk ",strsplit(var,"_")[[1]][2]," (", sum(V(g)$hr == 1, na.rm = T), ")"), 
           paste0("NA (", sum(is.na(V(g)$hr), na.rm = T), ")")), 
         pch = 19, 
         pt.cex = 2, 
         col = c("green","firebrick","grey"), 
         lty = 1, 
         cex = 1.4,
         title = "High Risk Type")
  
  ### legend - edge color (edge type)
  legend(1.15, -0.55, 
         legend = c("Referral", "Social", "Sex"), 
         col = ecolor[1:3], 
         lty = 1, 
         cex = 1.4,
         title = "Relationship Type")
  
  dev.off()
  graphics.off()
  
}




## plot
png(paste0(city, "_hpv1.png"), width = 1618, height = 1000)
plot(g,
     layout = layout,
     edge.width = ewidth,
     edge.arrow.size = esize,
     edge.curved = autocurve.edges2(g),
     vertex.label.dist = 0.25)

### legend - node size (hpv high risk)
size_vec <- as.numeric(names(table(V(g)$size))[-1])
scaled <- 1.5 + ((2 - 1) * (size_vec - min(size_vec)) / (max(size_vec) - min(size_vec)))
legend(1.15, 0.3,
       y.intersp = 1,
       legend = paste0(" ", 0:7),
       col = "black",
       pch = 19,
       pt.cex = scaled, #c(1.5, 2, 2.5, 3, 3.5, 4),
       lty = 1,
       cex = 1.4,
       title = "Number of HPV Risk Types")

#legend(1.25, -0.25, 
#       c(paste0("Not having HPV (", sum(V(g)$hr == 0, na.rm = T), ")"), 
#         paste0("Having no high risk type (", sum(V(g)$hr == 1, na.rm = T), ")"), 
#         paste0("Having high rish type (", sum(V(g)$hr == 2, na.rm = T), ")"), 
#         paste0("NA (", sum(is.na(V(g)$hr), na.rm = T), ")")), 
#       pch = 19, 
#       pt.cex = 2, 
#       col = c("green","firebrick","blue","grey"), 
#       lty = 1, 
#       cex = 1.4,
#       title = "High Risk Type")

legend(1.15, -0.25, 
       c(paste0("Not having Hight Rish Type 16 (", sum(V(g)$hr == 0, na.rm = T), ")"), 
         paste0("Hight Rish Type 16 (", sum(V(g)$hr == 1, na.rm = T), ")"), 
         paste0("NA (", sum(is.na(V(g)$hr), na.rm = T), ")")), 
       pch = 19, 
       pt.cex = 2, 
       col = c("green","firebrick","grey"), 
       lty = 1, 
       cex = 1.4,
       title = "High Risk Type")

### legend - edge color (edge type)
legend(1.15, -0.6, 
       legend = c("Referral", "Social", "Sex"), 
       col = ecolor[1:3], 
       lty = 1, 
       cex = 1.4,
       title = "Relationship Type")

dev.off()
graphics.off()

