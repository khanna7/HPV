
rm(list = ls())

setwd("C:/Users/Yucheng/OneDrive/Documents/UTH/YMAP/Data/Working_on/HPV")
setwd("C:/Users/zhaoy/OneDrive/Documents/UTH/YMAP/Data/Working_on/HPV")

city <- "houston"


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
HPV <- read.csv( "input/HPV_clean_2017-9-16.csv", as.is = T)
att <- read.csv( "input/HPV_attribute.csv", as.is = T)

All <- merge(HPV, att, by.x="ID", by.y ="caseid", all.x = TRUE)
write.csv(All,"output/HPV_attributes.csv" , row.names = F)

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

Ego_dyad1 <- c()
Alt_dyad1 <- c()
Rel_dyad1 <- c()
refer1 <- dyadize(REFER)
for (i in c(1:dim(refer1)[2])){
  Ego_dyad1[i] <- refer1[1,i]
  Alt_dyad1[i] <- refer1[2,i]
  Rel_dyad1[i] <- 1
}

Ego_dyad2 <- c()
Alt_dyad2 <- c()
Rel_dyad2 <- c()
social1 <- dyadize(SOCIAL)
for (i in c(1:dim(social1)[2])){
  Ego_dyad2[i] <- social1[1,i]
  Alt_dyad2[i] <- social1[2,i]
  Rel_dyad2[i] <- 2
}

Ego_dyad3 <- c()
Alt_dyad3 <- c()
Rel_dyad3 <- c()
sex1 <- dyadize(SEX)
for (i in c(1:dim(sex1)[2])){
  Ego_dyad3[i] <- sex1[1,i]
  Alt_dyad3[i] <- sex1[2,i]
  Rel_dyad3[i] <- 3
}

Ego_dyad <- c(Ego_dyad1,Ego_dyad2,Ego_dyad3)
Alt_dyad <- c(Alt_dyad1,Alt_dyad2,Alt_dyad3)
Rel_dyad <- c(Rel_dyad1,Rel_dyad2,Rel_dyad3)

dyad <- data.frame(Ego = Ego_dyad, Alt = Alt_dyad, Rel = Rel_dyad)
write.csv(dyad,"output/HPV_dyad.csv" , row.names = F)




