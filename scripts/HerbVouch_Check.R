#Code for comparing the accession ID of Living Collection Archive against Herbarium Vouchers
# This script currently only pulls from a csv that hasn't been updated as of 11-19-2019


library(dplyr)
library(tidyr)
library(googlesheets4)

path.data <- "G:/My Drive/LivingCollections_WoodArchive/"

setwd(path.data)

#retrieving herbarium list and making it only include species and accession number
herb <- read.csv("greenHERBSPM-COLLEXTRACT_19-11-2019_at_16-13-17.csv")
herb.num <- subset(herb, select = c(35,69))
colnames(herb.num) <- c("Species_Info","AccessionID")
herb.num$AccessionID <- as.character(herb.num$AccessionID)
str(herb.num)

#retrieving wood archive list and making it only include species, accession number, and voucher status
wood.df <- sheets_find("LivingCollectionsArchive_Sample_Database")
wood.dat <- data.frame(sheets_read(wood.df))
wood.num <- subset(wood.dat, select = c(2,3,4,5,15))
str(wood.num)

herb.v <- inner_join(wood.num, herb.num)

#removing repeats caused by multiple specimens (fruit, flower, veg) from one plant and ones we already know have a voucher
herb.v <- unique(herb.vouch)
herb.vouch <- subset(herb.vouch, herb.vouch$Herbarium.Voucher!="Yes")




