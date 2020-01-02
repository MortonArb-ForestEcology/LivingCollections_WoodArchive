#Code for comparing the accession ID of Living Collection Archive against Herbarium Vouchers
# This script currently only pulls from a csv that hasn't been updated as of 11-19-2019


library(dplyr)
library(tidyr)
library(googlesheets4)

path.data <- "G:/My Drive/LivingCollections_WoodArchive/HerbariumVoucher_Extractions/"

setwd(path.data)

#retrieving herbarium list and making it only include species and accession number
herb <- read.csv("greenHERBSPM-COLLEXTRACT_19-11-2019_at_16-13-17.csv")
herb.num <- subset(herb, select = c("SPECIES", "FULLNAME", "LCSOURCE"))
colnames(herb.num) <- c("Species_Info","FULLNAME", "AccessionID")
herb.num$AccessionID <- as.character(herb.num$AccessionID)

#retrieving wood archive list and making it only include species, accession number, and voucher status
setwd('..')
wood.df <- sheets_find("LivingCollectionsArchive_Sample_Database")
wood.dat <- data.frame(sheets_read(wood.df))
wood.num <- subset(wood.dat, select = c("PlantID","AccessionID","Genus","Species", "Herbarium.Voucher"))

#joining them by AccessionID
herb.v <- inner_join(wood.num, herb.num)

#removing repeats caused by multiple specimens (fruit, flower, veg) from one plant 
herb.v <- unique(herb.v)
#removing samples we already know have a voucher
herb.vouch <- herb.v %>% filter(Herbarium.Voucher!="Yes" | is.na(Herbarium.Voucher))

View(herb.vouch)


