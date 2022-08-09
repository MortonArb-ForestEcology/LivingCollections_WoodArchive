#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collection Wood Archive
# Purpose: Comparing the accession ID of Living Collection Archive against Herbarium Vouchers
# Inputs: Living Collections Wood Archive google sheet
#         Herbarium voucher extractions csv
# Outputs: Rewritten version of the LCWA sheet that includes updated herbarium voucher status
# Notes: This script will read in the most recent herbarium voucher extraction. This extraction will need to be updated occasionally 
#-----------------------------------------------------------------------------------------------------------------------------------#
library(dplyr)
library(tidyr)
library(googlesheets4)

#Soft-coded file path
path.data <- "G:/My Drive/LivingCollections_WoodArchive/"

#Retrieving herbarium list and making it only include species and accession number
#This part is currently soft coded until we get a consistent output file name from Brahms
herb <- read.csv(file.path(path.data,"HerbariumVoucher_Extractions/Extractions/Export20220802_190854.csv"))
herb.num <- subset(herb, select = c("LivingCollectionSource", "CalcFullName"))
colnames(herb.num) <- c("PlantID", "FULLNAME")
herb.num$PlantID <- as.character(herb.num$PlantID)
herb.num$AccessionID <- gsub("\\*.*", "", herb.num$PlantID)
herb.num$AccessionID <- as.character(herb.num$AccessionID)

#retrieving wood archive list and making it only include species, accession number, and voucher status
wood.dat <- read_sheet("1iVB5--m29mLKhXUNgmc92wijQDg98L6FoKvsrJBDkRg", sheet = "LCWA Inventory")
wood.df <- data.frame(wood.dat)
wood.num <- wood.df[!is.na(wood.df$Genus),]
wood.num$PlantID <- as.character(wood.num$PlantID)
wood.num$AccessionID <- as.character(wood.num$AccessionID)

#Checking for a voucher of the exact sample or a member from the same accession
for(i in 1:nrow(wood.num)){
  if(wood.num[i, "PlantID"] %in% unique(herb.num$PlantID)){
    wood.num[i, "Herbarium.Check"] <- "Yes"
  } else if(wood.num[i, "AccessionID"] %in% unique(herb.num$AccessionID)){
    wood.num[i, "Herbarium.Check"] <- "Accession-proxy"
  } else {
    wood.num[i, "Herbarium.Check"] <- "No"
  }
}

#Ideally we would be able to write straight to the googlesheet but googlesheets4 package can't do that currently
#IF that changes and we can write directly, it would make this faster and easier

#removing samples we already know have a voucher
herb.vouch <- wood.num %>% filter(is.na(Herbarium.Voucher) | Herbarium.Voucher != Herbarium.Check)

herb.vouch <- subset(herb.vouch, select = c("PlantID", "AccessionID", "Herbarium.Voucher", "Herbarium.Check"))
View(herb.vouch)


