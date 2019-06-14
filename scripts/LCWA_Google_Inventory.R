# Checking our Google Sheet Inventory and doing some quick summary stats

lwca <- googlesheets::gs_title("LivingCollectionsArchive_Sample_Database")
dat.samps <- data.frame(googlesheets::gs_read(lwca, ws="LCWA Inventory"))
dat.samps$Year_collected <- as.numeric(substr(dat.samps$Date_collected, 1, 4))
dat.samps$SppName <- paste(dat.samps$Genus, dat.samps$Species)
summary(dat.samps)

length(unique(dat.samps$PlantID))
length(unique(dat.samps$Genus))
length(unique(dat.samps$SppName))

spp.ID

dim(dat.samps)
