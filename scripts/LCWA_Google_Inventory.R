# Checking our Google Sheet Inventory and doing some quick summary stats

# Getting the key code for the spreadsheet called "LivingCollectionsArchive_SampleDatabase" 
# This can be found in the url for a spreadsheet after the part that is [...]/spreadsheets/d and before the part that is /edit?[...]
lwcaSS <- "1iVB5--m29mLKhXUNgmc92wijQDg98L6FoKvsrJBDkRg" 

dat.samps <- data.frame(googlesheets4::read_sheet(ss=lwcaSS, sheet="LCWA Inventory"))
dat.samps$SppName <- paste(dat.samps$Genus, dat.samps$Species)
summary(dat.samps)
dim(dat.samps)

# Continuous (numeric) variables
dat.samps$Pith.Date <- as.numeric(dat.samps$Pith.Date)
dat.samps$Earliest.Ring <- as.numeric(dat.samps$Earliest.Ring)
dat.samps$Latest.Ring <- as.numeric(dat.samps$Latest.Ring)
summary(dat.samps)


# Categorical Variables
dat.samps$Genus <- as.factor(dat.samps$Genus)
summary(dat.samps$Genus)
