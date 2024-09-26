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


#Practice making graphs and stats summaries (JG)
genus_count <- table(dat.samps$Genus)
barplot(genus_count,
        main="genus count",
        xlab= "genus",
        ylab= "count",
        col = "light pink",
        las = 2)
summary(genus_count)
summary(dat.samps$Genus)

#mean, min, max
dat.samps$Year.Acquired<- as.numeric(dat.samps$Year.Acquired)
summary(dat.samps$Year.Acquired)

#boxplot of pith date
boxplot(dat.samps$Pith.Date)
summary (dat.samps$Pith.Date)
max(dat.samps$Pith.Date, na.rm=T)


