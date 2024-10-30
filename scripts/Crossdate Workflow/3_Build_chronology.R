#Script by: Jocelyn Garcia, script builds off of previously written Scripts 1_Ring_width_read_and_clean, and 2_Crossdate, and DendroHelpTutorialCode 
#Written by Brendon Reidy, Christy Rollinson, and Dr. Stockton Maxwell
#Project: Living Collections Wood Archive 
#Purpose: build a useable chronolgy for the use of the Forest Ecology Lab, users should be able to discern a pattern of tree ring sizes from chrnonolgy 
# This scrip utilizes the dplR package (https://rdrr.io/cran/dplR/)to read files containing ringwidth measurments
# and their associated meta data house on the Forest Ecology Lab google drive. It then puts these data into 
# a form usable for analysis. 

library(dplR)
library(TRADER)
library(graphics)
library(utils)
library(ggplot2) 

path.google <- "~/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/My Drive/LivingCollections_WoodArchive/"
path.dat <- file.path(path.google, "Data/RAW Ring Width Series/Quercus RW Tridas 2024-07-16")
path.out <- file.path(path.google, "Data/Combined Ring Width Series/Quercus")

#building crn with AR model, this produces a residual crn
grow.crn <- chron(x = combined.rwl, prefix = "", biweight = TRUE, prewhiten = TRUE)
#plot crn
crn.plot(x = grow.crn[,-1], add.spline = TRUE, nyrs = NULL, f = 0.5, crn.line.col='grey50',
         spline.line.col='red', samp.depth.col='grey90', samp.depth.border.col='grey80',
         crn.lwd=1, spline.lwd=2.0, abline.pos=1, abline.col='black', abline.lty=1,abline.lwd=1,
         xlab="Time", ylab="RWI")
write.csv(grow.crn,  file.path(path.xdate,"chronology.csv"), row.names=T)
