#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Wood Collections Archive
# Purpose: This script creates a single rwl file made up of individual measurements to prepare for crossdating in cofecha
# Inputs: Individual Ring Width Measuremtn data exported from tellervo
# Outputs: Single .rwl file containing all measurements in a series
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
# Soft-coding approach = better more flexible code, but easier to break if you don't understand what's going on
Folder <- "Individual_measurements"
PLOT <- "Tellervo_oaks"
#path is currently for PC file stream. For Mac beginning should be "/VOlumes/GOogledrive/"
path.dat <- "G:/.shortcut-targets-by-id/1cQQnkGHmgWwGh1i6_3niZvcF6ruD92Qe/URF REU 2022 Oak Decline/REU2022_AlvarezLuis/Data_REU2022_OakDecline/Ring_Width_Data/"
fplot <- dir(file.path(path.dat, Folder), ".rwl")

#Reading in all the indiviual files as one large file
for(i in 1:length(fplot)){
  # need to read in a file and rename it; i=1
  nm.new <- strsplit(fplot[i], "-")[[1]]
  nm.new <- paste0(nm.new[2:5], collapse="")
  
  fnow <- dplR::read.tucson(file.path(path.dat, Folder, fplot[i]))
  names(fnow) <- nm.new
  
  if(i==1){
    rwl.all <- fnow
  } else {
    rwl.all <- dplR::combine.rwl(rwl.all, fnow)
  }
}
summary(rwl.all)
class(rwl.all)
summary(data.frame(rwl.all))

library(dplr)
#------------------------------------------------------#
# Crossdating with dplr
#------------------------------------------------------#
dat <- rwl.all

#Initial plot of all the series
plot(dat, plot.type="spag")

#Using a series length of 60, checking for problem series
rwl.50 <- corr.rwl.seg(dat, seg.length=50, pcrit=0.01)

#Looking at a problem segment
seg.50 <- corr.series.seg(rwl=dat, series="0011111.rwl", seg.length=50)

#Zoomed in skeleton plot looking at the problem area
win <- 1800:1960
dat.yrs <- time(dat)
dat.trunc <- dat[dat.yrs %in% win, ]
ccf.30 <- ccf.series.rwl(rwl=dat.trunc, series="0011111.rwl", seg.length=30, bin.floor=50)

#Zooming in even more
win <- 1850:1900
dat.trunc <- dat[dat.yrs %in% win, ]
ccf.20 <- ccf.series.rwl(rwl=dat.trunc, series="0011111.rwl",
                         seg.length=20, bin.floor=0)

#Skeleton plot of the series vs the overal trend
xskel.ccf.plot(rwl=dat, series="0011111.rwl",
               win.start=1865, win.width=40)



#------------------------------------------#
# Cofecha file preparation
#------------------------------------------#


# Where you want to save the file
path.out <- file.path(path.dat, "Combined", PLOT)
if(!dir.exists(path.out)) dir.create(path.out, recursive=T)
# What you want the name of the file to be
# file.prefix <- "TEST"
file.prefix <- paste(PLOT, Sys.Date(), sep="_")

iter = 1
while(file.exists(file.path(path.out, paste0(file.prefix, ".rwl")))){
  iter=iter+1
  file.prefix <- paste(PLOT, Sys.Date(), iter, sep="_")
}

dplR::write.rwl(rwl.all, file.path(path.out, paste0(file.prefix, ".rwl")), long.names=T)

# Check Crossdating
xdate1 <- dplR::corr.rwl.seg(rwl.all, seg.length=30, bin.floor=0)
summary(xdate1)
xdate1$flags
# -------------
