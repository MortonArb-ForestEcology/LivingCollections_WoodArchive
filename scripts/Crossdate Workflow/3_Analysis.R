# Doing analyses for Miranda Chiong's REU project
# Steps have been largely copied from this doc: https://docs.google.com/document/d/1cBrNMVzRvO8HOBFZUrbWhPAdppvrLDYoRz0gSt-2cVg/edit

# Workflow: 
# 1. Determine what series/data we plan on using
#  - Crossdated (has rho value)
#  - Cut off for what is not an acceptable correlation value: p<0.05; 
#  - if multiple cores, use highest correlating
#  - Has pith (use LWCA spreadsheet)
# 2. Identify/Compute response variable
#  - **Opt A:** Raw Ring Width (have easy; no additonal calcs needed) **USING THIS ONE TO START**
#  - Opt B: Basal Area Increment (better metric of 2-dimenisional growht, but would have to calculate; likely more worthwhile if looking at more years)
#  - 2.1. re-align to be based off of age
#  - 2.2. calculate the mean for the establishment window: starting with 10 years for now (could try other windows)
# 3. Graph Establishment Conditions
#  - Plan A: Originally wanted to compare trees with known establishment vs. unknown, but we don't have good enough data
#  - **Plan B** [what we're going with]: Describing establishment Conditions
#  - 3.1. Describing the variability in year of establishment --> are they from the same year or are there groups before/after certain events like the Arb's founding or when the region was settled?
#.     -- CR Note: This is the piece that will really be of most interest to folks at the Arboretum
#  - 3.2. Describe the variability in growth rates during establishment --> is it a pretty tight/normal distribution or super variable?
#  -- [[Added from CR: can look at the *trend* in initial growth and see if it's declining (sign of geometric decline; typical of open grown) or increasing or stead]]

# # # # # # # 
# Set file paths, etc ----
# # # # # # # 
library(googlesheets4) # we need this to read google drive spreadsheets
library(ggplot2) # What Christy uses for making snazzy graphs


# Path to our data
# path.REU <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/"
path.dat <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Raw Ring Widths/organized/"
# Path to spit stuff out 
path.out <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Analysis-Output"

if(!dir.exists(path.out)) dir.create(path.out)

dir(path.dat) # Seeing what info we have available to work with

# Flagging a file that leads to an outlier in BAI.inc, which means we need to look closer at things
fBAD <- "QUERCS-134-U61-11-1-CR1b.xml" # This is our outlier in later stages
fGOOD <- "UNKNWN-UNK-169-11-1-MC1.xml" # This is one from Miranda which isn't that weird
sBAD <- gsub("-", ".", fBAD)
sGOOD <- gsub("-", ".", fGOOD)

# Reading in all the stats about what tree things are form etc
series.metadata <- read.csv(file.path(path.dat, "Series-Metadata_all.csv"))
dim(series.metadata)# Checking dimensions to get a feel for how complex a set we're working with
summary(series.metadata) # Checking to get a feel for the data
head(series.metadata) # lots of stuff is characters, so spitting out the first few rows to check on what we have

# Read in the Xdate Stats file --> this is a version where we added in rho & p-vals
statsXdate <- read.csv(file.path(path.dat, "Series-Metadata-XdateStats_all.csv")) 
dim(statsXdate) # Checking dimensions to get a feel for how complex a set we're working with
summary(statsXdate) # Checking to get a feel for the data
head(statsXdate) # lots of stuff is characters, so spitting out the first few rows to check on what we have

# Read in the ring width series
seriesRW <- read.csv(file.path(path.dat, "Series-Measurements_all.csv"), row.names=1) # Years are the row names
dim(seriesRW) # Checking the dimensions
summary(seriesRW[,1:15]) # Doing a summary of just the first couple cols because there's a LOT of them
seriesRW[1:20,c(sBAD, sGOOD)]


# Also pulling in our LWCA master google sheet since it has info we care about
sheetKey= "1iVB5--m29mLKhXUNgmc92wijQDg98L6FoKvsrJBDkRg" # This is the code that goes with the file called "LivingCollectionsArchive_Sample_Database
lcwaMaster <- googlesheets4::read_sheet(ss=sheetKey, sheet="LCWA Inventory")
summary(lcwaMaster)
lcwaMaster <- data.frame(lcwaMaster) # I hate working in tidyverse mode, so I put it in a data frame
summary(lcwaMaster)


# # # # # # # 


# # # # # # # 
# 1. Determine what series/data we plan on using ----
#  - Crossdated (has rho value)
#  - Cut off for what is not an acceptable correlation value: p<0.05; 
#  - if multiple cores, use highest correlating
#  - Has pith (use LWCA spreadsheet)
# # # # # # # 
summary(statsXdate) # double checking the info we have to work with
head(series.metadata)

# Setting up a dataframe to condense things from series to looking at stuff by the tree because we want one series per tree
dfTree <- data.frame(treeID = unique(series.metadata$treeID), site=NA, taxon=NA, year.first=NA, year.last=NA, nyr.SeriesMax=NA, pith.present=NA, file.use = NA,  xDate.rho = NA, xDate.pval = NA)

# Using a loop to go through each individual tree and find the best core
# Finding a core with multiple series for testing
for(i in 1:nrow(dfTree)){
  # i=which(dfTree$treeID=="UNK-051") # If need a test example; this one has multiple series, but none meet our criteria
  # i = which(dfTree$treeID=="UNK-217") # If need a test example; This is a good example for where only 1 is good
  # series.metadata[series.metadata$file=="QUERCS-519-544-11-3-1.xml",]
  # i = which(dfTree$treeID=="519-54*4")
  
  # Finding files that belong to this tree in our series metadata table
  rowMetaDat <- which(series.metadata$treeID==dfTree$treeID[i])
  # series.metadata[rowMetaDat,]
  
  # If for some reason we don't have any trees, lets just move to the next one 
  # (this would be VERY weird but we can deal with it later)
  if(length(rowMetaDat)==0) next 
  
  dfTree[i,c("site", "taxon")] <- series.metadata[rowMetaDat[1],c("site", "taxon")]
  
  # Seeing if there is pith on the sample
  rowMaster <- which(toupper(lcwaMaster$SpecimenID)==toupper(dfTree$treeID[i]))
  # lcwaMaster[rowMaster,]
  # If we have more than 1 sample for this tree, figure out what sample we're working with
  if(length(rowMaster)>1) {
    ind <- as.numeric(substr(metaNOW$core,1,1))
    rowMaster <- rowMaster[ind]
  }
  
  # Putting this hear so we don't skip over, but we're checking for trees that don't meet our rho criteria
  if(length(rowMaster)==1){ 
    if(is.na(lcwaMaster$Pith.Present[rowMaster])) {
      dfTree[i, "pith.present"] <- NA
    } else {
      dfTree[i, "pith.present"] <- ifelse(lcwaMaster$Pith.Present[rowMaster]=="Y", T, F)
    }
  }
  
  
  # - - - - - - 
  # If we have more than one series, we need to find the best one
  # (This is annoyingly complicated because of weird code quirks)
  # - - - - - - 
  fNow <- series.metadata$file[rowMetaDat] # These are our file names 
  fNow <- gsub("-", ".", fNow) # We can't have hyphens in column names, so we have to replace them with dots to make the names match
  
  xDateNOW <- statsXdate[statsXdate$series %in% fNow,]
  
  # If we don't have any series that crossdate (not NA & p < 0.05), skip to the next one
  rowLOOK <- which(xDateNOW$p.val<0.05) # Which rows we should look at
  if(length(rowLOOK)==0){ 
    dfTree$year.first[i] <- min(xDateNOW$first) 
    dfTree$year.last[i] <- max(xDateNOW$last)
    dfTree$nyr.SeriesMax[i] <- max(xDateNOW$year)
    # , c("year.first", "year.last", "nyr")
    next
    }
  
  if(length(rowLOOK)>1){
    rowBest <- which(xDateNOW$rho[rowLOOK]==max(xDateNOW$rho[rowLOOK]))
    rowLOOK <- rowLOOK[rowBest] # Overwriting row Look with just the single best one
  }
  # - - - - - - 
  
  # rowLook should just be 1 item now, so lets subset our xDate & metadata frames to just that one so we can be consistent
  xDateNOW <- xDateNOW[rowLOOK,]
  metaNOW <- series.metadata[rowMetaDat[rowLOOK],]

  dfTree[i,c("nyr.SeriesMax", "file.use", "xDate.rho", "xDate.pval")] <- xDateNOW[,c("year", "series", "rho", "p.val")]
  
  dfTree[i,c("year.first", "year.last")] <- metaNOW[,c("year.first", "year.last")]
  
  # dfTree[i,]
  

}

summary(dfTree)
dfTree[is.na(dfTree$pith.present),]
# unique(dfTree$pith.present)

hist(dfTree$year.first) # All first years
hist(dfTree$year.first[dfTree$pith.present]) # only those with pith

# Subsetting seriesRW to just the series we want to use
seriesRW <- seriesRW[,names(seriesRW) %in% dfTree$file.use]

# Subsetting dfTree to be just the files we're using and ordering everything the same
dfTree <- dfTree[!is.na(dfTree$file.use),]
dim(seriesRW); dim(dfTree)

seriesRW <- seriesRW[,dfTree$file.use]
head(colnames(seriesRW))
head(dfTree$file.use)
length(which(is.na(dfTree$file.use)))
# # # # # # # 

# # # # # # # 
# 2. Identify/Compute response variable ----
#  - **Opt A:** Raw Ring Width (have easy; no additonal calcs needed) **USING THIS ONE TO START**
#  - Opt B: Basal Area Increment (better metric of 2-dimensional growth, but would have to calculate; likely more worthwhile if looking at more years)
#  - 2.1. re-align to be based off of age
#  - 2.2. calculate the mean for the establishment window: starting with 10 years for now (could try other windows)
#  - 2.3 [CR] calculating BAI & adding a quick look at the slope of the line in the first 10 or 20 years
# # # # # # # 


# 2.1. re-align to be based off of age
dfAge <- array(dim=dim(seriesRW)) # Note that these aren't the same dims as se
colnames(dfAge) <- names(seriesRW)
summary(dfAge[,1:3])

for(i in 1:ncol(dfAge)){
  # There are a couple ways to do this, but I'm breaking it out into several steps for transparency
  datNow <- seriesRW[i] # Pulling out just the series we care about
  datNow <- datNow[!is.na(datNow)] # Getting rid of NAs
  dfAge[1:length(datNow),i] <- datNow
}
summary(dfAge[,1:3])
head(dfAge[,c(sBAD, sGOOD)])

# 2.2. calculate the mean for the establishment window: starting with 10 years for now (could try other windows)
dfTree$RW.first10[dfTree$file.use %in% colnames(dfAge)] <-  apply(dfAge[1:10,], 2, mean)
dfTree$RW.first20[dfTree$file.use %in% colnames(dfAge)] <-  apply(dfAge[1:20,], 2, mean)
summary(dfTree)
dfTree[dfTree$file.use %in% c(sBAD, sGOOD),]
apply(dfAge[1:10,c(sBAD, sGOOD)], 2, mean)

plot(x=dfTree$year.first, y=dfTree$RW.first10) # Quick exploratory scatter that Miranda had suggested

# [CR] calculating BAI & adding a quick look at the slope of the line in the first 10 or 20 years
baiAge <- dplR::bai.in(dplR::as.rwl(dfAge))
head(baiAge[,1:10])
plot(baiAge[,1], type="b") # Just getting a check on the data

dfTree$BAI.first10[dfTree$file.use %in% colnames(dfAge)] <-  apply(baiAge[1:10,], 2, "mean")
dfTree$BAI.first20[dfTree$file.use %in% colnames(dfAge)] <-  apply(baiAge[1:20,], 2, "mean")
plot(x=dfTree$year.first, y=dfTree$BAI.first10) # Quick exploratory scatter that Miranda had suggested

# Setting up placeholders in the data frame for some slope stats
dfTree[,c("BAI.slope10", "BAI.p10", "BAI.slope20", "BAI.p20")] <- NA
for(i in 1:ncol(baiAge)){
  dfTemp <- data.frame(age=as.numeric(rownames(baiAge)), bai=baiAge[,i])
  
  # lm = linear model = linear regression (y=mx+b); 
  # the LEFT hand side of the ~ is the DEPENDENT variable (y)
  # the RIGHT hand side of the ~ is the INDEPENDENT variable (x)
  # R will estimate the slope (m) & intercept (b) from the data iwth stats associated
  lm10 <- lm(bai ~ age, data=dfTemp[1:10,]) 
  sumLM10 <- summary(lm10)
  
  lm20 <- lm(bai ~ age, data=dfTemp[1:20,])
  # summary(lm20)
  sumLM20 <- summary(lm20)
  
  dfTree[!is.na(dfTree$file.use) & dfTree$file.use==colnames(baiAge)[i], c("BAI.slope10", "BAI.p10")] <- sumLM10$coefficients["age", c("Estimate", "Pr(>|t|)")]
  dfTree[!is.na(dfTree$file.use) & dfTree$file.use==colnames(baiAge)[i], c("BAI.slope20", "BAI.p20")] <- sumLM20$coefficients["age", c("Estimate", "Pr(>|t|)")]
  
}
summary(dfTree)

# Writing out the status of trees/series we have to work with
write.csv(dfTree, file.path(path.out, "TreeData_toAnalyze.csv"), row.names=F)

# # # # # # # 



