# Code for graphs associated with Miranda Chiong's REU project
# Steps have been largely copied from this doc: https://docs.google.com/document/d/1cBrNMVzRvO8HOBFZUrbWhPAdppvrLDYoRz0gSt-2cVg/edit

# 3. Graph Establishment Conditions ----
#  - Plan A: Originally wanted to compare trees with known establishment vs. unknown, but we don't have good enough data
#  - **Plan B** [what we're going with]: Describing establishment Conditions
#  - 3.1. Describing the variability in year of establishment --> are they from the same year or are there groups before/after certain events like the Arb's founding or when the region was settled?
#.     -- CR Note: This is the piece that will really be of most interest to folks at the Arboretum
#  - 3.2. Describe the variability in growth rates during establishment --> is it a pretty tight/normal distribution or super variable?
#  -- [[Added from CR: can look at the *trend* in initial growth and see if it's declining (sign of geometric decline; typical of open grown) or increasing or stead]]
library(ggplot2)

path.out <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Analysis-Output"

# # # # # # # 
# Load in the data
# # # # # # # 
dfTree <- read.csv(file.path(path.out, "TreeData_toAnalyze.csv"))
dfTree$taxon <- as.factor(dfTree$taxon)
summary(dfTree)

dfTree[dfTree$BAI.first10>1e3 & !is.na(dfTree$BAI.first10),] # finding the outlier that shows up in some graphs

# Creating a simplified genus & species columns for us
dfTree[,c("genus", "sp.epith", "authority")] <- NA
for(i in 1:nrow(dfTree)){
  # Splitting the taxon field into its components
  taxParse <- strsplit(paste(dfTree$taxon[i]), split=" ")[[1]] # Not sure why we need paste today, but so it goes
  dfTree$genus[i] <- taxParse[1]
  if(length(taxParse)>=2) dfTree$sp.epith[i] <- taxParse[2]
  if(length(taxParse)>=3) dfTree$authority[i] <- taxParse[3]
}
dfTree$genus <- as.factor(dfTree$genus)
dfTree$sp.epith <- as.factor(dfTree$sp.epith)
dfTree$authority <- as.factor(dfTree$authority)

# Subsetting to just the trees old enough to analyze
dfTree <- dfTree[!is.na(dfTree$xDate.rho),]
summary(dfTree)

# Checking to see what our species distribution is
summary(dfTree$sp.epith)
# # # # # # # 

# # # # # # # 
# 3. Graph Establishment Conditions ----
#  - **Plan B** [what we're going with]: Describing establishment Conditions
#  - 3.1. Describing the variability in year of establishment --> are they from the same year or are there groups before/after certain events like the Arb's founding or when the region was settled?
#.     -- CR Note: This is the piece that will really be of most interest to folks at the Arboretum
#  - 3.2. Describe the variability in growth rates during establishment --> is it a pretty tight/normal distribution or super variable?
#  -- [[Added from CR: can look at the *trend* in initial growth and see if it's declining (sign of geometric decline; typical of open grown) or increasing or stead]]
# # # # # # # 
png(file.path(path.out, "EstablishmentYears_byPith.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree) +
  geom_histogram(aes(x=year.first, fill=pith.present), binwidth=10) +
  labs(fill="Pith Present", x="First Year", y="count") +
  theme_bw()
dev.off()
  
png(file.path(path.out, "EstablishmentYears_bySpp-All.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree) +
  geom_histogram(aes(x=year.first, fill=sp.epith), binwidth=10) +
  labs(fill="Species", x="First Year", y="count") +
  theme_bw()
dev.off()

png(file.path(path.out, "EstablishmentYears_bySpp-PithOnly.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree[!is.na(dfTree$pith.present) & dfTree$pith.present,]) +
  geom_histogram(aes(x=year.first, fill=sp.epith), binwidth=10) +
  labs(fill="Species", x="First Year", y="count") +
  theme_bw()
dev.off()


png(file.path(path.out, "EstablishmentRW-10yrs_byPith.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree) +
  geom_histogram(aes(x=RW.first10, fill=pith.present)) +
  labs(fill="Pith Present", x="Mean Ring Width (mm)", y="count") +
  theme_bw()
dev.off()

png(file.path(path.out, "EstablishmentRW-10yrs_bySpp-All.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree) +
  geom_histogram(aes(x=RW.first10, fill=sp.epith)) +
  labs(fill="Species", x="Mean Ring Width (mm)", y="count") +
  theme_bw()
dev.off()

png(file.path(path.out, "EstablishmentRW-10yrs_bySpp-PithOnly.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree[!is.na(dfTree$pith.present) & dfTree$pith.present,]) +
  geom_histogram(aes(x=RW.first10, fill=sp.epith)) +
  labs(fill="Species", x="Mean Ring Width (mm)", y="count") +
  theme_bw()
dev.off()


png(file.path(path.out, "EstablishmentBAI-10yrs_byPith.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree) +
  geom_histogram(aes(x=BAI.first10, fill=pith.present)) +
  labs(fill="Pith Present", x="Mean Basal Area Inc. (mm2)", y="count") +
  theme_bw()
dev.off()

png(file.path(path.out, "EstablishmentBAI-10yrs_bySpp-All.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree) +
  geom_histogram(aes(x=BAI.first10, fill=sp.epith)) +
  labs(fill="Species", x="Mean Basal Area Inc. (mm2)", y="count") +
  theme_bw()
dev.off()

png(file.path(path.out, "EstablishmentBAI-10yrs_bySpp-PithOnly.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree[!is.na(dfTree$pith.present) & dfTree$pith.present,]) +
  geom_histogram(aes(x=BAI.first10, fill=sp.epith)) +
  labs(fill="Species", x="Mean Basal Area Inc. (mm2)", y="count") +
  theme_bw()
dev.off()



png(file.path(path.out, "EstablishmentBAI10_vs_Year_bySpp-PithOnly.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree[!is.na(dfTree$pith.present) & dfTree$pith.present,]) +
  geom_point(aes(x=year.first, y=BAI.first10, color=sp.epith)) +
  labs(fill="Species", x = "First Year", y="Mean Basal Area Inc. (mm2)") +
  theme_bw()
dev.off()

png(file.path(path.out, "EstablishmentBAI10_vs_Year_bySpp-All.png"), height=6, width=8, units="in", res=360)
ggplot(data=dfTree[,]) +
  geom_point(aes(x=year.first, y=BAI.first10, color=sp.epith)) +
  labs(fill="Species", x = "First Year", y="Mean Basal Area Inc. (mm2)") +
  theme_bw()
dev.off()


ggplot(data=dfTree[,]) +
  geom_point(aes(x=year.first, y=BAI.slope20, color=sp.epith)) +
  labs(fill="Species", x = "First Year", y="BAI slope (mm2)") +
  theme_bw()

# # # # # # # 

