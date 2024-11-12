#Script by: Jocelyn Garcia, script builds off of previously written Scripts 1_Ring_width_read_and_clean, and 2_Crossdate, and DendroHelpTutorialCode 
#Written by Brendon Reidy, Christy Rollinson, and Dr. Stockton Maxwell
#Project: Living Collections Wood Archive 
#Purpose: build a useable chronolgy for the use of the Forest Ecology Lab, users should be able to discern a pattern of tree ring sizes from chrnonolgy 
# This script utilizes the dplR package (https://rdrr.io/cran/dplR/)to read files containing ringwidth measurments
# and their associated meta data house on the Forest Ecology Lab google drive. It then puts these data into 
# a form usable for analysis. 
#Resources:
#https://stackoverflow.com/questions/76510836/how-to-plot-two-sided-bar-with-text-in-middle-chart-in-r
#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

library(dplyr)
library(TRADER)
library(graphics)
library(utils)
library(ggplot2)
library(stringr)
library(ggpubr) # might need install.packages("ggpubr")

#install.packages("crayon")

#from testing on another script
#setwd("~/Documents/Personal Projects/Dendrochronolgy-practice/Dendrochronolgy script")
#path.dat <- file.path("/Users/jocelyngarcia/Documents/Personal Projects/Dendrochronolgy-practice/Dendrochronolgy script/RAW Ring Width Series/Quercus RW Tridas 2024-07-16")
#path.out <- file.path("/Users/jocelyngarcia/Documents/Personal Projects/Dendrochronolgy-practice/Dendrochronolgy script/Combined Ring Width Series/Quercus")

path.google <- "~/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/My Drive/LivingCollections_WoodArchive/"
path.dat <- file.path(path.google, "Data/RAW Ring Width Series/Quercus RW Tridas 2024-07-16")
path.out <- file.path(path.google, "Data/Combined Ring Width Series/Quercus")

if(!dir.exists(path.out)) dir.create(path.out, recursive = T)

#Decide if we want to use AR Model or not 

#building crn with AR model, this produces a residual crn
#grow.crn <- chron(x = combined.rwl, prefix = "", biweight = TRUE, prewhiten = TRUE)
#plot crn
#crn.plot(x = grow.crn[,-1], add.spline = TRUE, nyrs = NULL, f = 0.5, crn.line.col='grey50',
#spline.line.col='red', samp.depth.col='grey90', samp.depth.border.col='grey80',
#crn.lwd=1, spline.lwd=2.0, abline.pos=1, abline.col='black', abline.lty=1,abline.lwd=1,
#xlab="Time", ylab="RWI")
#write.csv(grow.crn, file = "chronology.csv")
#write.csv(grow.crn, file.path(path.out,"chronology.csv"), row.names=T)

#building crn without AR model, this produces a standardized crn
grow.crn <- chron(x = grow.rwl, prefix = "", biweight = TRUE, prewhiten = FALSE)


crn.plot(x = grow.crn, add.spline = TRUE, nyrs = NULL, f = 0.5, crn.line.col='grey50',
         spline.line.col='red', samp.depth.col='grey90', samp.depth.border.col='grey80',
         crn.lwd=1, spline.lwd=2.0, abline.pos=1, abline.col='black', abline.lty=1,abline.lwd=1,
         xlab="Time", ylab="RWI")

grow.crn$years <- rownames(grow.crn)  # Add years as a new column
rownames(grow.crn) <- NULL  # Remove the row names to avoid duplication


# Now write the data frame to a CSV file
write.csv(grow.crn, file.path(path.out, "chronology.csv"), row.names = FALSE)


#write.csv(grow.crn, file.path(path.out,"chronology.csv"), row.names=T)
summary(series.metadata)
summary(combined.rwl)

data <- read.csv(file.path(path.out, "chronology.csv")) #in chronology.csv res= residuals? Double check w/ Christy
summary(data)

#checking data before any plotting
print(data)

#plotting
plot(data$years,
     data$std,
     main = "Year vs Standardized RWI",
     xlab = "Year", 
     ylab = "Standardized RWI(standardized std using AR model)"
)
abline(h = 1, col = "red", lty = 2)

#attempt at barchart
barplot(data$std-1,
        names.arg = data$years, 
        xlab = "Year",
        ylab = "Standardized RWI (standardized std using AR model)", 
        col = "steelblue",
        main = "Year vs Standardized RWI",
        cex.main = 1.5, 
        cex.lab = 1.2, 
        cex.axis = 1.1)

###################################################################################
#splitting std column, will be helpful for two sided barplot
data <- data %>%
  mutate(big_rings = ifelse(std-1 >= 0, std-1, NA),
         small_rings = ifelse(std-1 < 0, std-1, NA))
print(data[, c("years", "std", "big_rings", "small_rings")]) #checking how data was split

#ommiting NA's to calculate quantile 
small_rings_no_NA <- na.omit(data$small_rings)

#print(small_rings_no_NA)
#summary(small_rings_no_NA)

#messing w/quantiles, lowest 35 percent of the small rings will be reference for marker rings
quantile_value <- quantile(small_rings_no_NA, probs = c(.10))
print(quantile_value)

#attempt at 2 sided barchart
#TO PDF (for some reason doesnt plot id pdf is running)
#pdf(file = "/Users/jocelyngarcia/Desktop/My Plot.pdf",   # The directory you want to save the file in
   #width = 20, # The width of the plot in inches
   #height = 9.5) # The height of the plot in inches


axis_margin <- 4
data$years <- factor(data$years)

get_color <- function(value) {
  ifelse(is.na(value), "gray20", 
         ifelse(value < quantile_value, "indianred4", "darkslategray")) # for marker rings, pick value to sort by 
}

# Apply the custom function to create a vector of colors
text_colors <- get_color(data$small_rings)

#Bottom Graph
p1 <- ggplot(data, aes(x = years, y = small_rings)) +
  geom_col(width = .8, fill = text_colors) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(position = "top")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 7, angle = 85, hjust = -0.1, margin = margin(b = -2),color = text_colors),
    axis.title.y = element_text(size=7),
    axis.text.y.left = element_text(size=7),
    
    plot.margin = margin(3, 3, 3, 3),
    panel.background = element_rect(fill = "white", colour = "white")
  )

# Top graph
p2 <- ggplot(data, aes(x = years, y = big_rings)) +
  geom_col(width = .8, fill = "gray20") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=7),
    axis.text.y = element_text(size = 7.25),,
   
    plot.margin = margin(3, 3, 3, 3),
    panel.background = element_rect(fill = "white", colour = "white")
  )
# Combine plots
ggarrange(p2, p1, ncol = 1, nrow = 2)

###################################################################################
#Printing Marker Rings
#Finding rows where small_rings_no_NA is less than the quantile
rows_under_quantile <- which(data$small_rings < quantile_value & !is.na(data$small_rings))
#print(rows_under_quantile)

result <- data[rows_under_quantile, c("years", "small_rings")]
print("Marker Rings Years & Values:")
print(result) # should all be less than quantile_value

###################################################################################
#dev.off()

