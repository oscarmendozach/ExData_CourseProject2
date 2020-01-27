pm25data <- readRDS(file = "summarySCC_PM25.rds")
codes <- readRDS(file = "Source_Classification_Code.rds")

#first run the str() function on both variables. 
#str(pm25data)
#'data.frame':	6497651 obs. of  6 variables:
#$ fips     : chr  "09001" "09001" "09001" "09001" ...
#$ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
#$ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
#$ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
#$ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
#$ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...

#As showed before, most of the variables are "chr" type. the "type" column will first be set to a factor variable, for we will work and do summaries on this variable.

pm25data$type <- as.factor(pm25data$type)

#Check the integrity of data: number of NA's and number of negative numbers (negative measurements)

mean(is.na(pm25data$Emissions))
#0
sum(pm25data$Emissions[pm25data$Emissions<0], na.rm=TRUE)
#0

#Question 3: Of the fourtypes of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from
#1999 to 2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question

#First, the ggplot2 and dplyr packages are loaded
library(ggplot2)
library(dplyr)

#Using the dplyr package, we will filter the rows from fips == 24510, group them by type and year, and finally summarise then by total sum

pm25baltimore <-  pm25data %>% filter(fips == "24510") %>% group_by(fips, type, year) %>% summarise(Emissions=sum(Emissions, na.rm = TRUE))

#Start the plot using the function ggplot and assign it to the variable called pm25baltimoreplot
png(file = "plot3.R", width = 480, height = 480)
pm25baltimoreplot <- ggplot(data = pm25baltimore, aes(x = year, y = Emissions)) + geom_point() + facet_grid(type~.)
dev.off()

