pm25data <- readRDS(file = "summarySCC_PM25.rds")
codes <- readRDS(file = "Source_Classification_Code.rds")

#first run the str() function. 
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

#Question 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from
#1999 to 2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question

#First, the ggplot2 and dplyr packages are loaded
library(ggplot2)
library(dplyr)

#Using the dplyr package, we will filter the rows from fips == 24510, group them by type and year, and finally summarise then by total sum

pm25baltimore <-  pm25data %>% filter(fips == "24510") %>% group_by(fips, type, year) %>% summarise(Emissions=sum(Emissions, na.rm = TRUE))

#for quality control, print the pm25baltimore dataframe

print(pm25baltimore)

#Start the plot device png()
png(file = "plot3.png", width = 480, height = 480)
#Using the function ggplot,  assign it to the variable called pm25baltimoreplot
pm25baltimoreplot <- ggplot(data = pm25baltimore, aes(x = year, y = Emissions)) + geom_point() + facet_grid(type~.) + ggtitle("PM 2.5 Emissions in Baltimore by type of source") + ylab("PM 2.5 Emissions in tons")
#Print the ggplot object
print(pm25baltimoreplot)
#disconnect the plot device
dev.off()

#Answer: A decrease in PM2.5 emissions has been seen in all the sources. However, not all of them share the same behaviour.
#For instance, for "POINT" sources there's a huge spike in 2008.
#For "NON-ROAD" and "ON-ROAD" sources, the decrement is almost negligible.
