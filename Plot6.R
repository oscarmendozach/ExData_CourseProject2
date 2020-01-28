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

#Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California
#(fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?

#Repeating the search for the vehicles SCC codes in codes dataframe

vehiclesindex <- grep(pattern= "Vehicles", x = codes$EI.Sector)
SCCvehicles <- codes$SCC[vehiclesindex]

#load of dplyr
library(dplyr)

#first we subset the pm25data dataframe into the pm25baltimore dataframe, subsetting by fip == 24510 and SCC contained in the SCCvehicles vector
pm25baltimore <- subset(x = pm25data, fips=="24510" & SCC %in% SCCvehicles)
#we do the same with the pm25losangeles dataframe, subsetting by fip == 06037 and SCC contained in the SCCvehicles vector
pm25losangeles <- subset(x=pm25data, fips == "06037" & SCC %in% SCCvehicles) 

#using dplyr, we group the data by fip, type and year and summarise the Emissions using the sum function
pm25baltimorevehicle <- pm25baltimore %>% group_by(fips, year) %>% summarise(Emissions = sum(Emissions, na.rm=TRUE))
#the same for Los Angeles
pm25losangelesvehicle <- pm25losangeles %>% group_by(fips, year) %>% summarise(Emissions = sum(Emissions, na.rm=TRUE))

#then we integrate the dataframes in a single one
pm25vehicle <- rbind(pm25baltimorevehicle, pm25losangelesvehicle)

library(ggplot2)

#we open the png device
png(filename = "Plot6.png", width = 480, height = 480)

#we use ggplot because it's easier
#first, the ggplot object is defined with the data from the integrated dataframe. After that, the geometry is selected (with points in color as the factor fips)
#then, in order to see the trend in both places, add a smooth line using a lineal model with confidence interval se set as TRUE
#finally, the plot is faceted based on the place fips, and with free scales to make a zoom on both facets.
#We setted the scales to "free" because the difference in Emissions in huge, therefore, making it difficult to observe. 
pm25vehicleplot <- ggplot(data = pm25vehicle, aes(x = year, y = Emissions)) + geom_point(color = as.factor(pm25vehicle$fips)) + geom_smooth(method = "lm", se = TRUE) + facet_grid(fips~., scales = "free") + ggtitle("PM2.5 Emissions from vehicle sources in L.A. and Baltimore")+xlab("Years") + ylab("PM 2.5 Emissions in tons")
print(pm25vehicleplot)

dev.off()

#Answer: The PM2.5 emissions are getting lower in Baltimore rather than Los Angeles. However, more data is needed to check is the trend is positive or negative (by looking at the point from 2008)