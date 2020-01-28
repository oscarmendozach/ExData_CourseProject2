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

#Question 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#After inspectioning the codes dataframe, we realized that all the vehicle sources are grouped in the "ONROAD"
#However, we can repeat the same procedure as the previous one and search for the string "Vehicles" in the codes dataframe

vehiclesindex <- grep(pattern= "Vehicles", x = codes$EI.Sector)
SCCvehicles <- codes$SCC[vehiclesindex]

#There, we can see that almost all of them are grouped by the "ONROAD" type. Nonetheless, there's one single category that 
#falls in "NONPOINT" type. The index is 1163
#The categories are
#Nonpoint
#Border Crossings /Border Crossings /Border Crossings
#Mobile - On-Road Gasoline Light Duty Vehicles
#Mobile Sources
#Border Crossings
#Which can lead to think that "Border Crossing" means vehicles that are not from the US and are crossing borders.
#Since is a motor vehicle source, we can keep this in the vector

library(dplyr)

#first we subset the pm25data dataframe into the pm25baltimore dataframe, subsetting by fip == 24510 and SCC contained in the SCCvehicles vector
pm25baltimore <- subset(x = pm25data, fips=="24510" & SCC %in% SCCvehicles) 

#using dplyr, we group the data by fip, type and year and summarise the Emissions using the sum function
pm25baltimorevehicle <- pm25baltimore %>% group_by(fips, type, year) %>% summarise(Emissions = sum(Emissions, na.rm=TRUE))

#Open the png graphic device
png(filename = "Plot5.png", height = 480, width = 480)

#Using the with function, plot the total emissions from vehicle sources
with(data = pm25baltimorevehicle, plot(x = year, y = Emissions, xlab = "Years", ylab = "Total PM2.5 Emissions in tons", pch = 9, col = "magenta"))
title("Total PM2.5 Emissions in Baltimore from vehicle sources")

#disconnect the graphic device
dev.off()
