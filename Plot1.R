pm25data <- readRDS(file = "summarySCC_PM25.rds")

#first run the str() function on both variables. 
#str(pm25data)
#'data.frame':	6497651 obs. of  6 variables:
#$ fips     : chr  "09001" "09001" "09001" "09001" ...
#$ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
#$ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
#$ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
#$ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
#$ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...

#Check the integrity of data: number of NA's and number of negative numbers (negative measurements)

mean(is.na(pm25data$Emissions))
#0
sum(pm25data$Emissions[pm25data$Emissions<0], na.rm=TRUE)
#0

#Question 1. Have total emissions decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005 and 2008

#So what is asking the question is to sum up all the Emissions from all sources, from all fips
#Firstly, the object pm25total is created using the with function, in which we summon the tapply function by year.

pm25total <- with(data = pm25data, tapply(Emissions, year , sum))

#And then we plot the variables using the plot function, and save it in a png file
png(file = "plot1.png", width = 480, height = 480)
plot(x = unique(pm25data$year), y = pm25total/1000000, xlab = "Years", ylab = "Total PM2.5 in millions of tons", pch = 19, col = "blue")
title("Total PM 2.5 Emissions in the United States.")
dev.off()
#Answer: According to the plot, the total emissions in the United States have been decreasing throughout the years.

