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

#Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008?
#Use the base plotting system to a plot answering this question

#subset the main dataframe to the data regarding only the city of Baltimore
pm25baltimore <- subset(x = pm25data, fips == "24510")

#Asign the total of the sum per year to the variable pm25baltimoretotal
pm25baltimoretotal <- with(data = pm25baltimore, expr = tapply(pm25baltimore$Emissions, pm25baltimore$year, sum, na.rm=TRUE))

#Open the pgn device
png(file = "plot2.png", width = 480, height = 480)

#Plot usign the plot function the years in x and the total pm25 in Baltimore
plot(x = unique(pm25baltimore$year), y = pm25baltimoretotal, pch = 20, col = "red", xlab = "Years", ylab = "Total PM2.5 Emissions in tons")
title("Total PM 2.5 Emissions in Baltimore")

#close the png device
dev.off()

#Answer: Indeed, the Total PM 2.5 Emissions in Baltimore have decreased.