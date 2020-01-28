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

#Question 4: Across the United States, how have emissions from coal combustion related sources changed from 1999-2008?

#Firstly, we need to check the data available within the codes dataframe

View(codes)

#There, we can see that all references to the string "coal" are in the EI.Sector variable
#Basically, we need to obtain the SCC from the codes dataframe in order to sum the Emissions values
#from only the sources that use coal.

coalindex <- grep(pattern = "Coal", x = codes$EI.Sector, fixed = TRUE)

#We select the codes based on the indexed

SCCcoal <- codes$SCC[coalindex]

#We create a subset of the main dataframe, based only in the SCC codes provided

pm25coal <- subset(x = pm25data, SCC %in% SCCcoal )

#Computes the total

pm25coaltotal <- with(data = pm25coal, expr = tapply(Emissions, year, sum, na.rm = TRUE))

#Open png device
png(filename = "Plot4.png", width = 560, height = 560)

#plot the data
plot(x = unique(pm25coal$year), y = pm25coaltotal, xlab = "Years", ylab = "Total PM2.5 Emissions in tons", pch = 15, col = "black")
title("Total PM 2.5 Emissions from coal combustion-related sources in the US")
#close the graphic device
dev.off()

#Answer: The total emissions from coal related sources have drastically decline in the US
