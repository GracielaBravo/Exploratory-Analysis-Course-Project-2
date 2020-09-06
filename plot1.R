##Set working directory
setwd("C:/Users/Core i5/Desktop/Coursera/Data science/Course 4_ Exploratory Data Analysis")
getwd()

##Read the files
NEI <- readRDS("Project 2/summarySCC_PM25.rds")
SCC <- readRDS("Project 2/Source_Classification_Code.rds")

##Library
library(dplyr)

######Question 1#########

##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## First save total emissions per year
TotalEmissions <- summarise(group_by(NEI, year), Emissions = sum(Emissions))
##Then Plot
png("plot1.png", width=480, height=480) #saving the plot in png format

BarPlot = barplot(TotalEmissions$Emissions, names.arg = TotalEmissions$year, 
                  xlab = "Years", ylab = expression("Total PM"[2.5]*" emissions (Tons)"),
                  ylim=c(0,8000000),
                  col = c("red", "blue", "green", "yellow"),
                  main= expression('Total PM'[2.5]*' emissions at 1990, 2002, 2005 and 2008'))
##Add text
text(x = BarPlot, y = (TotalEmissions$Emissions), labels=round(TotalEmissions$Emissions,0),cex=1, pos = 3)

dev.off()


####Answer Question 1#####

##The total PM2.5 emissions across the United States has decreased from 1999 to 2008. 
##The total emissions stabilized from 2002 to 2005 but significantly decreased from 2005-2008.
