##Set working directory
setwd("C:/Users/Core i5/Desktop/Coursera/Data science/Course 4_ Exploratory Data Analysis")
getwd()

##Read the files
NEI <- readRDS("Project 2/summarySCC_PM25.rds")
SCC <- readRDS("Project 2/Source_Classification_Code.rds")

##Library
library(dplyr)

######Question 2#########

##Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
##Use the base plotting system to make a plot answering this question.

#First save and filter the emissions from Baltimore per year
BaltiEmissions <-summarise(group_by(filter(NEI, fips == "24510"), year), Emissions=sum(Emissions))

#Then plot
png("plot2.png", width=480, height=480) #saving the plot in png format

BarPlot2 = barplot(BaltiEmissions$Emissions, names.arg = BaltiEmissions$year, 
                   xlab = "Years", ylab = expression("Total PM"[2.5]*" emissions (Tons)"),
                   ylim=c(0,4000),
                   col = c("red", "blue", "green", "yellow"),
                   main= expression('Total PM'[2.5]*' emissions in Baltimore City from 1999 to 2008'))
##Add text
text(x = BarPlot2, y = (BaltiEmissions$Emissions), labels=round(BaltiEmissions$Emissions,0),cex=1, pos = 3)

dev.off()


####Answer Question 2#####

##Emissions in Baltimore City, MD have decreased from 1999 to 2008, 
##but it was not a stable decrease (emissionsincrease in 2005).

