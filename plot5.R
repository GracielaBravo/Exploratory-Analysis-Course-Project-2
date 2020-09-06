
##Set working directory
setwd("C:/Users/Core i5/Desktop/Coursera/Data science/Course 4_ Exploratory Data Analysis")
getwd()

##Read the files
NEI <- readRDS("Project 2/summarySCC_PM25.rds")
SCC <- readRDS("Project 2/Source_Classification_Code.rds")

##Library
library(dplyr)
library(ggplot2)

######Question 5#########
#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#First subset the vehicles and then Baltimore
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]
baltimoreVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips== "24510",]

#Then summarise to put the numbers in the graphics
EmissionsMotorVehiclesBalti <- summarise(group_by(baltimoreVehiclesNEI, year), Emissions=sum(Emissions))

#Now we plot

png("plot5.png", width=600, height=600) #saving the plot in png format

ggplot(EmissionsMotorVehiclesBalti,aes(x = factor(year), y = Emissions)) +
    geom_bar(stat="identity", aes(fill = year)) +
    labs(x="Year", y=expression("Total PM"[2.5]*" emission (Tons)")) + 
    ggtitle(expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))+
    geom_text(aes(label=round(Emissions), vjust=-0.3), size = 3.5)+
    theme(legend.position = "none")

dev.off()

####Answer Question 5#####

##Emissions in Baltimore City, MD related to motor vehicle sources has decreased from 1999 to 2008.

