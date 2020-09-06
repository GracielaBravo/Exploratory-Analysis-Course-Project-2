
##Set working directory
setwd("C:/Users/Core i5/Desktop/Coursera/Data science/Course 4_ Exploratory Data Analysis")
getwd()

##Read the files
NEI <- readRDS("Project 2/summarySCC_PM25.rds")
SCC <- readRDS("Project 2/Source_Classification_Code.rds")

##Library
library(dplyr)
library(ggplot2)

######Question 6#########
##Compare emissions from motor vehicle sources in Baltimore City with emissions 
##from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
##Which city has seen greater changes over time in motor vehicle emissions?

#First subset the vehicles and then Baltimore and LA
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]
vehiclesBaltiNEI <- vehiclesNEI[vehiclesNEI$fips == "24510",]
vehiclesBaltiNEI$city <- "Baltimore City, MD"
vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips== "06037",]
vehiclesLANEI$city <- "Los Angeles County, CA"
bothNEI <- rbind(vehiclesBaltiNEI,vehiclesLANEI)

#Then summarise to put the numbers in the graphics
EmissionsBalti_LA <- summarise(group_by(bothNEI, year,city), Emissions=sum(Emissions))

#Now we plot
png("plot6.png", width=600, height=600) #saving the plot in png format

ggplot(EmissionsBalti_LA, aes(x=factor(year), y=Emissions, fill=city, label = round(Emissions))) +
    geom_bar(stat="identity") +
    facet_grid(scales="free", .~city) +
    labs(x="year", y=expression("Total PM"[2.5]*" emission (Tons)")) +
    ggtitle(expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))+
    geom_text(aes(label = round(Emissions), vjust=-0.3))+
    theme(legend.position = "none")

dev.off()

####Answer Question 6#####

##Emissions related to motor vehicle sources in Baltimore City vs Los Angeles have considerable differences.
##Los Angeles emissions greatly exceed Baltimore City emissions.
##Los Angeles hasn't cut motor vehicle emissions at all from 1999 to 2008, while Baltimore City cut emissions by more than half.