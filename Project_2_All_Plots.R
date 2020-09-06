
##Set working directory
setwd("C:/Users/Core i5/Desktop/Coursera/Data science/Course 4_ Exploratory Data Analysis")
getwd()

##Read the files
NEI <- readRDS("Project 2/summarySCC_PM25.rds")
SCC <- readRDS("Project 2/Source_Classification_Code.rds")

##Library
library(dplyr)
library(ggplot2)

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


######Question 3#########

##Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
##which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
##Which have seen increases in emissions from 1999-2008? 
##Use the ggplot2 plotting system to make a plot answer this question.


##First filter by type and fips per year
BaltiEmissionsType <-summarise(group_by(filter(NEI, fips == "24510"), year, type), Emissions=sum(Emissions))

##Then plot with ggplot
png("plot3.png", width=600, height=600) #saving the plot in png format

ggplot(BaltiEmissionsType, aes(x = factor(year), y = round(Emissions), fill = type,
       label = round(Emissions))) + 
    geom_bar(stat = "identity")+
    ylim(0,2200)+
    facet_grid(.~ type)+
    xlab("Year")+
    ylab(expression("Total PM"[2.5]*" emissions (Tons)"))+
    ggtitle(expression("Total PM"[2.5]*" emissions in Baltimore City by source types"))+
    geom_text(aes(label=round(Emissions), vjust=-0.3), size = 3.5)

dev.off()

######Question 4#########
##Across the United States, how have emissions from 
##coal combustion-related sources changed from 1999-2008?
library(ggplot2)
library(dplyr)

# Subset coal combustion related NEI data
combuRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (combuRelated & coalRelated)
combuSCC <- SCC[coalCombustion,]$SCC
combuNEI <- NEI[NEI$SCC %in% combuSCC,]
EmissionsCoalRelated <- summarise(group_by(combuNEI, year), Emissions=sum(Emissions))

#Now we plot

png("plot4.png", width=600, height=600) #saving the plot in png format

ggplot(EmissionsCoalRelated, aes(x = factor(year), y = Emissions))+
    geom_bar(stat = "identity", aes(fill = year))+
    ylim(0,600000)+
    xlab("Year")+
    ylab(expression("Total PM"[2.5]*" emissions (Tons)"))+
    ggtitle("Emissions from coal combustion-related sources")+
    theme(legend.position = "none")+
    geom_text(aes(label=round(Emissions), vjust=-0.3), size = 3.5)

dev.off()

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

