##Set working directory
setwd("C:/Users/Core i5/Desktop/Coursera/Data science/Course 4_ Exploratory Data Analysis")
getwd()

##Read the files
NEI <- readRDS("Project 2/summarySCC_PM25.rds")
SCC <- readRDS("Project 2/Source_Classification_Code.rds")

##Library
library(dplyr)
library(ggplot2)


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


####Answer Question 3#####

##Emissions from NON-ROAD, NON-POINT, and ON-ROAD sources all significantly decreased from 1999 to 2008. 
##The only source that increased slightly was from the POINT source.
