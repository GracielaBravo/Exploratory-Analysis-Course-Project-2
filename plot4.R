##Set working directory
setwd("C:/Users/Core i5/Desktop/Coursera/Data science/Course 4_ Exploratory Data Analysis")
getwd()

##Read the files
NEI <- readRDS("Project 2/summarySCC_PM25.rds")
SCC <- readRDS("Project 2/Source_Classification_Code.rds")

##Library
library(dplyr)
library(ggplot2)

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

####Answer Question 4#####

##Emissions across the United States related to coal combustion has decreased from 1999 to 2008.

