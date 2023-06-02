#Have total emissions from PM2.5 decreased in the United States from 1999 to 
#2008? Using the base plotting system, make a plot showing the total 
#PM2.5 emission from all sources for each of the years 1999, 2002, 2005, 
#and 2008. 

if(!require("dplyr")){
  install.packages("dplyr",dependencies=TRUE)
  library(dplyr)
}

if(!file.exists("Source_Classification_Code.rds")){
  fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="data.zip")
  unzip("data.zip")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(file="plot1.png")

##plot total emissions for each year

emissions_per_year<-
  NEI%>%group_by(year)%>%summarise(total=sum(Emissions))%>%as.data.frame()

with(emissions_per_year, 
     barplot(total,names.arg=emissions_per_year$year,
             xlab="Year",ylab="Total Emissions",
             main=expression("Total emissions from "*PM[2.5]*" in the U.S")))

dev.off()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#Have total emissions from PM2.5 decreased in the  Baltimore City, Maryland 
#(fips == 24510 fips == 24510) 
#from 1999 to 2008? Use the base plotting system to make a plot answering 
#this question.

if(!require("dplyr")){
  install.packages("dplyr",dependencies=TRUE)
  library(dplyr)
}

if(!file.exists("Source_Classification_Code.rds")){
  fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="data.zip")
  unzip("data.zip")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(file="plot2.png")

summary=NEI%>%subset( fips=="24510")%>%group_by(year)%>%summarise(total=sum(Emissions))%>%as.data.frame()
with(summary, barplot(total,names.arg=year,ylab="Total Emissions (tons)",xlab="Year",main=expression("Total Emissions from"*PM[2.5]*" in Baltimore, Maryland")))

dev.off()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Of the four types of sources indicated by the 
#type type (point, nonpoint, onroad, nonroad) variable, which of these four sources 
#have seen decreases in emissions 
#from 1999–2008 for Baltimore City? Which have seen increases in emissions 
#from 1999–2008? Use the ggplot2 plotting system to 
#make a plot answer this question.
if(!require("dplyr")){
  install.packages("dplyr",dependencies=TRUE)
  library(dplyr)
}

if(!require("ggplot2")){
  install.packages("ggplot2",dependencies=TRUE)
  library(ggplot2)
}

if(!file.exists("Source_Classification_Code.rds")){
  fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="data.zip")
  unzip("data.zip")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

png(file="plot3.png")

NEI=subset(NEI, fips=="24510")##get data for Baltimore City
summary=NEI%>%group_by_at(vars(year,type))%>%summarise(summation=sum(Emissions))

ggplot(summary, aes(year,summation))+geom_line(aes(color=type))+labs(x="Year",y="Total Emissions",title=expression("Total Emissions from "*PM[2.5]*" in Baltimore City"))

dev.off()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Across the United States,
#how have emissions from coal combustion-related sources changed from 
#1999–2008?
 
if(!file.exists("Source_Classification_Code.rds")){
  fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="data.zip")
  unzip("data.zip")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


png(file="plot4.png")

coal_sources = subset(SCC, grepl("Coal",Short.Name) | grepl("Coal",EI.Sector) )$SCC
NEI=subset(NEI, SCC %in% coal_sources)

summary=NEI%>%group_by(year)%>%summarise(summation=sum(Emissions))
with(summary, barplot(summation,names.arg=year, ylab="Total Emissions (tons)",xlab="Year",main=expression("Emissions From Coal Combustion Sources in the US") ))

dev.off() 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#How have emissions from motor vehicle 
#sources changed from 1999–2008 in Baltimore City?

if(!require("dplyr")){
  install.packages("dplyr",dependencies=TRUE)
  library(dplyr)
}


if(!file.exists("Source_Classification_Code.rds")){
  fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="data.zip")
  unzip("data.zip")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


png(file="plot5.png")

motor_vehicle_sources = subset(SCC, grepl("Motor Vehicle",Short.Name) )$SCC
NEI=subset(NEI, fips=="24510" & SCC %in% motor_vehicle_sources )
summary=NEI%>%group_by(year)%>%summarise(summation=sum(Emissions))
with(summary, barplot(summation,names.arg=year,
                      ylab="Total Emissions (tons)",
                      xlab="Year",
                      main="Emissions From Motor Vehicles Sources in Baltimore City"))

dev.off()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Compare emissions from motor vehicle sources in Baltimore 
#City with emissions from motor vehicle sources in Los Angeles 
#County, California ( fips == 06037 fips == 06037). 
#Which city has seen greater changes over time in motor vehicle emissions?

if(!require("dplyr")){
  install.packages("dplyr",dependencies=TRUE)
  library(dplyr)
}


if(!file.exists("Source_Classification_Code.rds")){
  fileUrl="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="data.zip")
  unzip("data.zip")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


png(file="plot6.png")

motor_vehicle_sources = subset(SCC, grepl("Motor",Short.Name) )$SCC
NEI=subset(NEI, SCC %in% motor_vehicle_sources & (fips== "24510" |fips== "06037" ))
summary=NEI%>%group_by_at(vars(year,fips))%>%summarise(summation =sum(Emissions))

summary$fips<-as.factor(summary$fips)
levels(summary$fips)<-c("Los Angeles","Baltimore")

ggplot(summary, aes(year,summation, fill=fips) )+geom_bar(stat="identity",position="dodge")+labs(x="Year",y="Total Emissions (tons)",title="Total Emissions From Motor Vehicles Sources")+ scale_x_continuous(breaks = c(1999, 2002, 2005,2008))

dev.off()

