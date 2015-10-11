# getwd()
# setwd("~/coursera/4explore/work")
# this file assumes already in proper working directory

# install/load any necessary packages:
install.packages("lubridate")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("scales")

library(lubridate)
library(ggplot2)
library(gridExtra)
library(scales)

#first download the file
fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile="power.zip")
list.files()

#next, unzip data files
unzip("power.zip",exdir="power")
# note that the exdir will be created if necessary; see help for unzip function for details

# open data
power<-read.table("power/household_power_consumption.txt", header=FALSE, col.names=c("Date", "Time", "Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"),sep=";", nrows=2880, skip=66637,stringsAsFactors = TRUE,strip.white = TRUE)
# note: b/v c large file only loaded needed lines for 2 feb dates specified

# do some basic data prep:
#convert to 2 digit string in var Date 
gsub("/2/", "/02/", power$Date)

#convert Date variable to date format
power$date<-as.Date(power$Date,"%d/%m/%Y")

#convert time variable to time format
power$time<-hms(power$Time)

# determine day of week of date (needed for #2)
power$weekday<-wday(power$date,label=TRUE)

#check if any vars have missing coded as ? (need as NA if so)
length(which(power$Date=="?"))
length(which(power$Time=="?"))
#probably are none in other variables, b/c all are coded as numeric, which would not have occurred if any ?s


# Now, recreate plots given in the assignment
# note: the assignment said that base plotting system was used to create the plots
#  however, it did not specify that system must be used, it simply said: 
# "Your task is to reconstruct the following plots below..."
# base plotting system was used for this plot because it worked well with this chart type

#1 histogram of Global_active_power
# define a sequence for tick marks & labeling, to call below
yticks<-seq(0, 1200, 200)
xticks<-seq(0,6,2)

# create basic histogram
# note, need to turn off axes (axes=FALSE) in order for 2 lines following to effectively label axes
hist(power$Global_active_power, axes=FALSE,xlim=c(0,8),ylim=c(0,1200),col="red",xlab="Global Active Power (kilowatts)", main="Global Active Power")
axis(2, at = yticks, labels = yticks)
axis(1, at=xticks, labels=xticks)
# the following website was v. helpful in how to change scale of axes:
#  http://danielmarcelino.com/changing-axis-values-in-r-plot/

# save the plot in viewing screen to PNG file:
dev.copy(png, file = "plot1.png")
dev.off()   # don't forget this last step, or won't be able to open file
