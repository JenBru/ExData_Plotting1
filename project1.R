getwd()
setwd("~/coursera/4explore/work")

install.packages("lubridate")
library(lubridate)

install.packages("ggplot2")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile="power.zip")
list.files()

#next, unzip data files
unzip("power.zip",exdir="power")
# note that the exdir will be created if necessary; see help for unzip function for details

# open data
power<-read.table("power/household_power_consumption.txt", header=FALSE, col.names=c("Date", "Time", "Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"),sep=";", nrows=2880, skip=66637,stringsAsFactors = TRUE,strip.white = TRUE)

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

#recreate plots using base plotting system

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

#2
# plot of global active power (y) vs day of week (x)

# 1st need date & time combined and in date format so can treat data as time-series
power$datePosix<-as.POSIXlt(power$date, format="%Y%m%d")
power$combinedDates<-paste(power$datePosix,power$Time)
power$tsDate<-as.POSIXlt(power$combinedDates)

# use ggplot for plotting line graph
ggplot(power, aes(x=tsDate, y=Global_active_power))+geom_line()
# this produces basic plot, need to clean up axes & labeling



#3 multi line plots
ggplot(power, aes(tsDate)) + 
    geom_line(aes(y = Sub_metering_1, colour = "black")) + 
    geom_line(aes(y = Sub_metering_2, colour = "blue")) +
    geom_line(aes(y = Sub_metering_3, colour = "red"))
# this produces basic plot, need to clean up axes & labeling & legend


#4 multi plot
# first create 4 indiv plots, then combine them

# global active power
plot1<-ggplot(power, aes(x=tsDate, y=Global_active_power))+geom_line()
# voltage
plot2<-ggplot(power, aes(x=tsDate, y=Voltage))+geom_line()
# energy submetering (from above)
plot3<-ggplot(power, aes(tsDate)) + 
    geom_line(aes(y = Sub_metering_1, colour = "black")) + 
    geom_line(aes(y = Sub_metering_2, colour = "blue")) +
    geom_line(aes(y = Sub_metering_3, colour = "red"))

# global REactive power
plot4<-ggplot(power, aes(x=tsDate, y=Global_reactive_power))+geom_line()

# see for useful hint on grid.arrange function:
# http://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# still need to fix axis ticks & labeling

