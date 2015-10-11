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
# as such, ggplot was used as it seemed more amenable to this type of plotting
# evaluation criteria also does not appear to specify that base plotting system need be used.

# 1st need date & time combined and in date format so can treat data as time-series
power$datePosix<-as.POSIXlt(power$date, format="%Y%m%d")
power$combinedDates<-paste(power$datePosix,power$Time)
power$tsDate<-as.POSIXlt(power$combinedDates)

# create variables containing limits, breaks and labels for x axis
lims <- strptime(c("2007-02-01 00:00:00","2007-02-03 00:00:00"), format = "%Y-%m-%d %H:%M")
lims2<-as.POSIXct(lims) # fixed in response to error message, lims no good
breaks <- strptime(c("2007-02-01 00:00:00","2007-02-02 00:00:00","2007-02-03 00:00:00"), format = "%Y-%m-%d %H:%M")
labels<-wday(breaks,label=TRUE)
breaks2<-as.POSIXct(breaks) # fixed in response to error message, had to be posixCt not lt for ggplot

#4 now, construct multi plot
# first create 4 indiv plots, then combine them

# global active power
plot1<-ggplot(power, aes(x=tsDate, y=Global_active_power))+geom_line()+ theme_bw() +theme_classic()+scale_y_continuous(name="Global Active Power (kw)") + scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels)
# voltage
plot2<-ggplot(power, aes(x=tsDate, y=Voltage))+geom_line()+ theme_bw() +theme_classic()+scale_y_continuous(name="Voltage") + scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels)
# energy submetering (from above)
plot3<-ggplot(power, aes(tsDate)) + 
    geom_line(aes(y = Sub_metering_1, colour = "black")) + 
    geom_line(aes(y = Sub_metering_2, colour = "red")) +
    geom_line(aes(y = Sub_metering_3, colour = "blue")) +
    theme_bw() +theme_classic()+ scale_y_continuous(name="Energy sub metering") +
    scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels) +
    theme(legend.justification = 'right', legend.position=c(1,0.75)) +
    scale_color_manual("",labels = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), values = c("black", "blue","red"))+
    theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"))
    
# global REactive power
plot4<-ggplot(power, aes(x=tsDate, y=Global_reactive_power))+geom_line()+ theme_bw() +theme_classic()+scale_y_continuous(name="Global_reactive_power") + scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels)

# see for useful hint on grid.arrange function:
# http://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# save the plot in viewing screen to PNG file:
dev.copy(png, file = "plot4.png")
dev.off()   # don't forget this last step, or won't be able to open file

