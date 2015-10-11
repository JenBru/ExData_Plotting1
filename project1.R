getwd()
setwd("~/coursera/4explore/work")

install.packages("lubridate")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("scales")

library(lubridate)
library(ggplot2)
library(gridExtra)
library(scales)

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
#power$tsDate2<-as.Date(power$tsDate)

# use ggplot for plotting line graph
# ggplot(power, aes(x=tsDate, y=Global_active_power))+geom_line()
# this produces basic plot, need to clean up axes & labeling-- see below for full plot...

lims <- strptime(c("2007-02-01 00:00:00","2007-02-03 00:00:00"), format = "%Y-%m-%d %H:%M")
lims2<-as.POSIXct(lims) # fixed in response to error message, lims no good

#ggplot(power, aes(x=tsDate, y=Global_active_power))+geom_line()+ theme_bw() +theme_classic()+scale_y_continuous(name="Global Active Power (kw)") + scale_x_datetime(name="", limits=lims2, breaks = date_breaks(width="1 day"),labels=c(wday(date_breaks(width="1 day"))))
#using breaks = date_breaks(width="1 day") was not working in conjunction with labels. 
#error msg was " Breaks and labels are different lengths"
#-- could not determine a way to fix, so tried this way instead:

breaks <- strptime(c("2007-02-01 00:00:00","2007-02-02 00:00:00","2007-02-03 00:00:00"), format = "%Y-%m-%d %H:%M")
labels<-wday(breaks,label=TRUE)
breaks2<-as.POSIXct(breaks) # fixed in response to error message, had to be posixCt not lt for ggplot

ggplot(power, aes(x=tsDate, y=Global_active_power))+geom_line()+ theme_bw() +theme_classic()+scale_y_continuous(name="Global Active Power (kw)") + scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels)
# hooray, it worked :)


#3 multi line plots
ggplot(power, aes(tsDate)) + 
    geom_line(aes(y = Sub_metering_1, colour = "black")) + 
    geom_line(aes(y = Sub_metering_2, colour = "red")) +
    geom_line(aes(y = Sub_metering_3, colour = "blue")) +
    theme_bw() +theme_classic()+ scale_y_continuous(name="Energy submetering") +
    scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels) +
    theme(legend.justification = 'right', legend.position=c(1,0.85)) +
    scale_color_manual("",labels = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), values = c("black", "blue","red"))+
    theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"))+
    theme(legend.background= element_rect(fill=NA,color="black", linetype="solid" ))

# this was helpful resource for panel border: http://stackoverflow.com/questions/22151085/problems-with-panel-border-in-ggplot
# for legend box lines: adapted the above and reviewed http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
# this produces basic plot, need to clean up axes & labeling & legend


#4 multi plot
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
    theme_bw() +theme_classic()+ scale_y_continuous(name="Energy submetering") +
    scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels) +
    theme(legend.justification = 'right', legend.position=c(1,0.75)) +
    scale_color_manual("",labels = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), values = c("black", "blue","red"))+
    theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"))
    
# global REactive power
plot4<-ggplot(power, aes(x=tsDate, y=Global_reactive_power))+geom_line()+ theme_bw() +theme_classic()+scale_y_continuous(name="Global_reactive_power") + scale_x_datetime(name="", limits=lims2, breaks=breaks2,labels=labels)

# see for useful hint on grid.arrange function:
# http://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


