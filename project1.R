getwd()
setwd("~/coursera/4data/work")

fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile="power.csv")
list.files()

