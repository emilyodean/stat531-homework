#Emily O'Dean
#10/1/15
#Homework 2

#1
data = read.table("http://weather.unisys.com/hurricane/atlantic/2015/DANNY/track.dat", skip=3, fill=TRUE, header=FALSE)
names(data) = c("ADV", "LAT", "LON", "TIME", "WIND", "PR", "STAT1", "STAT2")
str(data)

#2
data = read.table("SuperLottoPlus.txt", fill=TRUE, na.strings="", header=TRUE, stringsAsFactors=FALSE)
data = data[-1,]
str(data)

#3
data = read.fwf("Basketball.txt", widths=c(16, 4, 3, 3, 3, 4, 5, 4), stringsAsFactors=FALSE)
str(data)

#4

#a
load("vehicles.rda")

#b
nrow(vposts)

#c
ncol(vposts)

#d
names(vposts)

#e
is.character(vposts$id)

#f
tapply(vposts$price, INDEX=vposts$type, FUN=mean, na.rm=TRUE)

#g
unique(vposts$type)

#h
table(vposts$type)

#i
table(vposts$fuel, vposts$size)

#j
max(vposts$price, na.rm=TRUE)

#k
data = as.data.frame(table(vposts$maker), stringsAsFactors=FALSE)
data[which(data$Freq==max(data$Freq)),1]

