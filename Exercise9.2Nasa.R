# 9.2 NASA

setwd("/Users/emilyodean/stat531-homework/")
load("nasaData.Rdata")

convertCoordinates = function(coordinates)
{
    newCoordinates = NULL
    firstChar = substr(coordinates, nchar(coordinates), nchar(coordinates))
    if(firstChar == "N" || firstChar == "E")
    {
        newCoordinates = substr(coordinates,1,nchar(coordinates)-1)
    } else if (firstChar == "S" || firstChar == "W")
    {
        newCoordinates = paste("-", substr(coordinates,1,nchar(coordinates)-1), sep="")
    } 
    
    return(newCoordinates)
}

convertNasaData = function(data)
{
    data$latlong = "coords"
    for(row in 1:nrow(data))
    {
        dat = data[row,]
        lat = convertCoordinates(dat$lat)
        long = convertCoordinates(dat$long)
        data[row,"lat"] = lat 
        data[row,"long"] = long
        data[row,"latlong"] = paste(long, lat, sep="/")
    }
    
    return(data)
}

plotPoints = function(latlong, type=20, size=1, color="black")
{
    long = unlist(strsplit(latlong, "/"))[1]
    lat = unlist(strsplit(latlong, "/"))[2]
    points(long, lat, pch=type, col=color, cex=size)
}

calculateAverages = function(data, values)
{
    average = data.frame(coordinates=character(0), avgtemp=numeric(0), avgstdev=numeric(0))
    for(value in values)
    {
        samePlace = subset(x=data, subset=data$latlong==value)
        avg = mean(samePlace$temp)
        stdev = sd(samePlace$temp)
        
        average = rbind(average, data.frame(coordinates=value, avgtemp=avg, avgstdev=stdev))
    }
    
    return(average)
}

plotByAvg = function(data)
{
    for(i in 1:nrow(data))
    {
        avgtemp = data[i,"avgtemp"]
        if(avgtemp < 289)
        {
            size = .5
            color = "orangered"
        } else if (avgtemp >=289 & avgtemp <296)
        {
            size = 1
            color = "orangered3"
        } else if (avgtemp >= 296)
        {
            size = 1.5
            color = "orangered4"
        }
        coords = toString(data[i,"coordinates"])
        plotPoints(coords, size=size, color=color)
    }
}

#0 - convert lat lon points to -
data = convertNasaData(nasaData)

#1
latlons = table(paste(data$long, data$lat, sep="/"))
values = names(latlons)

#2
library(maps)
map("world", xlim=c(-130,0), ylim=c(-40,40))

for(row in 1:nrow(data))
{
    plotPoints(data[row,"long"], data[row,"lat"], 20)
}

#3
#par(mfrow=c(1,1))
avgTable = calculateAverages(data, values)
map("world", xlim=c(-130,-10), ylim=c(-40,40))
plotByAvg(avgTable)

#step 2- make another function for plotting by stdev, same deal

#4 & 5
#edit calculate averages functions to include columns for month and year
#can parse these out with substr - there are always the same number of chars before and after the months and years
#can use same functions to plot them
