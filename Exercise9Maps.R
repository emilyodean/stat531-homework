library(maps)
map("world")
map("usa")

mapInfo = map("state", plot=FALSE)
str(mapInfo)
mapInfo$names
map("state", regions="california")
map("county")
map("county", regions="california,san luis obispo")
map("state")



map("state")
capitals = us.cities[which(us.cities$capital==2),]
#noncapitals = us.cities[which(us.cities$capital==0),]

small = us.cities[which(us.cities$pop<=100000),]
medium = us.cities[which(us.cities$pop>100000 & us.cities$pop <= 500000),]
large = us.cities[which(us.cities$pop>500000),]

smallc = capitals[which(capitals$pop<=100000),]
mediumc = capitals[which(capitals$pop>100000 & capitals$pop <= 500000),]
largec = capitals[which(capitals$pop>500000),]

points(small$long, small$lat, pch=20, col=rgb(0,0,0,.7), cex=.5)
points(medium$long, medium$lat, pch=20, col=rgb(0,0,0,.7), cex=1.5)
points(large$long, large$lat, pch=20, col=rgb(0,0,0,.7), cex=2.5)
points(smallc$long, smallc$lat, pch=20, col=rgb(1,0,0), cex=.5)
points(mediumc$long, mediumc$lat, pch=20, col=rgb(1,0,0), cex=1.5)
points(largec$long, largec$lat, pch=20, col=rgb(1,0,0), cex=2.5)

legend("bottomleft", legend=c("<100,000 people", "100,000 - 500,000", ">500,000", "capital"),
       pch=20, col=c("black","black","black","red"), pt.cex=c(.5, 1.5, 2.5, 1.5), cex=.7)
