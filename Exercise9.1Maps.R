data = read.table("http://weather.unisys.com/hurricane/atlantic/2015/KATE/track.dat", skip=3, fill=TRUE, header=FALSE)
names(data) = c("ADV", "LAT", "LON", "TIME", "WIND", "PR", "STAT1", "STAT2")
str(data)

map("world", xlim=c(-90,0), ylim=c(15,60))
points(data$LON, data$LAT, pch=20)

slow = data[which(data$WIND < 50),]
medium = data[which(data$WIND >= 50 & data$WIND < 60), ]
fast = data[which(data$WIND >= 60),]


points(sort(slow$LON), slow$LAT[order(slow$LON)], pch=20, col=rgb(1,0,0,.3))
points(sort(medium$LON), medium$LAT[order(medium$LON)], pch=20, col=rgb(1,0,0,.7))
points(sort(fast$LON), fast$LAT[order(fast$LON)], pch=20, col="red")

lines(sort(slow$LON), slow$LAT[order(slow$LON)], lty=1)


library(XML)
names = readHTMLTable("http://weather.unisys.com/hurricane/atlantic/2015")

names = c("KATE","JOAQUIN","IDA","NINE","HENRI","GRACE","FRED","ERIKA","DANNY","CLAUDETTE","BILL","ANA")

for(name in names)
{
   
}