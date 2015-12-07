# Emily O'Dean
# 9/29/15
# Day 3 Activity

dir()
data = read.csv(file="Legos.csv", header=T)
str(data)

data = read.csv(file="Legos.csv", header=T, na.strings="", as.is=T)
str(data)


# xlsx
library(xlsx)
data = read.xlsx(file="Legos.xlsx", sheetIndex=1, header=T)
str(data)
    
# openxlsx
detach('package:xlsx', unload=TRUE)
library(openxlsx)
data = read.xlsx(xlsxFile="Legos.xlsx", sheet=1, colNames=TRUE)
head(data)
tail(data)
str(data)


Attack = read.csv(file="SharkAttacks.csv", header=T, as.is=T)
Fatal = read.csv(file="SharkFatalities.csv", header=T, as.is=T, na.strings="*")
names(Fatal)[2] = "Fatal"

sharkData1 = merge(Attack, Fatal, by.x="Decade", by.y="Years")
sharkData2 = merge(Fatal, Attack, by.x="Years", by.y="Decade")

sharkData1
sharkData2

#access rows based on order of the fatalities column
sharkData1[order(sharkData1$Fatal),]

#cross tabulate using table (how many places are there with 0-5 beds and fireplaces or no fireplaces
table(data$FireplaceF, data$Beds)

#find the mean of homes with and without a fireplace
tapply(data$Prices, INDEX=data$FireplaceF, FUN=mean)

#which is the most common number of bedrooms
data = as.data.frame(table(fireplaceF$bedrooms), stringsAsFactors=FALSE)
data[which(data$Freq==max(data$Freq)),1]