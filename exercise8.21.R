myLink = "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv"
coData = read.csv(myLink, header=T, as.is=T);

library(RCurl);
myLink = "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv"
link = getURL(myLink);
coData = read.csv(textConnection(link));


#symbol = %5EGSPC
#a = month
#b= day
#c = year
#g = d,w,m day week month
link2 = "http://real-chart.finance.yahoo.com/table.csv?s=%5EGSPC&a=00&b=3&c=1950&d=10&e=24&f=2015&g=m&ignore=.csv"
data = getURL(link2)
sp = read.csv(textConnection(data))

library(xlsx)
value = read.xlsx(file="TB3MS.xlsx", sheetIndex=1, startRow=12)

B0s = NULL
B1s = NULL
sector = NULL
symbols = NULL

for(row in 1:nrow(coData))
{
    symbol = coData[row,]$Symbol
    
    if (symbol != "QRVO")
    {
        link = paste("http://real-chart.finance.yahoo.com/table.csv?s=",symbol,"&a=00&b=1&c=2010&d=00&e=1&f=2015&g=m&ignore=.csv", sep="")
        data = getURL(link)
        moData = read.csv(textConnection(data))
        
        y=NULL
        x=NULL
        
        print(nrow(moData))
        print(symbol)
        
        for(i in 1:nrow(moData))
        {
            Rt = log(moData[i,]$Adj.Close/(moData[i,]$Adj.Close-1)) * 100
            month = substring(toString(moData[i,]$Date), 1, 7)
            Rft = value[grep(month, value$DATE),]$VALUE
            
            Rbt = log(sp[i,]$Adj.Close/(sp[i,]$Adj.Close-1)) * 100
            
            y = c(y,(Rt - Rft))
            x = c(x,(Rbt - Rft))
        }
        
        model = lm(y~x)
        B0s = c(B0s, model$coefficients[[1]])
        B1s = c(B1s, model$coefficients[[1]])
        sector = c(sector, coData[row,]$Sector)
        symbols = c(symbols, toString(symbol))
        
    }
}

hist(B0s)
hist(B1s)

data = data.frame(B0 = B0s, B1 = B1s, Sector = as.factor(sector), Symbols=coData[symbols,]$Name)
library(lattice)
histogram(~B0|Sector, data=data)

dotplot(B0~B1|sector, data=data)
plot(data$B0,data$B1, pch=19)

data[(data$B0 > 0 & data$B1 <1),]

