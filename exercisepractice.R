library(datasets)
library(UsingR)

str(USArrests)
x = lm(USArrests$Rape~USArrests$UrbanPop)

plot(y=USArrests$Rape, x=USArrests$UrbanPop)
abline(a=coef(x)[1], b=coef(x)[2])

map("state")





#show the mean and standard deviation of approval and disapproval over time
str(ObamaApproval)

dates=names(table(substr(ObamaApproval$start,1,7)))
ObamaApproval$month = substr(ObamaApproval$start,1,7)

avgPerMonth = function(dates, data)
{
    newData = data.frame(date=character(0), avgA=numeric(0), sdA=numeric(0),avgD=numeric(0),sdD=numeric(0))
    for(date in dates)
    {
        samedate = subset(ObamaApproval, subset=ObamaApproval$month == date)
        avgApprove = mean(samedate$approve)
        sdApprove = sd(samedate$approve)
        avgDisapprove = mean(samedate$disapprove)
        sdDisapprove = sd(samedate$disapprove)
        
        newData = rbind(newData, data.frame(date=date, avgA=avgApprove, sdA=sdApprove, sdD=sdDisapprove, avgD=avgDisapprove))
    }
    return(newData)
}

plot(x=ObamaApproval$start, y=ObamaApproval$approve)
avgDat = avgPerMonth(dates, ObamaApproval)
plot(x=avgDat$date, y=avgDat$avgA, type="p", col="green")
segments(avgDat$date, avgDat$avgA-avgDat$sdA, avgDat$date, avgDat$avgA+avgDat$sdA)


plot(avgDat$date, y=avgDat$avgD, type="p", col="red")


str
ls
dir
getwd
setwd
library
vector
matrix
data.frame
list
is./as.TYPE
rep, seq, dim, head, tail, nrow, ncol, length, rownames, colnames, names, c, cbind, rbind, na.omit, read.table, read.csv, read.xlsx, readLines, readHTMLTable, sort, rank, order, factor, which, subset, merge, summary, table, sum, prop.table, apply, tapply, lapply, mapply, grep, grepl, regexpr, grepexpr, nchar, substring, tolower/toupper, sub, gsub, strslit, unlist, charmatch, paste, plot, points, lines, abline, par, legend, barplot, hist, stripchart, dotchart, pairs, coplot, barchart, histogram, qqmath, qq, densityplot, bwplot, stripplot, dotplot, xyplot, qplot, r/d/p/qIDENTIFIER, sample, quantile, if, else, for, while, function, shapiro.test, ks.test, t.test, wilcox.test, bartlett.test, lm, loess, map.


