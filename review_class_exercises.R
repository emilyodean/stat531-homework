sort(mtcars$hp)
mtcars$hp


table(mtcars$am)
barplot(table(mtcars$am))


barplot(table(homeprice$bedrooms, homeprice$full), 
        col=2:6, legend.text=c("one bed", "2", "3", "4", "5"),
        beside=T )


priceMean= tapply(homeprice$sale, homeprice$bedroom,FUN=mean)
priceSd = tapply(homeprice$sale, homeprice$bedroom, FUN=sd)

mybarplot = barplot(priceMean)
arrows(mybarplot, priceMean, mybarplot, priceMean+priceSd, angle=90)

hist(homeprice$sale[homeprice$full==1], breaks=seq(0,700,50), freq=F)

boxplot(sale~bedrooms,data=homeprice,horizontal=T,xlab="Sales Price", ylab="Number of Bedrooms")

stripchart(sale~bedrooms, data=homeprice, vert=T, method="jitter")
mtext("With Averages and Standard deviations")


dotchart(homeprice$sale, groups=factor(homeprice$full))


panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(homeprice, diag.panel=panel.hist, upper.panel=panel.smooth, lower.panel=panel.cor)


coplot(sale~list | factor(full), data=homeprice, panel=panel.smooth)



#lattice

library(lattice)
mtcars$fCyl = factor(mtcars$cyl, labels=c("4 Cyl", "6 Cyl", "8 Cyl"))
mtcars$fAm = factor(mtcars$am, labels=c("Manual", "Automatic"))

barchart(~table(mtcars$fAm), data = mtcars)
barchart(~prop.table(table(mtcars$fAm)), data=mtcars)
Counts = c(table(mtcars$fAm, mtcars$fCyl))
dataTemp = data.frame(Counts, Cyl=rep(c("4", "6", "8"),each=2), Trans=rep(c("manual", "auto"), times=3))

barchart(Counts~Cyl, groups=Trans, data=dataTemp, auto.key=T)
barchart(rownames(mtcars)~mpg|fCyl*fAm, layout=c(6,1), origin=0, data=mtcars)
