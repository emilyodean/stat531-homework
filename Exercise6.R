#Day 5 Exercise STAT531

#6.1

#1 
mtcars$fCyl = factor(mtcars$cyl, labels=c("4 Cyl", "6 Cyl", "8 Cyl"))
mtcars$fAm = factor(mtcars$am, labels=c("Manual", "Automatic"))
#2
barchart(~table(mtcars$fAm), data=mtcars)
barchart(~prop.table(table(mtcars$fAm)), data=mtcars)

Counts = c(table(mtcars$fAm, mtcars$fCyl))
dataTemp = data.frame(Counts, Cyl=rep(c("4 Cyl", "6 Cyl", "8 Cyl"), each=2), Trans=rep(c("Manual", "Automatic"), times=3))

barchart(Counts~Cyl, groups=Trans, data=dataTemp, auto.key=TRUE)
barchart(Counts~Cyl, groups=Trans, data=dataTemp, auto.key=TRUE, stack=TRUE)
barchart(Counts~Cyl|Trans, data=dataTemp)

barchart(rownames(mtcars)~mpg|fCyl*fAm, layout=c(6,1), origin=0, data=mtcars)
#3
histogram(~mpg|fCyl*fAm, data=mtcars, type="count", breaks=10)
#4 normal probability plot - plots observed data on x axis vs expected data
qqmath(~mpg|fCyl*fAm,data=mtcars)
#5
qq(fAm~mpg|fCyl, data=mtcars, layout=c(3,1))

#shapiro.test - normality test
#6 - doesn't try to force a normal distribution
densityplot(~mpg|fCyl, groups=fAm, data=mtcars, auto.key=list(space="top", lines=TRUE), plot.points=FALSE, layout=c(3,1))
#7
bwplot(fCyl ~ mpg | fAm, data=mtcars)
bwplot(mpg~fAm|fCyl, data=mtcars,layout=c(3,1))
#8
stripplot(mpg~fCyl|fAm, data=mtcars,jitter.data=TRUE)
stripplot(mpg~fAm, groups=fAm, data=mtcars, jitter.data=TRUE, auto.key=list(space="top", points=TRUE))
#9
dotplot(mpg~fAm|fCyl, data=mtcars, jitter.x=TRUE, layout=c(3,1))
dotplot(mpg~fAm, groups=fCyl, data=mtcars, jitter.x=TRUE, auto.key=list(space="top", points=TRUE))
#10
xyplot(mpg~hp|fCyl, data=mtcars)
xyplot(mpg~hp|fCyl*fAm, data=mtcars, layout=c(3,2))
xyplot(mpg~hp|fCyl, data=mtcars, groups=fAm, layout=c(3,1), auto.key=list(space="top", points=TRUE))
xyplot(mpg~hp|fCyl*fAm, data=mtcars, layout=c(3,2), type=c("p", "r"))
xyplot(mpg~hp|fCyl, groups=fAm, data=mtcars, layout=c(3,1), type=c("p", "r"), auto.key=list(space="top", points=TRUE, lines=TRUE),
       col.line=c(2,3), strip=strip.custom(bg="white"))

#6.2
#1
qplot(fCyl, data=mtcars)
qplot(fCyl, data=mtcars, geom="bar")
qplot(fCyl, data=mtcars, fill=fAm, geom="bar")
qplot(fCyl, data=mtcars, fill=fAm, geom="bar", position="dodge")
qplot(fCyl, data=mtcars, facets=~fAm, geom="bar")
#2
qplot(mpg, data=mtcars, geom="histogram")
qplot(mpg, data=mtcars, geom="histogram", ..density..)
#position="dodge" is side by side
qplot(mpg, data=mtcars, geom="histogram", colour=fAm, ..density.., position="dodge")
qplot(mpg, data=mtcars, geom="histogram", fill=fCyl, ..density..)
qplot(mpg, data=mtcars, geom="histogram", fill=fCyl, colour=fAm, ..density..)

qplot(mpg, data=mtcars, geom="histogram", facets=fAm~fCyl, ..density..)
#3
qplot(mpg, data=mtcars, geom="density", colour=fCyl)
qplot(mpg, data=mtcars, geom="density", colour=fCyl, facets=~fAm)
#4
qplot(fAm, mpg, data=mtcars, geom="jitter", colour=fAm)
qplot(fAm, mpg, data=mtcars, geom="jitter", colour=fAm, shape=fCyl)
qplot(fAm, mpg, data=mtcars, geom="jitter", colour=fAm, facets=~fCyl)
#5
qplot(fAm, mpg, data=mtcars, geom=c("boxplot", "jitter"), colour=fAm, facets=~fCyl)
#6
qplot(hp, mpg, data=mtcars, geom=c("point", "smooth"))
qplot(hp, mpg, data=mtcars, geom=c("point"), colour=fAm, facets=fAm~fCyl)

qplot(hp, mpg, data=mtcars, geom=c("point", "smooth"), colour=fAm, facets=fAm~fCyl, method="lm")
