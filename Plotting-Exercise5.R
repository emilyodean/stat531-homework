library(datasets)
plot(mtcars$hp, mtcars$mpg, type="l")
plot(sort(mtcars$hp), mtcars$mpg[order(mtcars$hp)], type="l")

plot(mpg~hp, data=mtcars)
plot(mpg~hp, data=mtcars, xlab="Horse Power", ylab="MPG", main="Horse Power vs MPG", ylim=c(5,40), pch=20, col=4)

pdf(file="myGraph.pdf")
plot(mpg~hp, data=mtcars, xlab="Horse Power", ylab="MPG", main="Horse Power vs MPG", ylim=c(5,40), pch=20, col="orchid4")
dev.off()

#parameters of graph sheet
par(mfrow=c(2,1))
plot(mpg~hp, data=mtcars)
par(mfrow=c(1,1))

cars4=subset(mtcars, cyl==4);
cars6=subset(mtcars, cyl==6);
cars8=subset(mtcars, cyl==8);

plot(mpg~hp, data=mtcars, xlab="Horse Power", ylab="MPG", main="Horse Power vs MPG", type="n")
points(cars4$hp, cars4$mpg, col=2, pch=15)
points(cars6$hp, cars6$mpg, col=3, pch=17)
points(cars8$hp, cars8$mpg, col=4, pch=19)

plot(mpg~hp, data=mtcars, xlab="Horse Power", ylab="MPG", main="Horse Power vs MPG", type="n")
points(cars4$hp, cars4$mpg, col=2, pch=15)
points(cars6$hp, cars6$mpg, col=3, pch=17)
points(cars8$hp, cars8$mpg, col=4, pch=19)
legend("topright", legend=c("4 Cylinder", "6 Cylinder", "8 Cylinder"), col=c(2,3,4), pch=c(15,17,19))

m1=lm(mpg~hp, data=mtcars)
#1 way to add line - only works for lm though
abline(reg=m1, lty=2)
#more manual way
points(sort(mtcars$hp), fitted(ml)[order(mtcars$hp)], type="l")
#or
abline(coef=(m1), lty=2, col=1)

#vertical lines
abline(v=200)

legend("topright", legend=c("4 Cylinder", "6 Cylinder", "8 Cylinder", "Fitted"), col=c(2,3,4,1), pch=c(15,17,19,NA), lty=c(NA,NA,NA,2))

#barplots
library(UsingR)
table(homeprice$bedrooms)
table(homeprice$full)

par(mfrow=c(1,1))
barplot(table(homeprice$bedrooms), main="Number of Bedrooms")
barplot(table(homeprice$full), main="Number of Full Bathrooms")

barplot(table(homeprice$bedrooms, homeprice$full), main="Bedrooms and Bathrooms", xlab="Number of Full Bathrooms", col=2:6)
legend.text=c("One Bed", "Two Beds", "Three Beds", "Four Beds", "Five Beds")

barplot(table(homeprice$bedrooms, homeprice$full), main="Bedrooms and Bathrooms", xlab="Number of Full Bathrooms", col=2:6, legend.text=c("One Bed", "Two Beds", "Three Beds", "Four Beds", "Five Beds", beside=T))

priceMean = tapply(homeprice$sale, homeprice$bedrooms, FUN=mean)
priceSd = tapply(homeprice$sale, homeprice$bedrooms, FUN=sd)

barplot(priceMean, xlab="Number of Bedrooms", ylab="Average Sale Price", ylim=c(0,500), main="Average Price by Number of Bedrooms")

myBarplot = barplot(priceMean, xlab="n bedroos", ylab="avg sale price", ylim=c(0,500), main="tille here")
arrows(myBarplot, priceMean, myBarplot, priceMean+priceSd, angle=90)

par(mfrow=c(3,1))
hist(homeprice$sale[homeprice$full==1], xlab="Sale Price", main="One bath sales price", freq=F, xlim=c(0,700), breaks=seq(0,700,50), ylim=c(0,0,0.1)))
hist(homeprice$sale[homeprice$full==2], xlab="Sale Price", main="hi", freq=F, xlim=c(0,700), breaks=seq(0,700,50), ylim=c(0,0.01))

boxplot(sale~bedrooms, data=homeprice, horizontal=T, xlab="Sales Price", ylab="Number of Bedrooms")

stripchart(sale~bedrooms, data=homeprice, vert=T, method="jitter", pch=20, ylab="Sales Price", main="Sales Price by Number of Bedrooms", col=1)
mtext("With avgs and std devs")
points(1:5, priceMean, pch=15, cex=2, col=rainbow(5))
arrows(1:5, priceMean, 1:5, priceMean+2*priceSd, angle=90, col=rainbow(5))

