cars = mtcars

automatic = cars[cars$am==1,]
summary(automatic)
automatic_count =nrow(automatic)
automatic_mean_mpg = mean(automatic$mpg)

#frequency of each
table(mtcars$am)

manual = cars[cars$am==0,]
manual_count = nrow(manual)
manual_mean_mpg = mean(manual$mpg)

#mean of each
apply(mtcars[,c("am","mpg")], MARGIN=2, FUN=mean)
#mean of each categor
tapply(mtcars$mpg, INDEX=mtcars$am, FUN=mean)

bartlett.test(mpg~am, data=cars)
t.test(mpg~am, data=mtcars, alternative="less")

wilcox.test(mpg~am, data=mtcars, alternative="less")

d = cbind(mtcars$am, mtcars$mpg)


avgman=NULL
avgaut=NULL

for(i in 1:100)
{
    cars = sample(mtcars$mpg, nrow(mtcars), replace=F)
    automatic = cars[1:automatic_count]
    manual= cars[(automatic_count+1):(nrow(mtcars))]
    avgman=c(avgman, mean(manual))
    avgaut=c(avgman, mean(automatic))
}


par(mfrow=c(2,1))
hist(avgman)
hist(avgaut)

mean(avgman)
mean(avgaut)
