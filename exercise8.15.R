library(nutshell)

#1
library(lattice)
library(ggplot2)

#2 
shapiro.test(field.goals$yard)

#3
ks.test(field.goals$yards, "pnorm")

#4
tOut = t.test(field.goals$yards)
str(tOut)
tOut$conf.int

#5 
t = t.test(field.goals$yards, mu=35, alternative="greater")
t$p.value

#6 
wilcox.test(field.goals$yards, conf.int=T)

#7

means = NULL
for (i in 1:length(field.goals$yards))
{
    means = c(means, mean(sample(field.goals$yards, 100, replace=T)))
}

hist(means)
quantile(means, probs=c(0.025,0.975))

#8
library(boot)
myMean=function(data, index)
{
    mean(data[index])
}
bootOut = boot(data=field.goals$yards, myMean, 999)
boot.ci(bootOut)

