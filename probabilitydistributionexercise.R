
#probability x<= 1
pnorm(1, mean=0, sd=1)

#P(X>1)
1-pnorm(1,mean=0, sd=1)
pnorm(1, 0, 1, lower.tail=F)

#P(-1<X<1)
pnorm(1, 0,1)
pnorm(-1,0,1)

pnorm(1,0,1) - pnorm(-1,0,1)

#What is the 97.5th percentile of X
qnorm(0.975, mean=0, sd=1)
qnorm(0.025, mean=0, sd=1, lower.tail=F)

#randomly generate 999 observations of X
x= rnorm(999, 0, 1)

#find the density of observations
fx = dnorm(x, mean=0, sd=1)

#plot simulated observations
plot(x,fx)

#plot density line
plot(sort(x), fx[order(x)], type="l")

#what is the probability X is greater than 1?
sum(x>1)/length(x)


#
x = rnorm(999, mean=0, sd=1)
y= rexp(999, rate=1)
z=x+y
library(lattice)
densityplot(z)

#probability that Z > 1?
sum(z>1)/length(z)

#simulated distribution
quantile(z, c(.11,.975))

#sample from the distribution
sample(z, 2, replace=F)
sample(z, 2, replace=T)

sample(1:10, size=5, replace=F)
