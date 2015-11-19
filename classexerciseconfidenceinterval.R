#1
x = rnorm(25, mean=75, sd=25)
#2
avg = mean(x)
std = sd(x)
#3
quant = as.numeric(quantile(rnorm(999,0,1), 0.975))
#4
#confidencelower = avg - quant*std/sqrt(25)
#confidenceupper = avg + quant*std/sqrt(25)

confidencelower = avg - quant*25/sqrt(25)
confidenceupper = avg + quant*25/sqrt(25)

plot(c(50,90),c(0,100), type="n", xlab="", ylab="")
populationmean=75
abline(v=populationmean, col="blue")

for (i in 1:100)
{ 
    x = rnorm(25, mean=75, sd=25)
    avg = mean(x)
    std = sd(x)
    confidencelower = avg - quant*25/sqrt(25)
    confidenceupper = avg + quant*25/sqrt(25)
 
    if(populationmean <= confidenceupper & populationmean >= confidencelower)
    {
        #green
        segments(x0=confidencelower,y0=i, x1=confidenceupper, y1=i, col="green")
    } else
    {
        #red
        segments(x0=confidencelower,y0=i, x1=confidenceupper, y1=i, col="red")
    }
}