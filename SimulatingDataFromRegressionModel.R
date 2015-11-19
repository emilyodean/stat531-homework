intercept = NULL
slope = NULL
sd = NULL
resid_sd = 10
sample_size = 100

wine_data = ceiling(runif(sample_size, min=77, max=99))
mean = mean(wine_data)
sd = sd(wine_data)

residuals = rnorm(sample_size, mean=0, sd = resid_sd)


price = 10 + wine_data + residuals

par(mfrow=c(1,1))
plot(wine_data, price)
abline(lm(price~wine_data), col="blue")

price_population = 10 + wine_data
abline(lm(price_population~wine_data), col="red")


population = ceiling(runif(1000, min=77, max=99))
residuals = rnorm(1000, mean=0, sd = resid_sd)
price=10+population+residuals
plot(population, price, pch=19)


data = data.frame(slope=numeric(0), intercept=numeric(0))
for (i in 1:100)
{
    wine_data = sample(population, 100, replace=F)
    residuals = rnorm(sample_size, mean=0, sd = resid_sd)
    price=10+wine_data+residuals
    line = lm(price~wine_data)
    abline(line, col=colors()[i])
    data = rbind(data, c(line$coefficients[2],line$coefficients[1]))
    
}

plot(data[,1])
plot(data[,2])
