
power = function(sample_size, population_mean, population_sd, hypothesis_population_mean)
{
    alpha = 0.05
    rejected = 0
    values = data.frame(iteration=numeric(0), proportion=numeric(0))
    for(i in 1:1000)
    {
        observations = rnorm(sample_size, mean=population_mean, sd=population_sd)
        if(t.test(observations, mu = hypothesis_population_mean)$p.value < alpha)
        {
            rejected = rejected + 1
        }
    }
    
    return(rejected/i)
}

data = data.frame(iteration=numeric(0), proportion=numeric(0))
values = seq(from=5, to=7, by=.25)
for(avg in values)
{
    p = power(10, avg, 1, 5)
    data = rbind(data, c(avg, p))
}

plot(data, type="b", xlab="Mean", ylab="Power")
abline(h=alpha, col="red", lty=2)
abline(v=5, col="green", lty=2)
