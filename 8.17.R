observations = rnorm(10, mean=5.25, sd=1)
t.test(observations)

alpha = 0.05
rejected = 0

values = data.frame(iteration=numeric(0), proportion=numeric(0))
for(i in 1:1000)
{
    observations = rnorm(10, mean=5.25, sd=1)
    if(t.test(observations, mu = 5)$p.value < alpha)
    {
        rejected = rejected + 1
    }
    
    values = rbind(values, c(i, rejected/i))
}

plot(values, type="l", xlab="Iteration", ylab="Cumulative Type 1 Error", main="Convergence of Alpha")
abline(h=alpha, col="red", lty=2)

