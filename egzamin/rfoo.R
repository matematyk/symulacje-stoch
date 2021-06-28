n <- 10^4
var_s <- 25
X <- rnorm(n, mean = 17.4, sd=sqrt(var_s))

alfa <- 0.05
q <- round(qnorm(1-alfa/2))


n <- 10^6
sqrt(var_s)*q/sqrt(n)
