theta <- 1
n <- 10000
alfa = 1/3 

v <- sample(c(1,2), size=n, replace=TRUE, prob=c(alfa, 1-alfa) )
x <- rnorm(n, theta, sd=sqrt(v))
var(X[v==2])
var(X[v==1])
#P(v=1|x) = P(x|v=1)*alfa / (P(x|v=1)alfa + P(x|v=2)*(1-alfa))