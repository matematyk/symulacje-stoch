n <- 10^{3}
rnormCTG <- function(n) { replicate(n, sum(runif(12))-6) }

S<- rnormCTG(n)

hist(S, prob=TRUE) 

curve(dnorm, add=TRUE) 

plot(ecdf(S))
curve(pnorm, add=TRUE)

ks.test(S,pnorm)

qqnorm(S)
qnorm(1/100)

shapiro.test(S) #za duża próbka, nie wykrylo!

S <- rnorm(n)
