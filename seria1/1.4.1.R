n <- 10000
X <- runif(n, -1, 1)
Y <- runif(n, -1, 1)
R2 <- X^{2} + Y^{2}
Rel <- (R2 < 1)
Rel2 <- sqrt(-2*log(R2[Rel])/R2[Rel])
X1 <- X[Rel]*Rel2
Y1 <- Y[Rel]*Rel2

plot(X1,Y1)
var(X1)
var(Y1)

hist(X1, prob=TRUE)
curve(dnorm, add=TRUE)
hist(Y1, prob=TRUE)
curve(dnorm, add=TRUE)