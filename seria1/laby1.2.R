n <- 10^4
X <- runif(n, min=-1,max=1)
Y <- runif(n, min=-1,max=1)
plot(X,Y)
accept <- (X^2 + Y^{2} < 1)
plot(X[accept],Y[accept])
hist(X[accept], prob=TRUE)
curve(((1-x^{2})^{1/2})/(pi/2), add=TRUE)
R <- (X[accept]^{2} + Y[accept]^{2})^{1/2}
#
R <- sqrt(X[accept]^{2} + Y[accept]^{2})
hist(R, prob=TRUE)