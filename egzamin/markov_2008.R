m <- 10^4

X <- numeric(m)

for (n in 2:m){
  Y <- runif(1, X[n-1] - 1, X[n-1] + 1)
  if (Y^2 < X[n-1]^2){
    X[n] <- Y
  } else {
    U <- runif(1)
    if (U < exp(X[n-1]^2 - Y^2)) {
      X[n] <- Y
    } else {
      X[n] = X[n-1]
    }
  }
}
hist(X, prob=TRUE)
curve(dnorm(x,mean(X),var(X)),add=TRUE)

m <- 10^4

X <- numeric(m)

for (n in 2:m){
  Y <- runif(1, X[n-1] - 1, X[n-1] + 1)
    U <- runif(1)
    if (U < exp(X[n-1]^2 - Y^2)) {
      X[n] <- Y
    } else {
      X[n] = X[n-1]
    }
}
hist(X, prob=TRUE)
curve(dnorm(x,mean(X),var(X)),add=TRUE)
