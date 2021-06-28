N <- 10^3

X <- numeric(N)
Y <- numeric(N)
for (i in 1:N){
  X[i] <- runif(1, -1, 2)
  if (X[i] > -1 && X[i] < 1){
    Y[i] = X[i]
  } else {
    Y[i] = X[i] - 3
  }
}
plot(X,Y)

cov(X,Y)