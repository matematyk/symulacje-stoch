n <- 10^5


X <- numeric(X)

for (i in 1:n){
  X1 <- runif(1)
  X2 <- runif(1)
  U <- runif(1)
  
  if (U < X1/(X1+X2)){
    X[i] = X1
  } else {
    X[i] = X2
  }
}

hist(X, prob=TRUE)
curve(1/(2*sqrt(2)/3)* sqrt(2*x), add=TRUE)
