
N <- 10^3
X <- numeric(N)
for (i in 1:N){
  U <- runif(1,0,1)
  
  if (U < 1/2){
    T <- 1 - sqrt(2*(U))
  } else{
    T <- -1 + sqrt(2*(1-U))
  }
  X[i] <- T
}

hist(X)