
N <- 10
M <- 10^3
theta <- 1/10

a <- function(X_, X, n, theta){
  ulam <- (choose(n, X_) / choose(n, X)) *  theta^{X_-X} * (1-theta)^{X-X_}
  return(min(ulam, 1)) # min jest zbędna
  
}
X <- numeric(M)
X[1] <- 0
for (i in 2:M) {
  X_ <- sample(0:N,1) # propozycje
  U <- runif(1)
  
  if (U < a(X_, X[i-1], N, theta)){ # prawd akceptacji
    X[i] = X_
  } else {
    X[i] = X[i-1]
  }
}

table(X)
table(rbinom(M, N, theta))
hist(X)