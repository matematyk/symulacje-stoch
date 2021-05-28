
K <- 1
sumy <- numeric(K)
for (j in 1:K){
    M <- 10^4
    param <- 1/2
    
    A <- function(X_, X){
      ulam <- dnorm(X_)/ dnorm(X)
      return(min(ulam, 1))
      
    }
    X <- numeric(M)
    X[1] <- 0
    for (i in 2:M) {
      X_ <- runif(1, min=X[i-1]-param, max=X[i-1]+param) # propozycje
      U <- runif(1)
      
      if (U < A(X_, X[i-1])){ # prawdopodbienstwo akceptacji
        X[i] = X_
      } else {
        X[i] = X[i-1]
      }
    }
    sumy[j] <- sum(X)
}

var(sumy)/M

hist(X, prob=TRUE)
curve(dnorm, add=TRUE)