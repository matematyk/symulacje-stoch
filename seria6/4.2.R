# estymacja rozkladu stacjonarnego
traj <- function(alpha, N, v){
  W <- rnorm(1, 0, 1/(1-alpha^2))
  X[1] <- W
  for (i in 2:N) {
    W <- rnorm(1,0, v)
    X[i] <- X[i-1]*alpha + W
  }
  return(X)
}

N <- 10^{3}

K <- 10^{3}
Sumy <- numeric(K)
for (j in 1:K){
  Proces <- numeric(N)
  alpha <- 0.9
  Proces <- traj(alpha, N, 1)
  Sumy[j] <- sum(Proces)
}
#Obliczenie sigma_as (symulacyjnie)
1/N *var(Sumy)


#Obliczenie sigma_as: (teoretycznie)
1/(1-alpha^{2}) * ((1+ alfa)/ (1-alfa))