traj <- function(alpha, n, v){
  X <- numeric(n)
  X[1] <- rnorm(1, 0, 1)
  for (i in 2:N) {
    W <- rnorm(1,0,v)
    X[i] <- X[i-1]*alpha + W
  }
  return(X)
}
alpha = 0.9
Proces <- traj(0.9, 10000, 1)

plot(Proces[1:100], type='l')

# estymacja rozkladu stacjonarnego
traj <- function(alpha, n, v){
  W <- rnorm(1, 0, 1/(1-alpha^2))
  X[1] <- W
  for (i in 2:N) {
    W <- rnorm(1,0,v)
    X[i] <- X[i-1]*alpha + W
  }
  return(X)
}
alpha = 0.9
Proces <- traj(0.9, 10000, 1)


hist(Proces, prob=TRUE)
var(Proces)
1/ (1-alpha^{2})
#dnorm drugi jest std, a nie wariancja!
curve(dnorm(x, mean=0,  sqrt(1/ (1-alpha^{2}))), add=TRUE)