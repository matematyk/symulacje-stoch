N <- 10^2

X <- numeric(N)
X[1] <- 0

c <- 1

for (n in 2:N) {
  Y <- sample(c(X[n-1]+1, max(X[n-1]-1, 0)),size=1, replace=FALSE,  prob=c(1/2,1/2)) #losuje stan do przejścia
  print(Y)
  U <- runif(1)
  if (Y == (X[n-1] + 1) && (U  < (c/(X[n-1] + 1)))) {
    X[n-1] <- Y
  }
  if (Y == (X[n-1] - 1) && (U  < (X[n-1]/c))) {
    X[n] <- Y
  } else {
    X[n] <- X[n-1]
  }
}

hist(X, prob=TRUE)
dp <- function(x, lbd = 0.5 * 100) dpois(x, lambda = lbd)
curve(dp, 0, 100, add=TRUE)
