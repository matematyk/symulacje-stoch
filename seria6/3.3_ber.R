u <- 10 
b <- 10


M <- 10^3
Statusy <- numeric(M)
for (j in 1:M) {
  n <- 10^3
  mi <- 1
  p <- 1/4
  a <- 1
  Y <- ifelse(rbinom(n, 1, 1/10),-a,a)
  S <- cumsum(Y)
  for (i in 1:n) {
    if (S[i] > u || S[i] < -1 * b ){
      if(S[i] > u) {
        Statusy[j] <- 1
      } 
      break
    }
  }
}

theta_est_crud <- mean(Statusy)

## Important sampling
M <- 10^5 
Psi <- numeric(M)
for (j in 1:M) {
  n <- 10^3
  mi <- 1
  r <- 0
  Y <- ifelse(rbinom(n, 1, 1/10),-a,a)
  S <- cumsum(Y)
  for (i in 1:n) {
    if (S[i] > u ){
      Psi[j] <- exp(-r*(S[i]-u))
      break
    }
  }
}
theta_est <- mean(Psi)* exp(-r*u)