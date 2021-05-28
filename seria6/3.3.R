

u <- 10 # kapitał początkowy
b <- 10

#b <- sample()

M <- 10^2
Statusy <- numeric(M)
for (j in 1:M) {
  n <- 10^2
  mi <- 0.1
  Y <- rnorm(n, -mi, 1)
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
#przediał ufności: schemat bernuliego
#theta_est_crud*(1-theta_est_crud)
sigma2 <- theta_est_crud*(1-theta_est_crud)
dokl <- 2*sqrt(sigma2)/sqrt(M)
m  <- 400*sigma2/theta_est_crud^2
#osiagnać zadaną dokadność względną


# important sampling
#rozklad instrumentalny 
#N(mi, 1)
#mi = 0 to CRUD = IS
M <- 10^5 
Psi <- numeric(M)
for (j in 1:M) {
  n <- 10^3
  mi <- 1
  r <- 2* mi
  Y <- rnorm(n, mi, 1)
  S <- cumsum(Y) #@TODO while
  for (i in 1:n) {
    if (S[i] > u ){
      Psi[j] <- exp(-r*(S[i]-u))
      break
    }
  }
}
theta_est <- mean(Psi)* exp(-r*u)
sigma <- sd(Psi) * exp(-r*u)
dokl <- 2*sigma/sqrt(M)
m <- 400*sigma^2/ theta_est^{2} #~300 