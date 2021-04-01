

rdirich(10, 1/2, 1/2)
hist(rdirichlet(10, 1/2))

#rgamma 
rdirch_g <-function(alphas){
  X <- sapply(alphas, function(x) { rgamma(1, x,1)})
  
  return(X/ sum(X))
}

matrix()

plot(ecdf(Y))

rdirch_g(c(1/2,1/2,1/2))

#cumprod()

#beta dist

rdirich_b <- function(alfy){
  K <- length(alfy)
  x <- numeric(K)
  x[1] <- rbeta(1, alfy[1], sum(alfy[2:K]))
  for (j in 2:(K-1)){
    phi <- rbeta(1, alfy[j], sum(alfy[(j+1):K]))
    x[j] <- phi*(1-sum(x[1:(j-1)]))
  }
  x[K] <- 1- sum(x[1:(K-1)])
  
  return(x)
}
rdirich_b(c(1/2, 1/2, 1/2, 1/5, 1/10))

rep(1/100, 10)
# zapisac w macierzy 
# 
M <- as.vector(M)


