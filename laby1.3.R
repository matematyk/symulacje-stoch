n <- 10^{3}
S <- numeric(n)
for (i in 1:n){
  U <- runif(1) #przesuniecie inny rozklad prawdopodobieństwa
  repeat{
    X <- runif(1)
    
    if (X > U)
      break
  }
  S[i] <- X
}
S
hist(S, prob=TRUE)