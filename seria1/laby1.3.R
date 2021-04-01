n <- 10^{3}
S <- numeric(n)
for (i in 1:n){
  U <- runif(1) #przesuniecie inny rozklad prawdopodobieństwa
  repeat{
    X <- runif(1)
    
    if (U < X)
      break
  }
  S[i] <- X
}
S
hist(S, prob=TRUE)
x <- seq(from = 0, to = 1, by = 5)
curve(1-2*x,add =TRUE)
### inny podpunkt
n <- 10^{3}
S <- numeric(n)
for (i in 1:n){
  repeat{
    U <- runif(1) #przesuniecie inny rozklad prawdopodobieństwa
    X <- runif(1)
    
    if (X > U)
      break
  }
  S[i] <- X
}
S
hist(S, prob=TRUE)
x <- seq(from = 0, to = 1, by = 5)
curve(2*x,add =TRUE)