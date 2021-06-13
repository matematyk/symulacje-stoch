#Uruchomić symulacje dla parametrów: d = 20, α 0 = 4, α 1 = −2, β = 0.5.
d <- 2
alfa0 <- 4 #oddzialywanie
alfa1 <- -2
beta <- 0.5 #temperatura układu

l <- 4
States <- expand.grid(replicate(l, c(0,1), simplify=FALSE))


S <- numeric(l*l)
N <- numeric(l*l)
z = 0
for (i in 1:(l*l)){
  X <- matrix(zero, nrow=d+2, ncol=d+2)
  X[2,2] = States[i,1]
  X[2,3] = States[i,2]
  X[3,2] = States[i,3]
  X[3,3] = States[i,4]
  S[i] <- sum(X)
  N[i] <- sumy_sasiedzkie(X, d)
  
  z = z + exp(sumy_sasiedzkie(X, d))
}

#rozkład Gibbsa
exp(N)/z



zero <- numeric((d+2)*(d+2))
X <- matrix(zero, nrow=d+2, ncol=d+2)

#systematyczny przegląd miejsc
sumy_sasiedzkie <- function(X, d){
  N <- 0
  for (i in 2:(d+1)){
    for (j in 2:(d+1)){
      N <- N + X[i,j]*(X[i+1,j]+X[i,j+1]+X[i-1,j]+X[i,j-1])
    }
  }
  return(N/2)
}

#rozklad Botlmazna
hist(S*alfa0 + N*alfa1, prob=TRUE)
