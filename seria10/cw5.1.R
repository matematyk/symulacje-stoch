#Przeprowadzić doświadczenie dla parametrów: d = 20, α0 = 4, α1 = −2, β = 0.5.
d<- 20
alfa0 <- 4
alfa1 <- 2
beta <- 0.5

zero <- numeric((d+2)*(d+2))
X <- matrix(zero, nrow=d+2, ncol=d+2)
M <- 10^5

burn_time <- 10^3

S <- numeric(M-burn_time)
N <- numeric(M-burn_time)
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
#algorytm Metropolisa
for (m in 1:M){
  i <- ceiling(runif(1, 1, d+1))
  j <- ceiling(runif(1, 1, d+1))
  
  U <- runif(1)
  if (X[i,j] == 1){
    prop <- 0
  } else {
    prop <- 1
  }
  a <- exp(-beta*(alfa0*(prop-X[i,j])+alfa1*(prop*(X[i+1,j]+X[i,j+1]+X[i-1,j]+X[i,j-1])-X[i,j]*(X[i+1,j]+X[i,j+1]+X[i-1,j]+X[i,j-1]))))

  if (U < a){
    X[i,j] <- prop
  }
  if (m > burn_time){
    S[m-burn_time] <- sum(X)
    N[m-burn_time] <- sumy_sasiedzkie(X, d)
  }
  
}
# Wyestymować rozkłady statystyk dostatecznych
mean(S)
mear(N)
# Wyestymować rozkad Boltzmanna (rozkład na poziomach energii): jest to z definicji rozkład

S*alfa0 + N*alfa1

hist(S*alfa0 + N*alfa1, prob=TRUE)
# 
image(X)