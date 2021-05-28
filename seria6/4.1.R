trajektoria <- function(Stan, alfa, beta){ #stan
  Stan_w = -1
  if (Stan == 1){
    W <- runif(1)
    if (W < alfa){
      Stan_w = 2
    } else { #ifelse
      Stan_w = 1
    }
  }
  if (Stan == 2){
    W <- runif(1)
    if (W < beta){
      Stan_w = 1
    } else {
      Stan_w = 2
    }
  }
  return(Stan_w)
}

alfa = 0.9
beta = 0.9
#rozklad stacjonarny
pi1 = alfa / (alfa + beta)
pi2 = beta / (beta + alfa)

N <- 10^{3}

K <- 10^{3}
Sumy <- numeric(K)
Trajektorie <- matrix( nrow=N, ncol=K)
for (j in 1:K) {
  Traj <- numeric(N)
  Traj[1] = 1
  
  for (i in 2:N){
    Traj[i] <- trajektoria(Traj[i-1], alfa, beta)
  }
  Trajektorie[,j] <- Traj
  Sumy[j] <- sum(Traj)
}
1/N *var(Sumy)


as <- alfa*beta*(2-alfa-beta)/(alfa+beta)^{3}

S <- apply(Trajektorie, 2, cumsum)
#Wariancje rosną liniowo z długoscią trajektorii

VAR_S <- apply(S, 1, var)