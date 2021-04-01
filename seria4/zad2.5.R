
alfa = 0.2
beta = 0.8

X <- numeric(N)

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
# if X[i] == 1 sample(x = c(1,2), size=1, prob(1-a,a))
# if X[i] == 1
N <- 10^{5}
Traj <- numeric(N)
Traj[1] <- 1
for (i in 2:N){
  Traj[i] <- trajektoria(Traj[i-1], alfa, beta)
}

plot(Traj[1:100], type="s")


#rozkład stacjonarny

alfa = 0.9
beta = 0.1

pi1 = alfa / (alfa + beta)
pi2 = beta / (beta + alfa)

N <- 1000000
Traj <- numeric(N)
W <- runif(1, 0,1)
#if (W < pi1) {
#  Traj[1] = 1
#} else {
#  Traj[1] = 2
#}
Traj[1] = 1


for (i in 2:N){
  Traj[i] <- trajektoria(Traj[i-1], alfa, beta)
}

pi1_hat = length(Traj[Traj==2])/length(Traj)
pi2_hat = length(Traj[Traj==1])/length(Traj)

plot(Traj[1:1000], type="s")

