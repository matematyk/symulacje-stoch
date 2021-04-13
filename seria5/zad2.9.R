N <- 10^5

alfa = 0.1
beta = 0.9



Tn <- numeric(N)
X <- numeric(N)
Tn[1] <- 0
X[1] <- 1 
parm <- 1
W<- numeric(N)
for (i in 2:N) {
  if (X[i-1] == 1){
    X[i] <- 2
    parm <- alfa
  } else {
    X[i] <- 1
    parm <- beta
  }
    
  Wi <- rexp(1, parm)
  W[i] <- Wi
  Tn[i] <- Tn[i-1] + Wi
}
plot(Tn[1:100], X[1:100], type='s')

sum(W[X==1])/Tn[N] #[0, TN[N]]]
sum(W[X==2])/Tn[N]

beta/(alfa+beta)
alfa/(alfa+beta)


#start z rozkładu stacjonarnego:


Tn <- numeric(N)
X <- numeric(N)
Tn[1] <- 0
if (alfa/(alfa+beta) < runinf(1,0,1)){
  X[1] <- 1
} else {
  X[1] <- 2
}
parm <- 1
W<- numeric(N)
for (i in 2:N) {
  if (X[i-1] == 1){
    X[i] <- 2
    parm <- alfa
  } else {
    X[i] <- 1
    parm <- beta
  }
  
  Wi <- rexp(1, parm)
  W[i] <- Wi
  Tn[i] <- Tn[i-1] + Wi
}
plot(Tn[1:100], X[1:100], type='s')



Tn <- numeric(N)
X <- numeric(N)
Tn[1] <- 0
X[1] <- 1 
Q.star <- 0.95
R <- rexp(N, Q.star)
Moments <- cumsum(R)
### drugi sposob 
for (i in 2:N){
  U <- runif(1)
  if (X[i-1] == 1){
    if (U < alfa / Q.star){ X[i] <- 2 }
    else {X[i] <- 1}
  } else {
    if (U < beta / Q.star){ X[i] <- 1}
    else {X[i] <- 2}
  }
}
plot(Moments[Moments < 500], x[Moments < 500] , type = 's')
sum(R[X==1])/ Moments[N]; sum(R[X==2]) / Moments[N]
