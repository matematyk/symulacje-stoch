N <- 10^{3}

X <- numeric(N)
X[1] <- 1 
astar <- 10
across <- 2
Tn <- numeric(N)


for (x in 2:N) {
  lambda = astar + (across*X[x-1])
  p = astar /(astar + (across*X[x-1]))
  #K <- runif(1)
  k = rbinom(n=1, size=1,prob=p)
  if (k == 0){
    X[x] = X[x-1]+1
  } else {
    X[x] = X[x-1]-1
  }

  Wi <- rexp(1, lambda)
  Tn[x] <- Tn[x-1] + Wi
}

index <- length(Tn[Tn < 1])


plot(Tn[1:100], X[1:100] , type='s')


#zbadaj symulacyjnie zbiezność do rozkładu stacjonarnego

#hist(X, prob=TRUE)

#counts <- table(X)
#barplot(counts)