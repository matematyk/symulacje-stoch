n <- 100
k <- 1000
X <- matrix(numeric(n*(k+1)), nrow=k+1)
W <- matrix(numeric(n*(k+1)), nrow=k+1)

Y <- numeric(k+1)

for(i in 2:(k+1)){
  Y[i] <- a*Y[i-1]^2+rnorm(1,0,2)
}
Y <- Y + rnorm(k+1,0,0.5)

for (i in 1:n){
  X[1,i] <- 0
  W[1,i] <- dnorm(X[1,i], 0, 1)
}

N <- matrix(numeric(n*k), ncol=n)

for (i in 2:(k+1)){
  for (i in 1:n){
    N[t-1,i] <- sample(x=1:n, 1, replace=F, prob=W[t-1,]/sum(W[t-1,]))
    X[t,i] <- rnorm(1, X[t-1, N[t-1,i], 1]) 
    W[t,i] <- dnorm(X[t,i], X[t-1,i])
  }
}
B <- matrix(numeric((k+1)*n), ncol=n)

B[k+1] <- sample(x=1:n, 1, replace, prob=W[k,]/Sum(W[k,]))
for (t in k:1){
  B[t] <- N[t,B[t+1]]
}