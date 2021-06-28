x0 = 1
M <- 10^4
X <- numeric(M)
Y <- numeric(M)
X[1] <- x0



for (n in 1:(M-1)) {
  Y[n] <- runif(1, 0, exp(-X[n]))
  X[n+1] <- runif(1, 0, -log(Y[n]))
}

plot(X,Y)
hist(Y, prob=TRUE)
#mean(Y) = 
curve(-log(x), add=TRUE,col='red')

curve(dexp(x,rate=4), add=TRUE)

plot(rexp(M,1), rexp(M,4))
