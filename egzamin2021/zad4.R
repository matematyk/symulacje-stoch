n <- 10^5

X <- numeric(n)

for (i in 1:n){
  U <- runif(1)
  
  X[i] <- 1/2*(4*U-sqrt(8*U+1) +1)
  
}

hist(X, prob=TRUE)

curve(1/2*(1+1/(2*sqrt(x))), add=TRUE)