rexp(1, 1)
n <- 10^{2}
m <- 10^{4}

T_min<- numeric(m)
for (i in 1:m){
  X1 <- rexp(n)
  X1 <- sort(X1,decreasing=TRUE)
  T_min[i] <- X1[n]
}

hist(T_min, prob = TRUE)
curve(dexp(x,n), add=TRUE)

T_max <- numeric(m)
for (i in 1:m){
  X1 <- rexp(n)
  X1 <- sort(X1,decreasing=TRUE)
  T_max[i] <- X1[1]
}


d_max <- function(x,n){
  return (n*(1-exp(-x))^{(n-1)})*exp(x)
}
hist(T_max, prob = TRUE)

curve(d_max(x,n), add=TRUE)

sapply(1:14, d_max, n =100)

X2 <- rexp(n)
X2 <- -log(cumsum(X2)/sum(X2))

hist(X1, prob=TRUE)
hist(X2, prob=TRUE)

