
n <- 10^{3}
Y <- runif(n)
X_Y <- sapply(Y, function(x) { runif(1, 0, x)})

hist(X_Y, prob=TRUE)
p <- function(x){
  return(-log(x))
}
curve(p,add=TRUE)