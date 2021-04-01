id <- function(Si){
  return (as.integer(Si > 0))
}
n <- 10^{3}
m <- 10^{2}

X_emp <- numeric(m)
for (k in 1:m) {
  X <- rnorm(n)
  trajektoria <- cumsum(X)
  
  Tn <- 1/n * sum(sapply(trajektoria, id))
  X_emp[k] <- Tn
}

hist(X_emp, prob=TRUE)

curve(dbeta(x,shape1=1/2, shape2=1/2), add=TRUE, col="blue")