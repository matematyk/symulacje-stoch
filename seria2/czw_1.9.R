rmult.norm <- function(n, V){
  d <- dim(V)[1]
  Z <- rnorm(n*d)
  
  A<- t(chol(V)) #w r jest z lewej transpozycja
  Z <- matrix(Z, d, n)
  X <- A%*%Z
  return(matrix(X, n,d))
}
n <- 10^{4}
X<- rmult.norm(n, matrix(c(5,1,1,3), 2, 2))
plot(X[,1],X[,2])

hist(X[,1], probability = TRUE)
curve(dnorm(x, 0, sqrt(5)),add=TRUE)


