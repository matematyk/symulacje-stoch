fun <- function(){
  k <- 0
  success <- FALSE
  while (!success) {
    k <- k + 1
    y <- runif(1)
    x <- 1/y
    u <- runif(1)
    
    success <- (u > 1/(1+x^2))
  }
  
  return(x)
}
N <- 10^6
X <- numeric(N)

for (i in 1:N) {
  X[i] <- fun()
}

hist(X[X<10],prob=TRUE)
curve(1/(x^2+1), from=0,to=10,add=TRUE)