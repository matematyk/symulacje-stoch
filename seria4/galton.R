

sym <- function (n, lambda){
  x <- numeric(n)
  x[1] = 1

  for (i in 2:n){
      x[i] <- rpois(1,lambda*x[i-1])
  }

  return(x)
}

l1 <- 1.5
N <- 10^{3}
koniec <- numeric(N)
for (m in 1:N){
  n <- 10^{3}
  X <- sym(n, l1)
  koniec[m] <- X[n]
}
length(koniec[koniec==0])/length(koniec)




#plot(stepfun(c(1:(n-1)), Y))

#plot(Y ~ X, type = "s")
