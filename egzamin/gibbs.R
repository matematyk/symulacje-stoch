
k <- numeric(N)
x <- numeric(N)


for (i in 1:N){
  if (i == 1){
    x <- 0
    k[i] <- rpois(1,x)
  } else {
    k[i] <- rpois(1, x[i-1])
  }
  x[i] <- rgamma(1, k[i] + 1, 2)
  
}