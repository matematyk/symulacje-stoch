

for (i in 1:10^4){
success <- FALSE

while(!success){
  q <- rexp(1)
  U <- runif(1)
  
  if (U < 1/(q+1)){
    success <- TRUE
  }
}

X[i] <- q

}

hist(X, prob=TRUE)
#curve(dexp, add=TRUE)
curve(1/c * exp(-x)/(x+1), from=0, to=10, add=TRUE)

 
n <- 10^3
x <- rexp(n, 1)

c<- mean(1/(1+x))
