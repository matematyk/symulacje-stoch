

m <- 10^4
X <- numeric(m)
for (i in 1:m){
  sukces <- FALSE
  N = -1 
  p = 0.1
  
  while(!sukces){
    N = N+1
    U <- runif(1)
    
    sukces <- (U <p)
    
  }
  X[i] <- N
}

hist(X,prob=TRUE)