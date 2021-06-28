n <- 10^3
X <- numeric(n)
theta <- 1/2
for (i in 2:n){
    x <- X[i-1]
    U <- runif(1)
    Y <- sample(c(min(x+1,0), max(x-1, 0)),1)
 
   a1 <- (theta/(1-theta))* (n-x)/(x+1) 
   if ( U < a1){
     X[i] = Y
   }
  
   a2 <- 1 - (((1-theta)/theta) * x/(n-x+1))
   if (U < a2) {
     X[i] = X[i-1]
   }
 }
   