lambda <- 0.2
t <- 200
n <- 200

## simulate exponential interarrivals the
W <- rexp(n = n, rate = lambda)
S <- c(0, cumsum(W))
plot(x = 0:n, y = S, type = "s", xlim = c(0, t)) 

#ad2

X <- rexp(n = n, rate = lambda)





e <- function(T1, T2){
 Y <- (T1 - T2)/ 2
  
  return(Y)
}
n <- 200
t <- seq(from = 0, to = 20, by = 0.25)
n <- length(t)
W <- rexp(n = n-1,  lambda)
T <- c(0, cumsum(W))
plot(t, T , type = "s", xlim = c(0, 20)) 

y <- numeric(length(T))
for (i in 1:(length(T)-1)){
  y[i] <- ((T[i+1] -T[i])/2)
}




plot(t,y)
