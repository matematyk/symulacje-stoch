lambda <- 0.2
t <- 200
n <- 200

## simulate exponential interarrivals the
W <- rpois(n = n,  lambda)
T <- c(0, cumsum(W))
plot(x =0:n, y = T, type = "s", xlim = c(0, t)) 

#ad2



e <- function(t, lambda){
  mean <- lamba
  
  return(mean)
}
n <- 20
t <- seq(from = 0, to = 20, by = 0.25)
n <- length(t)
W <- rpois(n = n-1,  lambda)
T <- c(0, cumsum(W))
plot(t, y = T, type = "s", xlim = c(0, 20)) 

sapply(t, function(x){ 1})