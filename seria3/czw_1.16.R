library(MLMLPack)
library(MASS)

n <- 10^5

U <- rdirichlet(n, c(2,2,2))

M <- matrix(U[,1], U[,1] + U[2,], n, 2)

plot(M, pch=".", col="yellow")

contour(kde2d(M[,1], M[,2], add =TRUE, col ="red"))
        
dorder <- function(x,y){
  x[x>y] <- 0
  return(120*(x-y)*(1-y)*x)
}

x<- seq(0,1, by=0.01)
y<- seq(0,1, by=0.01)
contour(x,y, outer(x,y, FUN=dorder), add, col="blue")

cov(M)
