n <-5 
s2 <- 5
y <- 0 
m <- 5
v2 <-0.2
N <- 10^5

mi <- numeric(N)
kappa <- numeric(N)

for (i in 1:N){
  if (i == 1){
    m0 <- 1/2
    param <- (s2/2 + (n/2)* (m0 - y)^{2})
    kappa[i] <- rgamma(1, n/2, param)
  } else {
    param <- (s2/2 + (n/2)* (mi[i-1] - y)^{2})
    kappa[i] <- rgamma(1, n/2, param)
  }
  mean <- (kappa[i]*n*y + v2*m)/ (kappa[i]*n + v2)
  sd <- sqrt(1/(kappa[i]*n+v2))
  mi[i] <- rnorm(1, mean, sd)
}

plot(kappa, mi, pch='.')
contour(kde2d(kappa,mi), add=TRUE)

x <- seq(0, 5, by=0.01)
y <- seq(-2, 5, by=0.01)
f <- function(a,b) {
  return (exp(-a*(s2/2+(n/2)*(b-y)^2)-(v2/2)*(b-m)^2)*a^{(n/2)-1})
}
batch_numbers <- 10^{3}
S <- apply(matrix(kappa, nrow=batch_numbers, byrow=TRUE), MARGIN=1, sum)
var(S)* batch_numbers/ N
S_mi <- apply(matrix(mi, nrow=batch_numbers, byrow=TRUE), MARGIN=1, sum)
var(S_mi)* batch_numbers/ N

contour(x,y, outer(x,y, f), add=TRUE, col='blue')
