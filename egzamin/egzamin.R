N <- 10^3
lambda <- 10
X <- rexp(N)
Y <- X/lambda
c <- 5


estym <- mean(1/(X+c)*exp(-c))
estym

estym <- exp(-lambda*c)*mean(1/(Y+c)*exp(-Y-c)/(lambda*exp(-lambda*(Y+c))))
estym

M <- 1000

out1 <- numeric(M)
out2 <- numeric(M)
out3 <- numeric(M)
for(i in 1:M){
  N <- 10^3
  c <- 5
  X <- rexp(N)
  lambda <- 0.1
  Y <- X/lambda
  out1[i] <- exp(-lambda*c)*mean(1/(Y+c)*exp(-Y-c)/(lambda*exp(-lambda*(Y+c))))
  lambda <- 1
  Y <- X/lambda
  out2[i] <- exp(-lambda*c)*mean(1/(Y+c)*exp(-Y-c)/(lambda*exp(-lambda*(Y+c))))
  
  lambda <- 10
  Y <- X/lambda
  out3[i] <- exp(-lambda*c)*mean(1/(Y+c)*exp(-Y-c)/(lambda*exp(-lambda*(Y+c))))

}
var(out1)
var(out2)
var(out3)

#> var(out2)
#[1] 1.263317e-06
#> var(out2)
#[1] 2.139466e-11
#> var(out2)
#[1] 6.523563e-09