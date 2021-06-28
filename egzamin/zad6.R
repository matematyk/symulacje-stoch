alfa1 <- 5
lambda1 <- 5
alfa2 <- 6
lambda2 <- lambda1
a <- 5
n <- 10^6

X1 <- rgamma(n, alfa1, lambda1)
X2 <- rgamma(n, alfa2, lambda2)

Suma <- X1 + X2

theta_crud <- mean(Suma > a)
sigma2 <- var(Suma > a)


#important 

X1 <- rgamma(n, alfa1, lambda1-4) #generujemy z qi
X2 <- rgamma(n, alfa2, lambda2-4) #instrumentalne


SumaIS <- X1 + X2
vec <- SumaIS > a  #policzenie f(X) ze wzoru Tehta_is sumy

# policzenie stalych tylko dla 
c1 <- (1-4*lambda1)^{alfa1}
c2 <- (1-4*lambda2)^{alfa2}

#nie umiemy policzyc wag 
w1 <- 1/(exp(4*X1)*c1)
w2 <- 1/(exp(4*X2)*c2)

#przypadek gdy \lambda1=\lambda2
teta_s <- mean(vec*(dgamma(SumaIS, alfa1+alfa2, lambda1)/dgamma(SumaIS, alfa1+alfa2, lambda1-4)))
sigma2 <- var(vec*(dgamma(SumaIS, alfa1+alfa2, lambda1)/dgamma(SumaIS, alfa1+alfa2, lambda1-4)))
teta_s-2*sqrt(sigma2)/ (sqrt(n))
teta_s+2*sqrt(sigma2)/ (sqrt(n))

#qnorm(0.95,0,1) zle
#> qnorm(0.975,0,1)