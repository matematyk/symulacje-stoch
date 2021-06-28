#zad3
n <- 10^6

X<- numeric(n)
for (i in 1:n){
  X1 <- runif(1)
  U <- runif(1)
  if (U <= 1-X1){
    Y = X1;
  } else {
    Y = X1 -1;
  }
  X[i] <- Y
}

hist(X, prob=TRUE)
curve(dnorm(x,0,1/2), add=TRUE)


#zad1

#zad2 
X<- numeric(n)
for (i in 1:n){
  V <- runif(1,0,1)
  X[i] <- log(V/(1-V))
}

plot(ecdf(X))
curve(exp(x)/(exp(x)+1), from=0, to=1,add=TRUE,col='red')

#zad3
n <- 10^4

X<- numeric(n)
for (i in 1:n){
 V <- runif(1,0,pi/2)
 X[i] = sin(V)^2
}

hist(X, probability =TRUE)

#curve(1- 2*x/(sqrt(1-x^2)), from=0,to=10,add=TRUE)

plot(ecdf(X)) #dystrybuanta empiryczna
curve(2/pi* asin(sqrt(x)), from=0, to=1, add=TRUE)
X<- numeric(n)
for (i in 1:n){
  V <- runif(1,0,1)
  X[i] <- ln(t/(1-t))
}

hist(X, prob=TRUE)



