n<- 10^{4}
#Zbadaj eksperymentalnie rozkład brzegowy X. Oblicz ten rozkład analitycznie.
theta <- runif(n)
X <- sapply(theta, function(x) { rbinom(1, 9, x)})

#zapominamy o U i patrzymy X 
#rozklad 2 wymiarowy X,U
#w 2 etapach
# wylosowalo sie 1000 puntkow 2 wymiarowych
# brzegowy, to nas U przestaje interesowac (marginalizujemy go)
# 

hist(X, prob=TRUE, breaks = -1:9)
table(X)
plot(table(X))
barplot(table(x))

experiments <- c(1:9)
X_exp <- numeric(10)

brzegowy <- function(k) {#jednostany dyskretny
  return (choose(9,k)* beta(k+1, 9-k+1 ))
}


X_exp <- sapply(0:9, brzegowy)

points(0:9, X_exp)

#Zbadaj eksperymentalnie rozkład a posteriori zmiennej θ przy X = 3, metodą ABC. Oblicz
#ten rozkład analitycznie. Porównaj.
X_3<- theta[X==3]
hist(X_3, prob=TRUE)
curve(dbeta(x, 3+1, 9-3+1), add=TRUE)


#Wypróbuj losowanie 2-etapowe w odwotnym porządku: wygeneruj X z rozkładu brzegowego,
#a następnie θ z rozkładu a posteriori. Sprawdź, że otrzymana zmienna θ ma brzegowy rozkład
#a priori.
temp <- sapply(0:9, brzegowy)
X_marginal <- sample(0:9, 9, replace= TRUE, prob=temp )#binomial
#
apost <- function(x){ #beta (alfa+x, beta + n - x) 
  return (rbeta(1, x+1, 1 - x + 9)) #unif(0,1) = beta(1,1)
}

theta_post <- sapply(X_marginal, apost )
hist(theta_post, prob=TRUE)

curve(dbeta(x,1,1),add=TRUE)
