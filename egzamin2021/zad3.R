#3.(b) Przeprowadź symulacje i pokaż wygenerowane punkty.
n <- 10^4

X<- numeric(n)
Y<- numeric(n)
for (i in 2:n){
  Yp <- runif(1, X[i-1], 1)
  Xp <- runif(1, 0, Yp)
  X[i] = Xp
  Y[i] = Yp
}

#rozkład łaczny
plot(X,Y) 

#rozkład brzegowy X
hist(X,prob=TRUE)
curve(2-2*x, add=TRUE)

#rozkład brzegowy Y
hist(Y, prob=TRUE)
curve(2*x, add=TRUE)