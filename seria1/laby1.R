#generowanie zmiennych losowych z rozkladu normalnego
n <- 5000
#S <- vector(n)
#numeric()
S<- numeric()
for(i in 1:n){
  U <- runif(12)
  S[i] <- sum(U) - 6

}
mean(S)
var(S) #var
sd(S) #standard
#hist(prob=TRUE) #prob=TRUE wazne, wyskalowanie nie 
hist(S, prob=TRUE) #biale pola sumuja sie do 1
curve(dnorm, add=TRUE) ## dodanie krzywej na wykres s, nie widac, gdy prob jest TRUE
pnorm()
plot(ecdf(S)) ## dystrybuanta empiryczna
curve(pnorm, add=TRUE)
#proba, vs teoria
ks.test(S,pnorm)
#wykres kwantylowy
qqnorm(S)
# min(S) to na po lwej stronie, 1/100 kwantu rzedu 1/100 , 1/5000
qnorm(1/100)
# druga, 2/100 
#niedokladnosci tylko w ogonach, generataor sie psuje w ogonach
shapiro.test(S) #za duża próbka, nie wykrylo!
# idealnie
S <- rnorm(n)