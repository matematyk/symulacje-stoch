N <- 10^{5}

I <- numeric(N)
I[1] <- 1 
alfa <- 10
beta <- 2
Tn <- numeric(N)

#(algorytm Gillespie’go).
for (i in 2:N) {
  K <- runif(1, 0, 1)
  if (K < alfa/(alfa+beta)){
    I[i] = I[i-1]+1
  } else {
    I[i] = I[i-1]-1
  }
  
  param <- (beta+alfa)*I[i-1]
  
  Wi <- rexp(1, param)
  #W[i] <- Wi
  Tn[i] <- Tn[i-1] + Wi
}

index <- length(Tn[Tn < 1])

#Wybierz kilka wartości (α, β)
#takich, że α > β. Co się dzieje, jeśli ten warunek nie jest spełniony?



plot(Tn[1:1000], I[1:1000] , type='s')
curve(exp(x*(alfa-beta)), add=TRUE)

## Zrób rysunek w skali logarytmicznej:
plot(Tn[1:1000], log(I[1:1000]) , type='s')

##Wygeneruj wiele trajektorii procesu losowego, oblicz i narysuj EI(t), medI(t), parę kwantyli
##(metodą Monte Carlo), porównaj z równaniem różniczkowym.
ostatni <- function(t, M, N, alfa, beta){
  Wynik <- numeric(M)
  for (j in 1:M) {
    I <- numeric(N)
    Tn <- numeric(N)
    I[1] <- 1
    for (i in 2:N) {
      K <- runif(1, 0,1)
      if (K < alfa/(alfa+beta)){
        I[i] = I[i-1]+1
      } else {
        I[i] = I[i-1]-1
      }
      param <- (beta+alfa)*I[i-1]
      if (param == 0){
        break
      }
      Wi <- rexp(1, param)
      
      Tn[i] <- Tn[i-1] + Wi
    } #N
    index <- length(Tn[Tn < t])
    Wynik[j] <- I[index]
  } #M
  return(Wynik)
}
Wynik <- ostatni(0.8, 10^{3}, 10^{3}, 4, 2)
mean(Wynik)
median(Wynik)
quantile(Wynik, 0.2)
exp(1.6)

