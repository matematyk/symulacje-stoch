
m <- 10^6 #wykonuje się ok 10sec

R <- c()
for (i in 1:m){
  n <- 200
  X <- sample(c(-1,1), n, replace=TRUE)
  
  if (length(X[X==-1]) == 100){ #jesli na końcu ma być zero to liczba 1 i -1 ma być równa
      S <- cumsum(X)
      R <- c(R, max(S)- min(S))
  }

}
alfa <- 0.05
q <- round(qnorm(1-alfa/2),1)

dokl <- q* sd(R) /sqrt(m)

c(mean(R)-dokl, mean(R)+dokl) #przediał ufności