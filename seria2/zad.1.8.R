

p <- function(y,n=2) {
  return(y^{-n-1} *n)
}

curve(p, from= 0, to=1)