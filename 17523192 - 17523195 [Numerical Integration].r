
library (matlib)

trapezoid <- function(f, a, b){
            if (is.function(f) == FALSE) {
                stop('f must be a function with one parameter (variable)')}
 
  h <- b - a
   
  fxdx <- (h / 2) * (f(a) + f(b))
   
  return(fxdx)
}


f <- function(x) {
  return(x^2 - x * sin(x))
}

trapezoid(f, 0, 5)

simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
   
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
   
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
   
  return(s)
}

f2 <- function(x) {
  return(x^2 - x * sin(x))
}

simpsons.rule(f2, 0, 5)
