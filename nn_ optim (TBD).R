
sigmoid_ <- function( x ) {
  return(  1 / (1 + exp(-x)) )
}


costW <- function( par, y, x, v ){ 
  w <- as.matrix(par)
  
  h <- sigmoid_(x%*%w)
  y_hat <- sigmoid_(h%*%v)
  
  err <- sum( (y-y_hat)^2 )/2
  return( err )
}


costV <- function( par, y, x, w ){ 
  v <- par
  
  h <- sigmoid_(x%*%w)
  y_hat <- sigmoid_(h%*%v)
  
  err <- sum( (y-y_hat)^2 )/2
  return( err )
}


nn_ <- function(x, y, hiddenSize = 5, learn_rate = 0.0001 , iterations = 10 ) {
  x <- as.matrix(x)
  
  d <- ncol(x) # +1 ???
  p <- hiddenSize
  m <- ncol(x)
  
  w <- matrix(rnorm(d * p), d, p)
  v <- as.matrix(rnorm(p )) # p+1 ??
  
  for (i in 1:iterations) {
    h <- sigmoid_(x%*%w)
    y_hat <- sigmoid_(h%*%v)
    
    w_optim <- optim(par=w, fn=costW, y=y, x=x, v=v) 
    v_optim <- optim(par=v, fn=costV, y=y, x=x, w=w) 
    
    w1 <- bp$w1
    w2 <- bp$w2
  }
  list(output = ff$output, w1 = w1, w2 = w2)
}


### ### DATA ### ###
set.seed(2016)
x0 = rep(1,30) #bias
x1 = rnorm(30,3,2) + 0.1*c(1:30)
x2 = rbinom(30, 1,0.3)
x3 = rpois(n = 30, lambda = 4)
x3[16:30] = x3[16:30] - rpois(n = 15, lambda = 2)

Y = c(rbinom(5, 1,0.1),rbinom(10, 1,0.25),rbinom(10, 1,0.75),rbinom(5, 1,0.9))
X = cbind(x0,x1,x2,x3)


### Run for Test ###
nn_(X, Y)


### Refer. using R method ###
library(nnet)
model <- nnet(Y~ ., data = cbind(X,Y), size = 2, decay = 5e-04)
summary(model)

