### Test Data
set.seed(2016)
x0 = rep(1,30) #bias
x1 = rnorm(30,3,2) + 0.1*c(1:30)
x2 = rbinom(30, 1,0.3)
x3 = rpois(n = 30, lambda = 4)
x3[16:30] = x3[16:30] - rpois(n = 15, lambda = 2)
X = cbind(x0,x1,x2,x3)

  # dependent variable 
y = c(rbinom(5, 1,0.1),rbinom(10, 1,0.25),rbinom(10, 1,0.75),rbinom(5, 1,0.9))
Y <- as.matrix(y)

# Cost Function
cost <- function( beta ) { 
  p <- 1 / (1+exp(X%*%-beta))
  w <-  -(Y*log(p)) - (1-Y)*(log(1-p))
  return( sum(w) )
}

# Intial beta
initial_beta <- rep(0,ncol(X))

# Check the cost at inital theta
# cost(initial_beta)

# Derive theta using gradient descent using optim function
beta_optim <- optim(par=initial_beta, fn=cost)
beta_optim$par

#Refer. using R package 
model = glm(y~x1+x2+x3, family = "binomial")
model$coefficients
