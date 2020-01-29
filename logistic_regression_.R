set.seed(2016)
x0 = rep(1,30) #bias
x1 = rnorm(30,3,2) + 0.1*c(1:30)
x2 = rbinom(30, 1,0.3)
x3 = rpois(n = 30, lambda = 4)
x3[16:30] = x3[16:30] - rpois(n = 15, lambda = 2)
X = cbind(x0,x1,x2,x3)

y = c(rbinom(5, 1,0.1),rbinom(10, 1,0.25),rbinom(10, 1,0.75),rbinom(5, 1,0.9))


logistic_regression_ <- function(X, y)
{
  calc_p <- function(X,beta)
  {
    beta <- as.vector(beta)
    return ( exp(X%*%beta) / (1+ exp(X%*%beta)) )
  }  
  
  beta <- rep(0, ncol(X))
  iter_count <- 0
  repeat{
    #calculate probabilities using current estimate of beta
    p <- as.vector(calc_p(X, beta))
    
    #calculate matrix of weights W
    W <- diag(p * (1-p)) 
    
    #calculate the change in beta
    beta_change <- solve(t(X) %*% W %*% X) %*% t(X) %*% (y - p)
    
    #update beta
    beta <- beta + beta_change
    iter_count <- iter_count + 1 
    
    # when it converse or not-conervse, stop iteration
    if ( sum(beta_change^2) <  0.0001 || iter_count > 100 ){  
      break 
    }
  }

    return( coef = c("(Intercept)" = beta[1], x1 = beta[2], x2 = beta[3], x3 = beta[4]) )
}

logistic_regression_(X,y)


#using R package 
M1 = glm(y~x1+x2+x3, family = "binomial")
M1$coefficients

