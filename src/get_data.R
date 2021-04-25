## function to simulate data

# n: sample size
# missing: the type of missing mechanism. 
## "No"- no missing
## "MAR" - missing at random


expit <- function(z) {1/(1+exp(-z))}


get_data <- function(n, missing, b){
  x1 <- rbinom(n, size=1, prob=.4)
  x2 <- rbinom(n, size=1, prob=.2 + .5*x1)
  y <-  rbinom(n, size=1, 
               prob=expit(cbind(1,x1,x2) %*% 
                            c(-1, 0.2, b)))
  
  if(missing=="No") dat <- data.frame(x1=x1, x2=x2, y=y)
  
  if(missing=="MAR") {
    ### MAR
    r <- rbinom(n, size=1, prob=0.55-.3*x1*y)
    x2.obs <- rep(NA, n); x2.obs[r==1] <- x2[r==1]
    dat <- data.frame(x1=x1, x2=x2.obs, y=y)
  }
  dat
}