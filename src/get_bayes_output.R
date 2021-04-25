## function to get Bayes model output

# priorType: "non" - non informative prior; "inf" - informative orior
# nIter: number of iteration in MCMC 
# latent: whether fit a latent variable model


get_bayes_output <- function(data, priorType, nIter,latent){
  if(priorType=="non" && latent==FALSE){
    genmod.string <- "model{
  
  ### prior distribution
   beta0 ~ dnorm(0, 0.1)
   beta1 ~ dnorm(0, 0.1)
   beta2 ~ dnorm(0, 0.1)

  ### statistical model
  for (i in 1:n) {
    y[i] ~ dbern(pr.y[i])
    logit(pr.y[i]) <- beta0 + beta1*x1[i] + beta2*x2[i]
  }
}"
  }
  
  if(priorType=="inf0" && latent==FALSE){
    genmod.string <- "model{
  
  ### prior distribution
   beta0 ~ dnorm(0, 0.1)
   beta1 ~ dnorm(0, 0.1)
   beta2 ~ dnorm(-0.6, 0.5)

  ### statistical model
  for (i in 1:n) {
    y[i] ~ dbern(pr.y[i])
    logit(pr.y[i]) <- beta0 + beta1*x1[i] + beta2*x2[i]
  }
}"
  }
  
  if(priorType=="inf1" && latent==FALSE){
    genmod.string <- "model{
  
  ### prior distribution
   beta0 ~ dnorm(0, 0.1)
   beta1 ~ dnorm(0, 0.1)
   beta2 ~ dnorm(0, 0.5)

  ### statistical model
  for (i in 1:n) {
    y[i] ~ dbern(pr.y[i])
    logit(pr.y[i]) <- beta0 + beta1*x1[i] + beta2*x2[i]
  }
}"
  }

  if(priorType=="inf2" && latent==FALSE){
    genmod.string <- "model{
  
  ### prior distribution
   beta0 ~ dnorm(0, 0.1)
   beta1 ~ dnorm(0, 0.1)
   beta2 ~ dnorm(0.6, 0.5)

  ### statistical model
  for (i in 1:n) {
    y[i] ~ dbern(pr.y[i])
    logit(pr.y[i]) <- beta0 + beta1*x1[i] + beta2*x2[i]
  }
}"
  }
  
  mod <- jags.model(textConnection(genmod.string),
                    data=list(x1=data$x1, x2=data$x2, y=data$y,n=dim(data)[1]), 
                    n.chains=4)
  
  ###  MC output comes out
  opt.JAGS <- coda.samples(mod, n.iter=nIter, 
                           variable.names=c("beta0","beta1","beta2")) 
  
  sum <- MCMCsummary(opt.JAGS)
  b_hat <- sum[3,1]
  se <- sum[3,2]
  ci_l <- sum[3,3]
  ci_u <- sum[3,5]
  cov <- ci_l <= trueb & trueb <=ci_u
  
  bayes.out <- c(b_hat, se, cov)
  bayes.out
}