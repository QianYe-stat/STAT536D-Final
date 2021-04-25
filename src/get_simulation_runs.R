


get_simulation_runs <- function(sample.size, nrun, missing.mech, trueb, nIter){

  glm.RES <- non.RES <- inf0.RES <- inf1.RES <- inf2.RES <- c()
  
  for(rep in 1: nrun) {
    cat("\n\n This is run", rep, "for sample size n=", n.pool[i], "\n\n")
    
    fit.glm <- b_hat <- 0
    class(fit.glm) <- class(b_hat) <- "try-error"
    while(class(fit.glm)[1]=="try-error"|class(b_hat) == "try-error"){
      
      dat <- get_data(n=sample.size, missing=missing.mech, b=trueb)
      
      ## classical glm
      fit.glm <- try(glm(y~x1+x2, family=binomial, data=dat))
      glm.coef <- summary(fit.glm)$coef
      b_hat <- try(glm.coef[3,1])
      
    }
    
    se <- glm.coef[3,2]
    ci_l <- b_hat-1.96*se
    ci_u <- b_hat+1.96*se
    cov <- ci_l <= trueb & trueb <=ci_u
    
    glm.out <- c(b_hat, se, cov)
    glm.RES <- rbind(glm.RES, glm.out)
    
    
    if(missing.mech=="No") latent <- FALSE 
    if(missing.mech=="MAR") latent <- TRUE 
    ## bayes with non-informative prior
    non.out <- get_bayes_output(data=dat, priorType="non", nIter=nIter, latent = latent)
    non.RES <- rbind(non.RES, non.out)
    
    ## bayes with informative prior
    inf0.out <- get_bayes_output(data=dat, priorType="inf0", nIter=nIter, latent = latent)
    inf1.out <- get_bayes_output(data=dat, priorType="inf1", nIter=nIter, latent = latent)
    inf2.out <- get_bayes_output(data=dat, priorType="inf2", nIter=nIter, latent = latent)
    
    inf0.RES <- rbind(inf0.RES, inf0.out)
    inf1.RES <- rbind(inf1.RES, inf1.out)
    inf2.RES <- rbind(inf2.RES, inf2.out)
    
  }
  RES <- list(glm=glm.RES, non=non.RES, inf0=inf0.RES, inf1=inf1.RES, inf2=inf2.RES)
  map(RES,get_summary)
}