get_summary <- function(x.RES){
  
  out <- c(mean(x.RES[,1]),
  sd(x.RES[,1]),
  sqrt(mean(x.RES[,2]^2)),
  mean(x.RES[,3]))
  
  names(out) <- c('b_hat', 'SE.em', "SE", "Cov")
  out
}

