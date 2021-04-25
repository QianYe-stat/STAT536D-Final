rm(list=ls())
require(rjags)
require(MCMCvis)
library(here)
library(purrr)
library(tibble)
library(dplyr)

set.seed(1234)
### sane output
options(digits=3)
options(width=65)

source(here::here("src", "get_data.R"))
source(here::here("src", "get_bayes_output.R"))
source(here::here("src", "get_summary.R"))
source(here::here("src", "get_simulation_runs.R"))

n.pool <- c(10, 50, 100, 500, 1000)
trueb <- 0.6

output <- vector("list", 5)
names(output) <- c('glm', 'non', 'inf0', 'inf1', 'inf2')

for(i in seq_along(n.pool)){
  cat("sample size is", n.pool[i])
  
  temp <- get_simulation_runs(sample.size = n.pool[i], nrun=500, missing.mech="No", trueb=0.6, nIter=10000)
  
  output <- map2(output,temp,function(x1, x2){
    as_tibble(rbind(x1,x2))
  })
  
}

output <- map(output, function(t){
  t %>% mutate(n=n.pool) %>% dplyr::select(n,everything())
})

saveRDS(output, file=here::here("data", "Nomissing_output.RDS"))


