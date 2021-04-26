rm(list=ls())
library(purrr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tibble)

output <- readRDS(here::here("data", "Nomissing_output.RDS"))

names <- names(output)

for(l in 1:length(output)){
  output[[l]] <- output[[l]] %>% 
    mutate(method=names[l],
           method=factor(method, levels=c("inf0","inf1", "inf2", "non", "glm")),
           n=factor(n),
           bmin=b_hat-SE.em,
           bmax=b_hat+SE.em) %>% 
    dplyr::select(method, everything())
     
}
plot_dat <- do.call(rbind,output)



cols <- c(brewer.pal(11, "Spectral")[c(2,3,9)], "gray47" ,"gray22")


gg1 <- plot_dat %>% filter(n %in% c(10, 20, 40, 50, 100, 500, 1000)) %>% 
  ggplot(aes(x=b_hat, y=n, group=method))+
  geom_point(aes(color=method), size=1.2, position=position_dodge(width = 0.5))+
  geom_errorbarh(aes(xmin=bmin, xmax=bmax, color=method), height=0.3,size=0.7, position=position_dodge(width = 0.5))+
  scale_color_manual(values=cols, name='Inference Method', na.translate = FALSE, 
                     labels=c("Bayes: prior center at -0.6", "Bayes: prior center at 0","Bayes: prior center at 0.6","Bayes: non-informative prior", "MLE"))+
  xlab("Estimates")+
  ylab("Sample Size")+
  guides(color = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0.6, linetype="dashed")

print(gg1)

ggsave(here::here("figures", "Estimates_full.pdf"))
  
gg2 <- plot_dat[-c(1,2),] %>% filter(n %in% c(10, 20, 40, 50, 100, 500, 1000)) %>% 
   ggplot(aes(x=b_hat, y=n, group=method))+
   geom_point(aes(color=method), size=1.2, position=position_dodge(width = 0.5))+
   geom_errorbarh(aes(xmin=bmin, xmax=bmax, color=method), height=0.3,size=0.7, position=position_dodge(width = 0.5))+
   scale_color_manual(values=cols, name='Inference Method', na.translate = FALSE, 
                      labels=c("Bayes: prior center at -0.6", "Bayes: prior center at 0","Bayes: prior center at 0.6","Bayes: non-informative prior", "MLE"))+
   xlab("Estimates")+
   ylab("Sample Size")+
   guides(color = guide_legend(reverse = TRUE)) +
   geom_vline(xintercept = 0.6, linetype="dashed")

print(gg2)


ggsave(here::here("figures", "Estimates.pdf"))
       

 
  