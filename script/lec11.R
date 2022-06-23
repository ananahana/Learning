library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(BBmisc)



#simulate data from normal distr
sim_null <- function(sample_size = 50, N_experiments = 10000){
  sapply(seq(N_experiments), function(i){
    x <- rnorm(sample_size)
    y <- rnorm(sample_size)
    t.test(x,y, alternative = "two.sided")$p.value
  })
}


#visualize (with hist or) Q-Q blot: you can compare each point to expected value
plot_vals <- function(pvals, main = "", ...){
  par(mfrow = c(1,2))
  hist(pvals, main = paste("Histogram:", main),...)
  
  exp_quantiles <- qunif(ppoints(length(pvals)))
  plot(exp_quantiles, sort(pvals))
  grid();
  abline(0, 1, col= "firebrick")
}

pvals0 <- sim_null(sample_size = 50)
plot_vals(pvals0)

#pvals is the area under the curve
#quantiles are the x values
#QQ plots, data should lie on the diagonal if obs values they follow the same distr as the expected vals

exp_quantiles <- qunif(ppoints(length(pvals0)))
exp_quantiles <- ppoints(length(pvals0))
plot(exp_quantiles, sort(pvals0))
grid();
abline(0, 1, col= "firebrick")

pvals0 <- sim_null(sample_size = 50, N_experiments = 100)
plot_vals(pvals0)

pvals0
padj <- p.adjust(pvals0, method = "bonfe")
plot_vals(padj)

#when doing the QQ plots always 
#simulate data from non normal distribution

simulate_alt <- function(sample_size = 100, N_experiments = 10, mu_diff = 0.5){
  sapply(seq(N_experiments), function(i){
    a <- rnorm(sample_size)
    b <- rnorm(sample_size, mean = mu_diff)
    t.test(a, b)$p.value
  })
  }

#sample size is how big the initial samples drown from real distr are, that are then permuated
#N_experiments is the number of value you have in the plot
simulate_alt(sample_size = 50)
plot_vals(simulate_alt(10))
plot_vals(simulate_alt(100))
plot_vals(simulate_alt(1000))


#Q1.7

pvals <- c(
  sim_null(50, 10000),
  simulate_alt(50, 1000)
)

plot_vals(pvals)

#what tranformation to use on pvalue? the log tranformation, 
#since you are interested in the amplitude,therefore log10. 
#so you will know how many zeros before the value
#use negative log 10 to make it loook nicer

error_analysis <- function(method = "BH", sample_size = 50, cut = 0.05){
  pvals <-c( sim_null(sample_size, N_experiments = 10000), 
             simulate_alt(sample_size, N_experiments = 1000))
  names(pvals) <- rep(c("H0", "H1"), c(10000, 1000))
  pvals_adj <- p.adjust(pvals, method = method)
  table(ifelse(pvals_adj < cut, "sign", "non sign"), names(pvals))
}

error_analysis(sample_size = 10)
error_analysis(sample_size = 100)
error_analysis(sample_size = 1000)


#if QQ plot completely on diagonal it means observed distr is unifor, if slightly tailed, it is normal

library(data.table)
plot_qq <- function(vals, expected, main = "", ...){
  par(mfrow = c(1,2)) 
  hist(vals)
  plot(expected, sort(vals),
       xlim = c(-6,6),
       ylim = c(-6,6),
       ...)
       grid();
       abline(0, 1)
}

n <- 100
data <- data.table(
  rnorm_vals = sort(rnorm(n)),
  quantiles_vals = qnorm(ppoints(n))
)

plot_qq(data$rnorm_vals, data$quantiles_vals)
data$shift <- rnorm(n,4)
plot_qq(data$shift, data$quantiles_vals)


#Q2.3, increase standard deviation so that hist is broader compared to gaussian, so that edges of distr is above expected

data$broad <- rnorm(n, sd =2)
plot_qq(data$broad, data$quantiles_vals)

#how dows QQ plot will look like if obs is another distr

data$negbin <- rnbinom(n, mu = 100, size = 10)/100
plot_qq(data$negbin, data$quantiles_vals)


library(tidyr)
library(data.table)
library(ggplot2)
library(ggthemes)
gene <- fread("./extdata/eqtl/gene.txt")
genotype <- fread("./extdata/eqtl/genotype.txt")
genotype <- genotype %>% melt(id.vars = 'strain', variable.name = 'marker', value.name = 'genotype')








