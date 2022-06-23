## store the case study dataset in a folder “./extdata/”
## run following code

library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)

expression <- fread("./extdata/eqtl/expression.txt")
gene <- fread("./extdata/eqtl/gene.txt")
genotype <- fread("./extdata/eqtl/genotype.txt")
genotype <- genotype %>% melt(id.vars = 'strain', variable.name = 'marker', value.name = 'genotype')
growth <- fread("./extdata/eqtl/growth.txt")
growth <- growth %>% melt(id.vars = "strain", variable.name = 'media', value.name = 'growth_rate')
marker <- fread("./extdata/eqtl/marker.txt")

#' ### Get the marker
marker[chrom == "chr07" & start == 1069229, ]
mk <- marker[chrom == "chr07" & start == 1069229, id]
mk

#' ### Get genotypes of the marker
mk_geno <- genotype[marker == mk, .(strain, genotype)]
tab <- merge(growth[media == 'YPMalt'], mk_geno, by = 'strain') #only for 1mrk

ggplot(tab, aes(genotype, growth_rate)) +
  geom_boxplot() +
  #facet_wrap(~ media, scales = 'free') +
  labs(title = mk) + 
  theme(plot.title = element_text(hjust = 0.5))

#' ## Question: is that association statistically significant?
#is the number of values sufficient?
table(tab$genotype)

#simulate the data, by assuming there is no difference. Permute the genotype randomly

sample(LETTERS[1:10]) #random permutation
#copy the table otherwise it is going to change 
tab_rnd<- copy(tab)
tab_rnd[,genotype := sample(genotype)]
tab_rnd
tab

ggplot(tab_rnd, aes(genotype, growth_rate)) +
  geom_boxplot() +
  #facet_wrap(~ media, scales = 'free') +
  labs(title = mk) + 
  theme(plot.title = element_text(hjust = 0.5))



diff_med <- median(tab$growth_rate[tab$genotype=="Wild isolate"]) - median(tab$growth_rate[tab$genotype=="Lab strain"], na.rm = TRUE)

#compute the difference of median of wild vs median of Lab
diff_med <- function(dt){
  median(dt$growth_rate[dt$genotype=="Wild isolate"], na.rm =TRUE) - median(dt$growth_rate[dt$genotype=="Lab strain"], na.rm = TRUE)
}

#you can calculate for the "real" tab or the permuted one
diff_med_obs <- diff_med(tab)

#compute the diff of medians for 1000 tables with randomly permutated genotype values (without replacement)

vec <- sapply( 1:1000, function(i){
  tab_rnd[, genotype := sample(genotype)]
  diff_med(tab_rnd)
})

hist(vec)

#the prob to have that particular observation or more, by change. We observed a median of 2.17, 
#but the frequency of random differences is very low for higher than 1.5

#Monte Carlo permutations is what we did our P^ (is because r = 0 )1/1001

#EXERCISE LECTURE

#function that for each marker, merges by the col "strain" the growth and 
#a subset of the genotype table containing only the selected marker
# then further subsets it to take only one media

getMaltoseDt = function(mrk){
  growth_mrk <-  merge(growth, genotype[marker == mrk, .(strain, genotype)],
                       by = 'strain')
  growth_mrk[media == "YPMalt"]
}

# boxplot
plot_growth_one_mk <- function(mk){
  ggplot(getMaltoseDt(mk), aes(genotype, growth_rate)) +
    geom_boxplot() +
    labs(title = mk) + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))}
plot_growth_one_mk("mrk_5211")

#for a particular table, calucaltes diff of growth rate median between WT and Lab
median_diff <- function(dt){
    dt[genotype == 'Wild isolate', median(growth_rate, na.rm=T)] -
    dt[genotype == 'Lab strain', median(growth_rate, na.rm=T)]
  }


p_val_medians <- function(dt, N_permu = 1000){
  # It will return both a pvalue and plot a histogram of T_star T_ref <- median_diff(dt)
  T_ref <- median_diff(dt)
  T_star <- c()
  i=1
  while(i <= N_permu){
    T_star <- c(T_star, median_diff(dt[, genotype := sample(genotype)]))
    i <- i + 1 
    }
    hist(T_star, xlim = c(-3, 3))
    abline(v = T_ref, lwd = 2)
    p_val <- (sum(T_star > T_ref | T_star < -T_ref) + 1) / (N_permu + 1)
    p_val
}

p_val_medians(getMaltoseDt("mrk_5211"))

#remember that sample() permutates
#T_ref: the real median growth obtain in WT vs Lab
#T_star: for that marker, permutated only the WT or lab and
#obtain a difference in median growth rate betweeen them

#reference 
#pvalue will be = 1/1001, is the area outside the T_ref

plot_growth_one_mk("mrk_1653")
p_val_medians(getMaltoseDt("mrk_1653"))

plot_growth_one_mk("mrk_5091")
p_val_medians(getMaltoseDt("mrk_5091"))


#marker is associated with growth (has a small pvalue), it doesn't mean that is causal
# a statistic is T_ref meaning what are you testing? (previously the statistic was the difference between medians)

mks_geno <- genotype[marker %in% c('mrk_5091', 'mrk_5211')] %>% spread(marker, genotype) #combining the two tables 
#contingency table. To see if the two are associated. 
#It counts how many times both were WT for the same strain, both are Lab, one lab other WT and viceversa
cont <- table(mks_geno[, 2:3])


#Ho: the markers are not significally associated
#T statistic: number of times both markers have the same genotype (so if they are equal)
#if the two are associated, both will have an effect on growth. They are associated if they are found in the same conditions a lot


# It will return both a pvalue and plot a histogram of T_star T_ref <- median_diff(dt)

p_val_link <- function(mk2test, geno = mks_geno, N_permu = 1000){
  T_ref <- geno[, sum(get(mk2test) == mrk_5211)] / nrow(geno) #number of times they were the same
  
  T_star <- numeric(N_permu)
  for(i in 1:N_permu){
  geno[, mk2test] <- sample(geno[, get(mk2test)])
  T_star[i] <- geno[, sum(get(mk2test) == mrk_5091)]
  }
  hist(T_star, xlim = c(50, 110))
  abline(v = T_ref, lwd = 2, col = "red")
  p_val <- (sum(T_star > T_ref | T_star < -T_ref) + 1) / (N_permu + 1)
  return(p_val)
  }
p_val_link("mrk_5091")

##Question 3: Conditioning
#what does it mean conditions??? 
#coffe drinkers, split amog those who smoke and don't and check whether there is a difference
#in subsetting one with respect of the other
#check one condition (es growth of marker wt vs lab, within each, check growth status of the other marker )


# Does marker 5091 still associate with growth in maltose (YPMalt) when conditioned on marker 5211? 
#Define a null hypothesis,
# a statistics and use permutation testing to answer the question. 
# Strengthen your answer with a relevant plot. 
# Then, test if marker 5211 associates with growth in maltose when conditioned on marker 5091.
# Are the results the same? Discuss.


ggplot(dt, aes(genotype, growth_rate)) +
  geom_boxplot()+
  labs(title = paste(test_mk, "conditing on", conditioning_on = "mrk_5211"))

#obtain growth rate and genotype of both markers, for all strains




















