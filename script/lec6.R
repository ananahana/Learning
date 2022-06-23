library(data.table)
library(ggplot2)
library(tidyr)
library(magrittr)
library(plotly)
library(GGally)
library(ggbeeswarm)


dt <- data.table(pro_uptake = c(
  rnorm(3, 10100, 300), rnorm(4, 12100, 300), rnorm(3, 9850, 300),
  rnorm(4, 11100, 300), rnorm(4,8300, 300), rnorm(3,10050, 300),
  rnorm(3, 12000, 300), rnorm(3, 10020, 300), rnorm(3, 10080, 300), rnorm(3, 10070, 300) ),
  mutants = c(rep('WT',3), rep('T49A',4), rep('K227N',3), rep('A400V',4), rep('L421P',4),
              rep('I500T',3), rep('N591D',3), rep('A601T',3), rep('E684D',3), rep('G710R',3) )
  )


#sort by mean/median
#WT in one color and MT in another
#boxplot orders by factor (a data type, that have a default order)
# you have a vector x = c(A, B, C), transform to factor and order using levels x = as.factor(x) 
#factor(x, levels = x(C, B, A))
#as.factor(x)



dt[, median_per_mut := median(pro_uptake), by = mutants]
wt_med = unique(dt[mutants == 'WT', median_per_mut]) #get the median of the WT as a unique value
dt[, mutants:= factor(mutants, levels=unique(dt[order(median_per_mut), mutants]))]


# assign color by relation to WT
dt[, rel_to_wt := ifelse(median_per_mut < wt_med, 'Smaller than WT', 'Larger than WT'), by = mutants] # add another col that tells if larger or smaller than WT
dt[mutants == 'WT', rel_to_wt := 'WT']


#boxplot(group x axis, values y axis)
p <- ggplot(dt, aes(mutants, pro_uptake, fill = rel_to_wt)) + geom_boxplot() +
  geom_jitter(width = 0.4) +
  labs(y = "Proline Uptake")


p <- ggplot(dt, aes(mutants, pro_uptake, fill = rel_to_wt)) + geom_boxplot() + geom_beeswarm()
dt[, rel_to_wt := ifelse(median_per_mut < wt_med, 'Smaller', "Larger"), by = mutants]
dt[mutants == "WT", rel_to_wt := "WT"]





dt[, median := median(pro_uptake), by = "mutants"][order(median)] #adding additional col with median
dt[, .("median" = median(pro_uptake)), by = "mutants"][order(median)] #how to add the name to the new col but not adding a col to the dataframe
dt[,median(pro_uptake), by = "mutants"][order(V1)]


x_order_dt <- dt[, .("median" = median(pro_uptake)), by = "mutants"][order(median)][,mutants]

dt$mutants
#you see is alfabetically sorted
#sort them differently

dt[,mutants := factor(mutants, levels = x_order_dt)] # when you call the boxplot again, now it will oder it properly
levels(dt$mutants)

plottly(p)



summary_dt <- dt[, .(mean = mean(pro_uptake), sd = sd(pro_uptake)), by = "mutants"]
order <- dt[, mutants := factor(mutants)]

summary_dt[, color := ifelse(mean >= wt, "larger", "smaller")]




# question 2

titanic <- read.csv("extdata/titanic.csv")
head(titanic, n = 2)

T_dt <- as.data.table(titanic)
names(T_dt)

ggplot(T_dt, aes(factor(survived))) + geom_bar()

#use scatter plot to check for correlation among survived and clas
#however the two values have to be continues (class is NOT)

#Histograms instead

ggplot(T_dt, aes(survived, pclass)) + geom_point() # not what i wanted because first you need to tranform pclass to factor since it is a categorical value

ggplot(T_dt, aes(factor(survived), pclass)) + geom_bar() + facet_wrap(~pclass)

ggplot(T_dt, aes(x =  factor(pclass), fill = factor(survived))) + geom_point()


T_dt[, survived := ifelse(survived, 'survived', 'not suvived')] #, "survived column is an array of true and false (because 0 and 1), so when true it gives first of the list, hence "suvived"renamed the survived table from 1 to 0 to survived not survived
ggplot(T_dt, aes(factor(pclass), fill = factor(survived))) + geom_bar(position = 'fill')  #we see number but not ratio
#position fill is a way of normalizing it from 0 to 1
  


# is there age association to survival? check with boxplot
T_dt[, age]

ggplot(T_dt, aes(factor(survived), age)) + geom_boxplot()
ggplot(T_dt, aes(age)) + geom_histogram(color)

ggplot(T_dt, aes(factor(survived), age)) + geom_violin() + geom_jitter(alpha = 0.5)+facet_wrap(~pclass)

ggplot(T_dt, aes(factor(survived), age)) + geom_boxplot() + 
  #geom_jitter(alpha = 0.5)+
  facet_wrap(~pclass)


T_dt[, mean(age, na.rm = TRUE), by = "survived"]

age_S<- T_dt[survived == "survived", age_surv := age]
age_N<- T_dt[survived == "non survived", age_not := age]


#create a vector of ages of survived and not survived and do two histograms that overlap

ggplot(T_dt, aes(factor(survived), age)) + geom_boxplot() + 
  #geom_jitter(alpha = 0.5)+
  facet_grid(sex~pclass) #3D data


ggplot(T_dt, aes(factor(survived), sex)) + geom_boxplot() + 
  #geom_jitter(alpha = 0.5)+
  facet_wrap(~pclass)


#effect of sex


ggplot(T_dt, aes(factor(sex), fill = factor(survived))) +
  geom_bar(position = "fill") +
  facet_wrap(~pclass)

coffe <- fread("extdata/coffee.csv") #opened as datatable



#is there association?

# because cups is written in a strange way it is converted to string and as such is treated already as factor


ggplot(coffe, aes(factor(cups), coffee_risk_margin)) + geom_bar() #error because #bar plot is a 1D plot, but we are trying to do a 2D



ggplot(coffe, aes(factor(cups), coffee_risk_margin)) + geom_boxplot() #not a boxpoot because only 1 value


ggplot(coffe, aes(cups, coffee_risk_margin)) + geom_bar( stat = "identity")
#identity means you want to display the y not as counts (default), because it is going to be 1, but say you want to display the actual value of the table as count

ggplot(coffe, aes(packs, cig_risk_margin)) + geom_bar( stat = "identity")

#which one is counfounding? is it coffe or cigar causing the disease?
#use the facet to tell them apart

#facet conditional operation, if u condition of n of coffe you drink
#variable to condition is categorical

ggplot(coffe, aes(packs, cig_risk_margin)) +geom_bar( stat = "identity") +facet_wrap(~cups)
ggplot(coffe, aes(cups, cig_risk_margin)) +geom_bar( stat = "identity") +facet_wrap(~packs)



#can be done as well with color
ggplot(coffe, aes(cups, risk, fill = packs)) + geom_bar(stat = "identity", position = "dodge") +
  labs(x = )


#correlation plots 

cancer <- readRDS("extdata/cancer_data.rds")
cancer <- as.data.table(cancer, keep.rownames = "genes") #if you don't keep row names you will loose a column which is the row name (data tables by default don't have row names)
ggcorr(cancer, use = "pairwise")
ggcorr(cancer[, !"genes"]) #you tell it to ignore the genes col because not numeric

#FUK seems higher than any other value

#visualize the whole matrix, high dimensional data.
#melt the table to obtain a long one

cancer_m<- melt(cancer, id.vars = "genes", variable.name = "cancer")
ggplot(cancer_m, aes(cancer, genes)) + geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue")

#you see value that is way off

cancer_m[value == max(value)]
ggplot(cancer_m, aes(FUK, UGP2)) + geom_point()
cancer_m[genes == "DOHH2", FUK := NA]
cancer_m[genes == "DOHH2", UGP2 := NA]





