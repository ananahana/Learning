library(data.table)
library(ggplot2)
library(ggbeeswarm)
library(tidyr)
library(magrittr)
library(ggthemes)

iris <- as.data.table(iris)

mpg <- as.data.table(mpg)
head(iris)
# How are the lengths and widths of sepals and petals distributed? Make one plot with multiple facets. 
# You will need to reshape your data so that the different measurements (petal length, sepal length, etc.) are in one column and the values in another. 
# which is the best plot for visualizing distributions; .

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point()
ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()
ggplot(iris, aes(Species, Sepal.Length)) + geom_violin()
ggplot(iris, aes(Species, Sepal.Length)) + geom_violin() + geom_boxplot()+ geom_beeswarm()
ggplot(iris, aes(Species, Sepal.Length)) +  geom_beeswarm() + geom_violin() #first black dots then white violin! Is like adding layers in photoshop
ggplot(iris, aes(Sepal.Width)) + geom_histogram() #hist neeeds only 1 variable because you are plotting the density of 1 variable. Not comparing anything
ggplot(iris, aes(Sepal.Width)) + geom_histogram(bins = 100)
ggplot(iris, aes(Sepal.Width)) + geom_histogram(binwidth =0.4) # the width of the bin will be 0.4
ggplot(iris, aes(Sepal.Width)) + geom_density()
ggplot(iris, aes(Sepal.Width)) + geom_histogram(bins = 100)+geom_density()
ggplot(iris, aes(Sepal.Width)) + geom_histogram(stat = "density")
ggplot(iris, aes(Species)) + geom_bar(stat = "count")


newIris <- iris %>% gather("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", key = "variable", value = "value") #here you don't care about species
Iris <- iris %>% gather(-Species, key = "variable", value = "value") #here you don't care about species
ggplot(newIris, aes(value)) + geom_histogram() + facet_wrap(~ variable) +theme_bw()

ggplot(newIris, aes(value)) + geom_histogram(bins = 100) + facet_wrap(~ variable, scales = "free") #scales to free tells it to scale each facet indipendently
ggplot(newIris, aes(variable, value)) + geom_boxplot()

#facet means to divide each plot for each variable

ggplot(newIris, aes(value)) + geom_histogram(aes(fill = Species)) + 
  facet_wrap(~ variable, scales = "free") +
  ggtitle("one plot per variable") + theme_bw() #theme_bw background of the plot

ggplot(newIris, aes(value)) + geom_histogram() + 
  facet_grid(rows = vars(Species), cols = vars(variable), scales = "free")


ggplot(newIris, aes(variable, value)) + geom_boxplot() + geom_jitter() #gitter adds dots on top. boxplots not good because data bimodal and not homogeneusly distributed
ggplot(newIris, aes(variable, value)) + geom_violin() + geom_jitter() 
ggplot(newIris, aes(variable, value)) + geom_beeswarm()
ggplot(newIris, aes(variable, value)) + geom_jitter()


ggplot(newIris, aes(variable, value)) + geom_beeswarm(aes(col = Species))
ggplot(newIris, aes(variable, value)) + geom_boxplot(aes(fill = Species)) + theme_economist() + scale_fill_economist()
ggplot(newIris, aes(variable, value)) + geom_boxplot(aes(fill = Species)) + theme_bw(base_size = 15) + scale_fill_economist() #base_size changes the font size

ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(aes(col = Species)) + geom_smooth()

ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(aes(col = Species)) +
 geom_smooth(method = 'lm') +
 ggtitle("Relationship between petal length and width") +
 xlab("Petal Length") + ylab("Petal Width")


ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point() +
  geom_smooth(method = 'lm') + #lm is linear regression, versicolor is best corralated because of the confidence interval. Not because slope is slope = 1, 
  labs( x = "Petal Length", y = "Petal Width", title = "correlation")+
  theme_bw()

ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) + geom_point() +
  geom_smooth(method = 'lm') + #lm is linear regression, versicolor is best corralated because of the confidence interval. Not because slope is slope = 1, 
  labs( x = "Petal Length", y = "Petal Width", title = "correlation")+
  theme_tufte()


iris[, cor(Petal.Length,Petal.Width), by = Species]

anscombe_reshaped <- anscombe %>%
  as.data.table %>%
  .[, ID := seq(nrow(.))] %>%
  melt(id.var=c("ID")) %>%
  separate(variable, c('xy', 'group'), sep=1) %>%
  dcast(... ~ xy) %>%
  .[, group := paste0("dataset_", group)]
anscombe_reshaped[, mean(x), by = group]
anscombe_reshaped[, mean(y), by = group]
anscombe_reshaped[, sd(x), by = group]
anscombe_reshaped[, sd(y), by = group]
anscombe_reshaped[, cor(x,y), by = group]

#so far everything looks the same, is it the same data?
#variables can be different in different groups but have same mean and sd


ggplot(anscombe_reshaped, aes(x,y, color = group)) + geom_point() +
  facet_wrap(~ group)

ggplot(anscombe_reshaped, aes(x,y)) + geom_point() +
  facet_wrap(~ group)

ggplot(anscombe_reshaped, aes(x,y, color = group)) + geom_point() #all in one colored by group


mtcars
ggplot(mtcars, aes(cyl, mpg, group = cyl)) + geom_boxplot() #cyl is a factor, so if you don't specify this the box plot will not work
ggplot(mtcars, aes(as.factor(cyl), mpg)) + geom_boxplot()


#draw a boxplot from scratch

mtcars <- as.data.table(mtcars)
mtcars[, IQR(mpg), by = cyl]
mtcars[, mean(mpg), by = cyl]
mtcars[,mpg, by = cyl]

# cyl       V1
# 1:   6 19.74286
# 2:   4 26.66364
# 3:   8 15.10000


# cyl   IQR
# 1:   6 2.35
# 2:   4 7.60
# 3:   8 1.85

mtcars[, medians = median(mpg), by = cyl]
mtcars[, c("lq", "uq") := .(quantile(mpg, 0.25), qauntile(mpg, 0.75), by = cyl]
quantile(mtcars$mpg, 0.75)
mtcars[, IQR := 1.5 * IQR(mpg) , by = cyl]


ggplot(mtcars, aes(cyl, medians, ymax = up, ymin = lq)) +
  geom_crossbar(fill = "white", width = 1.3) +
  geom_segment(aes(cyl, down_wiskers, xend =cyl, yled = lq)) +
  geom_segment(aes(cyl, uq, xend =cyl, yled = up_whisker)) +
  
  


