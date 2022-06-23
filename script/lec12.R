#pvalue of lm is if there is no relationship , and the feature is useless, pvalue is big. ANOVA for many features
#nested models. if you remove features, is it better if it was better than before?
#compare models: anova(reduced, full): correction for overfitting. if not significative means that full model not more useful
#when things are indipendent and normally distr, you can multiply.
#show it for multiple data point, then log

#simulate the data under the model: repeat 
#1. first draw x (normally distr)
#2. draw epsilon
#3. obtain y by fitting lm

x <- rnorm(1000, 0, sqrt(20))
esp <- rnorm(1000, 0, sqrt(0.7))
y <- 0.5 + 1.5 *x + esp
fit <- lm(y ~ x)
coef(fit)
coef(fit)['x']


#or
y <- rnorm(1000,  0.5 + 1.5 *x + sqrt(0.7))
#do it many times. how does hist look

infer_beta <- function(apha, beta, sigma.sd){
  y <- rnorm(length(x), ... )
  fit <- lm(y ~ x)
  return(coef(fit)['x'])
}


y ~ x *y #it will give you all possible features, 
#sepate intercept and slope for the category

y ~ x + z + x + z



data(mtcars)
predictors <- setdiff(colnames(mtcars), "mpg")
model <- "1"
candidates <- predictors
model_none <- lm(mpg ~ 1,  data = mtcars)
summary(model_none)


model_wt <- lm(mpg ~ 1 + wt, data = mtcars)
model_cyl <- lm(mpg ~ 1 +cyl, data = mtcars ) 
anova(model_none, model_cyl) #you could compare with which you get the lower pvalue for each combination but with function is faster

model = "1"








