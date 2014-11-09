#Topic: Transformation 

#Reasons for transformations:
# 1. Heteroscedasticity: non-symmetric error distributions -->Transform the dependent variable
# 2. Underlying relationship nonlinear: Transform the predictor variables. 
# 
# Consider a model: β0 + β1xk
# When K=1: proportional returns to scale
# When K>1: increasing returns to scale
# When K<1: deminishing returns to scale
# 
# Rules of thumb: 
# 1. Take logs of an amount or count
# 2. Take logit of fractions or percents: log(p/(1-p))

#Using the R build-in dataset: airquality
View(airquality)
hist(airquality$Ozone) #Can see that Ozone is right-skewed.

#Testing normality: Using shapiro.test()
#The null hyphothesis: The distribution is normal. 
shapiro.test(airquality$Ozone)
#Result: Pvalue<0.05, reject the null. Not normal
# Shapiro-Wilk normality test
# 
# data:  airquality$Ozone
# W = 0.8787, p-value = 2.79e-08


#Transform Ozone into log:
logO=log(airquality$Ozone)
hist(logO)  #Can see now it's normalized



#Detect non-linearity in residual plot 
#Types of plot:
# 1. Residual plot: 
#   -if not random distribution, then indicate non-linear relationship
#    But this case, variance of residuals doesn't depend on yhat)
#   -if residual plot shows heteroscedasticity, then need to transform dependent variable
# 2. Normal Q-Q plot: points falling on a line indicate normality
# example: 
set.seed(2312)
z=rnorm(60) #create normal distribution data
qqnorm(z);qqline(z) 
qqnorm(c(z,8));qqline(c(z,8)) #Add in an outlier 8

#3. Residuals vs leverage: How much would the fitted value change without a particular observation—this way we can see
#the influence of a particular observation—whether it is an outlier, and how much leverage it has. 
#Cook’s distance: a value 1 or greater means you have an outlier. 

#Generating residual plots: 
plot(fit,which=1) #One of the four plots to specifiy, or run plot(fit) for all graphs.



2. 