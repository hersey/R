#How to handle missing data when they are not random. 
#Solution: Treat missing as a seperate category and include a dummy.
#Step: 
#1. Create a dummy xmiss that equals 1 when x is miising. 
#2. When x is missing, set x=0
#3. Regress y on both x and xmiss. 

agemiss = data.frame(age = c(NA,NA,35,NA,81,39,20,25,62,NA,45,57,36,39,NA,48,36,NA,NA,30,78,35,NA,20,26,28,44,30,31,32,72,33,33,NA,55,37,36,43,40,NA),y = c(2.9,2.8,8.4,2.8,4.5,8.3,9.4,9.1,5.6,2.9,7.4,6.3,7.7,8.1,3.2,6.5,7.9,3.0,3.0,9.0,5.1,8.8,3.4,9.5,8.9,8.3,7.4,8.3,8.6,8.7,5.3,8.3, 7.8,3.2,6.6,8.4,8.6,7.8,7.6,3.7))

View(agemiss)

agemiss$xmiss=is.na(agemiss$age) #Create dummy: Take the na value in agemiss and assign to new variable xmiss
agemiss$age[is.na(agemiss$age)]=0 #Set missing to 0
plot(agemiss$age,agemiss$y)
fit=lm(y~age+xmiss,agemiss) #Regression
summary(fit)

#Model: 
#When age is present: y=11.04-0.08*age-7.95*0
#When age is absent: y=11.04-0.08*0-7.95=3.09 (When age is missing, y clusters around 3)

# Call:
#   lm(formula = y ~ age + xmiss, data = agemiss)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.66395 -0.19500 -0.00262  0.21501  0.61000 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 11.040189   0.163265   67.62   <2e-16 ***
#   age         -0.080755   0.003737  -21.61   <2e-16 ***
#   xmissTRUE   -7.950189   0.191438  -41.53   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3161 on 37 degrees of freedom
# Multiple R-squared:  0.9826,  Adjusted R-squared:  0.9817 
# F-statistic:  1045 on 2 and 37 DF,  p-value: < 2.2e-16