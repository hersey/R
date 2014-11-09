#Quality Control Case: How to identify and deal with multicollinearity
quality <- read.csv("~/GitHub/R/csv/quality.csv")
view(quality)
round(cor(quality),3)
#Result of correlation. Note strong negative corr between temp/density,density/rate,
#          temp   density   rate  am   defect
# temp     1.000  -0.959  0.908  0.563  0.929
# density -0.959   1.000 -0.915 -0.461 -0.923
# rate     0.908  -0.915  1.000  0.581  0.885
# am       0.563  -0.461  0.581  1.000  0.535
# defect   0.929  -0.923  0.885  0.535  1.000

#Draw scatterplot and observe correlations among variables.
library(car)
scatterplot.matrix(~temp+density+rate+am+defect, quality)
fit=lm(defect~.)

#Regress on all variables to predict defect
fit<-lm(defect~.,quality)
summary(fit)

#Test on VIF:
vif(fit)
#Note: Very high value for temp and density
# temp   density      rate        am 
# 16.314613 18.205395  7.706898  1.926129 


#Note: Overall model is significant but non of the individual coefficients are. Sign for multicollinearity.
# Call:
# lm(formula = defect ~ ., data = quality)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.3478  -3.7614  -0.7298   3.0401  15.8935 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) 31.14457   77.09396   0.404    0.690
# temp        13.95814    9.26003   1.507    0.144
# density     -2.24388    1.69777  -1.322    0.198
# rate         0.08883    0.14253   0.623    0.539
# am           1.94741    3.65017   0.534    0.598
# 
# Residual standard error: 7.203 on 25 degrees of freedom
# Multiple R-squared:  0.8813,  Adjusted R-squared:  0.8623 
# F-statistic: 46.42 on 4 and 25 DF,  p-value: 3.23e-11

#Test if only regress AM against defect
fit1=lm(defect~am,quality)
summary(fit1)
#Note: coefficient for am is significant. There's 20.44 defect rate between am and pm shifts.
# Call:
#   lm(formula = defect ~ am, data = quality)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -35.86 -11.14   4.26  11.97  23.44 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   16.920      4.308   3.927  0.00051 ***
#   am            20.440      6.093   3.355  0.00229 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.69 on 28 degrees of freedom
# Multiple R-squared:  0.2867,  Adjusted R-squared:  0.2612 
# F-statistic: 11.25 on 1 and 28 DF,  p-value: 0.002295


#Test if regress AM and Rate against defect
fit2=lm(defect~am+rate,quality)
summary(fit2)
#Note: When adding "rate", there's no different between AM and PM shift.
# Call:
#   lm(formula = defect ~ am + rate, data = quality)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -18.0855  -5.0016  -0.8756   7.6177  22.2812 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -126.29287   18.29411  -6.903 2.03e-07 ***
#   am             1.19647    4.19152   0.285    0.777    
# rate           0.64619    0.08182   7.897 1.72e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.34 on 27 degrees of freedom
# Multiple R-squared:  0.7845,  Adjusted R-squared:  0.7685 
# F-statistic: 49.14 on 2 and 27 DF,  p-value: 1.004e-09