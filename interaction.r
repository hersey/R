#Topic: Interaction 
#Definition: an interaction[1][2] may arise when considering the relationship among 
#three or more variables, and describes a situation in which the simultaneous 
#influence of two variables on a third is not additive.

#Type 1: Both variables are categorical
#Here's a demonstration of Interaction using the oj dataset.
#The oj dataset demonstrate the stabiity of Vitamin C for three brands of OJ for up to a week.

oj = data.frame(brand=c(rep("A",12), rep("B",12), rep("C",12)),
                time = factor(rep(c(0,0,0,0,3,3,3,3,7,7,7,7), 3)),
                acid = c(52.6,54.2,49.8,46.5,49.4,49.2,42.8,53.2,42.7,48.8,40.4,47.6,56,48,49.6,48.4,48.8,44,44,42.4,49.2,44,42,43.2,52.5,52,51.8,53.6,48,47,48.2,49.6,48.5,43.4,45.2,47.6))

View(oj)

interaction.plot(oj$time,oj$brand,oj$acid,col=1:3)
#From the plot, can observe interaction between time and brand since the lines are not parallels. 

#Fit a regression model with acid against the time/brand interaction
fit=lm(acid~time*brand,oj) 
summary(fit)
drop1(fit,test="F")

#Model: This model suggest the additional variation explained by the interaction 
# Single term deletions
# Model:
#   acid ~ time * brand
#           Df Sum of Sq    RSS    AIC F value Pr(>F)
# <none>                  254.14 88.357               
# time:brand  4    17.301 271.44 82.728  0.4595 0.7647

fit1=lm(acid~time+brand,oj)
drop1(fit1,test="F")

# Single term deletions
# Model:
#   acid ~ time + brand
#         Df Sum of Sq    RSS     AIC F value    Pr(>F)    
# <none>              271.44  82.728                      
# time    2   226.676 498.12 100.583 12.9438 8.191e-05 ***
# brand   2    32.962 304.40  82.854  1.8822    0.1692 

#Type 2: Two numerical variables have interaction (use cheme dataset)
#Note: The interaction will be yˆ = b0 + b1x1 + b2x2 + b11x21 + b22x2 + b12x1x2
fit=lm(yield~time*temp+I(time^2)+I(temp^2),cheme)
summary(fit)
# Call:
#   lm(formula = yield ~ time * temp + I(time^2) + I(temp^2), data = cheme)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.23995 -0.18089 -0.03995  0.17758  0.36005 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.431e+03  1.529e+02  -9.360 3.30e-05 ***
#   time         7.809e+00  1.158e+00   6.744 0.000266 ***
#   temp         1.327e+01  1.485e+00   8.940 4.46e-05 ***
#   I(time^2)   -5.506e-02  4.039e-03 -13.630 2.69e-06 ***
#   I(temp^2)   -4.005e-02  4.039e-03  -9.916 2.26e-05 ***
#   time:temp    1.000e-02  5.326e-03   1.878 0.102519    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2663 on 7 degrees of freedom
# Multiple R-squared:  0.9827,  Adjusted R-squared:  0.9704 
# F-statistic: 79.67 on 5 and 7 DF,  p-value: 5.147e-06

b=coef(fit)[2:3]
B=matrix(c(coef(fit)[c(4,6,6,5)])*c(1,.5,.5,1),nrow=2)
xs=-solve(B)%*%b/2
xs
eigen(B)$values
size=30
x1=seq(75,95,length=size)
x2=seq(165,185,length=size)
#Use the regression model above to calculate y. 
y=outer(x1,x2,function(x1,x2)-0.001431+7.809*x1+13.27*x2-0.05506*x1^2 -0.04005*x2^2 + 0.01*x1*x2)
persp(x1,x2,y,xlab="Time",ylab="Temperature")  #see interaction_cheme.png

image(x1,x2,y,xlab="Time",ylab="Temperature") #see contour_cheme.png
contour(x1,x2,y,add=T)
points(jitter(cheme$time),jitter(cheme$temp))
points(xs[1],xs[2],pch="x",cex=4)
