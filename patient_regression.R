#Read in patient satistisfaction dataset
patsat <- read.csv("~/GitHub/R/csv/patsat.csv")

#Obtain Anova table to decompose the regression sum of squares 
#into extra sum of squares associated with X2, with X1 given X2, and with X3 given X2 and X1
#(Note: Anova tells which data to bring in first)

fit=lm(y~x2+x1+x3,patsat)
anova(fit)

#Analysis of Variance Table
# Response: sales
# Df Sum Sq Mean Sq F value    Pr(>F)    
# ad         1 463451  463451 227.198 < 2.2e-16 ***
#   reps       1  59327   59327  29.084 4.167e-06 ***
#   Residuals 37  75475    2040                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1