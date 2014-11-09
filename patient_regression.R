#Read in patient satistisfaction dataset
patsat <- read.csv("~/GitHub/R/csv/patsat.csv")

#Obtain Anova table to decompose the regression sum of squares 
#into extra sum of squares associated with X2, with X1 given X2, and with X3 given X2 and X1
#(Note: Anova tells which data to bring in first)

fit=lm(sat~x2+x1+x3,patsat)
anova(fit)
# Response: sat
# Df Sum Sq Mean Sq F value    Pr(>F)    
# x2         1 4860.3  4860.3 48.0439 1.822e-08 *** (When bring x2, explain 4860 total variation in sat score)
# x1         1 3896.0  3896.0 38.5126 2.008e-07 ***
# x3         1  364.2   364.2  3.5997   0.06468 .  
# Residuals 42 4248.8   101.2        