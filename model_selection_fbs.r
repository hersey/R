#Topic: Model Selection
#Discuss the procedure of selecting the right models with forward/backward/stepwise. 
# 
# • Forward selection
# 1. Begin with no variables
# 2. Add variable that yields greatest significant improvement in SSE
# 3. Repeat (2) until no significant improvement in SSE
# • Backward elimination
# 1. Begin with all candidate variables
# 2. Drop variable variable that causes smallest non-significant increase in SSE
# 3. Repeat (2) until dropping variable causes significant increase in SSE
# • Stepwise selection
# 1. (Usually) begin with no variables
# 2. Drop variables that causes smallest non-significant decrease in SSE
# 3. Add variable that yields greatest significant improvement in SSE
# 4. Repeat (2) and (3) until no improvement in SSE

#Using click.csv example to demonstrate stepwise regression
View(click)
#create dummy variables for categorical data "eff"
#Note: when fair=0,good=0 and outstanding=0 -->eff=Poor
click$fair[as.factor(click$eff)==2]=1
click$fair[is.na(click$fair)]=0
click$good[as.factor(click$eff)==3]=1
click$good[is.na(click$good)]=0
click$outstanding[as.factor(click$eff)==4]=1
click$outstanding[is.na(click$outstanding)]=0


#Stepwise method:
fit=lm(sales~1,click)
fit2=step(fit,scope=~ad+reps+fair+good+outstanding,test="F")

# Start:  AIC=386.52
# sales ~ 1
# 
# Df Sum of Sq    RSS    AIC  F value    Pr(>F)    
# + reps         1    465161 133092 328.40 132.8114 5.739e-14 ***
#   + ad           1    463451 134802 328.91 130.6445 7.327e-14 ***
#   <none>                     598253 386.52                       
# + good         1     24847 573406 386.82   1.6466    0.2072    
# + fair         1      9076 589177 387.90   0.5854    0.4489    
# + outstanding  1      6008 592245 388.11   0.3855    0.5384    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Step:  AIC=328.4
# sales ~ reps
# 
# Df Sum of Sq    RSS    AIC  F value    Pr(>F)    
# + ad           1     57617  75475 307.71  28.2458 5.317e-06 ***
#   <none>                     133092 328.40                       
# + outstanding  1      6008 127084 328.55   1.7492    0.1941    
# + fair         1      2160 130932 329.74   0.6104    0.4396    
# + good         1      2086 131006 329.76   0.5891    0.4476    
# - reps         1    465161 598253 386.52 132.8114 5.739e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Step:  AIC=307.71
# sales ~ reps + ad
# 
#               Df Sum of Sq    RSS    AIC F value    Pr(>F)    
# <none>                      75475 307.71                      
# + outstanding  1      3273  72202 307.93  1.6317    0.2096    
# + fair         1      1289  74185 309.02  0.6257    0.4341    
# + good         1         5  75470 309.70  0.0022    0.9626    
# - ad           1     57617 133092 328.40 28.2458 5.317e-06 ***
# - reps         1     59327 134802 328.91 29.0842 4.167e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Backward method:
fit=lm(defect~.,quality)
summary(fit)
step(fit) #Backward selection, starting with all variables
# Call:
#   lm(formula = defect ~ ., data = quality)
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
# 
# 
# Start:  AIC=123
# defect ~ temp + density + rate + am
# 
#           Df Sum of Sq    RSS    AIC
# - am       1    14.767 1311.8 121.34
# - rate     1    20.149 1317.2 121.46
# <none>                 1297.0 123.00
# - density  1    90.624 1387.6 123.03
# - temp     1   117.878 1414.9 123.61
# 
# Step:  AIC=121.34
# defect ~ temp + density + rate
# 
#           Df Sum of Sq    RSS    AIC
# - rate     1    40.934 1352.7 120.26
# - density  1    76.163 1387.9 121.03
# <none>                 1311.8 121.34
# - temp     1   189.021 1500.8 123.38
# 
# Step:  AIC=120.26
# defect ~ temp + density
# 
#           Df Sum of Sq    RSS    AIC
# <none>                 1352.7 120.26
# - density  1    142.69 1495.4 121.27
# - temp     1    258.24 1611.0 123.50
# 
# Call:
#   lm(formula = defect ~ temp + density, data = quality)
# 
# Coefficients:
#   (Intercept)         temp      density  
# 46.256       18.049       -2.329 


#Forward method: 
null=lm(defect~1,quality)
full=lm(defect~.,quality)
step(null,scope=list(lower=null,upper=full),direction="forward")

#Backward method: 
step(full,data=quality,direction="backward")

#Stepwise:
step(null,scope=list(upper=full),data=quality,direction="both")