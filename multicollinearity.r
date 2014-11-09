#Multicollinearity
#Definition: predictor variables are highly correlated with one another.
#Effects:
#1. Unstable coefficients: model may be significant (F-test) but individual slopes (T-tests) are not; add/drop variables largely change coefficients.
#2. Incorrect signs: Counterintuitive coefficient signs. 
#Note: Extrapolation from multicollinear data model is very questionable. 

#Detection: 
#1. Compute data on correlation/scatterplot matrix and find large correlations.
#2. Unstable coefficients or incorrect signs
#3. Use variance inflation factor (VIF)
#Tolerance is 1-R^2 :fraction of variance unexplained by the model
#VIF=Variance Inflation Factor is 1/tolerance
#VIF measures how much variance of the estimated regression coefficient is inflated by the existence of correlation among predictor variables in the model. 
#Rule of thumb: VIF>4 needs further investigation. VIF>10 shows serious multicollinearity.

library(car)
round(cor(newfood),4)
#Notice the high correlation between volume/income. See VIF value below, shown sign of multicollinearity.

fit=lm(sales~price+ad+loc+volume+income,newfood)
vif(fit)
#price       ad      loc   volume   income 
#1.079882 2.697664 1.005447 3.447143 3.367158 

#What to do with Multicollinearity:
#1. Use orthogonal design to avoid multicollinearity
#2. Understand why has it: Causal relation? (x1-->x2); latent construct (w->x1,w->x2)
#3. Use shrinkage estimation: PCR, PLS, Ridge regression, lasso etc.

