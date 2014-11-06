#run linear regression on click.csv dataset to see the effect of ad and click on sales
fit=lm(sales~ad,click)
plot(click$ad,click$sales,xlab="Number of TV spots", ylab="Sales")
abline(fit)
summary(fit)
plot(fit)

#The estimated regression model is y^=135+25.3ad

confint(fit)   #confidence interval of intercept and ad models

#Use this model to predict ad value. Be aware of extrapolation (predict data beyond original range)
predict(fit, data.frame(ad=5))

#Find predictive interval
predict(fit,data.frame(ad=5),interval="prediction")
#Find confidence interval for the mean sales when there are 5 ads.
predict(fit,data.frame(ad=5),interval="confidence")

#Anova table
anova(fit)

#Multiple linear regression for click.csv
fit=lm(sales~ad+reps+eff,click)
summary(fit)

#The regression model is: y^=31.15+12.97*ad+41.25*reps+11.52*eff
#predict sales for ad=4, reps=1, eff=1
predict(fit,data.frame(ad=4,reps=3,eff=1))

#Since coefficient are not directly comparable because of differences in units of measurment,
#converting to standerdized regression coeeficients can help compare which variable is more 
#"important" in explaining the dependent variable

Zclick=as.data.frame(scale(click[,1:4]))
fit=lm(sales~ad+reps+eff-1,Zclick) #-1 drops intercept
summary(fit)


