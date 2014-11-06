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
