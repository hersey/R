#Topic: Decision Tree

#Definition: 

# Decision trees used in data mining are of two main types:
# 1. Classification tree analysis is when the predicted outcome is the class
# to which the data belongs.
# 2.Regression tree analysis is when the predicted outcome can be considered 
# a real number (e.g. the price of a house, or a patient’s length of stay in a hospital).

# CART:
# The term Classification And Regression Tree (CART) analysis is an umbrella term used to 
# refer to both of the above procedures.
# CART divides the predictor (x) space by successively splitting into
# rectangular regions and models the response (Y ) as constant over each region.

# This can be schematically represented as a “tree:”
# – each interior node of the tree indicates on which predictor 
# variable you split and where you split
# – each terminal node (aka leaf) represents one region and 
# indicates the value of the predicted response in that region

#Terms: 
# -Depth: max number of nodes in a path between root and terminal ->Measure the complexity of the tree
# -Terminal node: no children
# -Root node: Top node of the tree

#How it works:
# -Each step, the single best next split is the one that gives biggest reduction in SSE
# -Stop splitting when reduction in SSE with the next split is below a specified threshold
# -After fitting a CART model, spits out the final fitted tree for interpretation/prediction

#Create a dataset "dat"
set.seed(1234567)
n=30
x = runif(n)*3
y = sin(x*pi/2) + rnorm(n)/4
dat = data.frame(x=round(x,3), y=round(y,3))
xx = seq(0,3,.05)
plot(dat$x, dat$y, pch=16, xlab="x", ylab="y") 
lines(xx, sin(xx*pi/2))

#Run decision tree on it:
View(dat)
library(tree)
fit=tree(y~x,dat)
partition.tree(fit,main="Fitted Function")
points(dat$x,dat$y,pch=16)
plot(fit)
text(fit,cex=.8)  #see file dat_tree.png
deviance(fit)
fit
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 30 17.46000  0.1314  
# 2) x < 2.0285 19  2.20100  0.6628  
# 4) x < 0.6305 5  0.21970  0.3546 *
#   5) x > 0.6305 14  1.33600  0.7729  
# 10) x < 1.376 8  0.46690  0.9349 *
#   11) x > 1.376 6  0.37950  0.5568 *
#   3) x > 2.0285 11  0.62740 -0.7865  
# 6) x < 2.508 5  0.04279 -0.5398 *
#   7) x > 2.508 6  0.02650 -0.9922 *

#Interpretation: The result above shows how split happens. 

#Tree Pruning: 
fit2=tree(y~x,dat,minsize=1,mindev=1e-4) #minimize size of a node is 1; mindev is better to be small
summary(fit2)
# Regression tree:
#   tree(formula = y ~ x, data = dat, minsize = 1, mindev = 1e-04)
# Number of terminal nodes:  25 
# Residual mean deviance:  0.004736 = 0.02368 / 5 
# Distribution of residuals:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.104   0.000   0.000   0.000   0.000   0.104 
plot(prune.tree(fit2),main="Training set error")

#Plot CV deviance vs complexity parameter
plot(cv.tree(fit2,prune.tree),main="Test Set Error")

#Find final tree with best complexity parameter
fit3=prune.tree(fit2,best=5)
