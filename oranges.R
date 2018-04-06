## Lec. 1 R Code

library(MASS)
summary(Orange)

## Store the predictor and the response variables
tree.age = Orange$age[1:7]              ##Predictor variable
tree.circ = Orange$circumference[1:7]   ##Response variable 

## Create a simple model
simp.mod = lm(tree.circ ~ tree.age)

## Summarize the model
summary(simp.mod)

## Different options available for the model 
names(simp.mod)

## Plot the residuals. Here it plots all 7 residuals
plot(simp.mod$residuals)    

## Plotting the variables 
plot(tree.age, tree.circ, xlab="Age", ylab="Circumference", main="Simple Linear Regression") 
## Adding a line
abline(simp.mod, col="red")
## Plotting the fitted values
points(tree.age, simp.mod$fitted.values, pch=16, col="red")

## Check the help menu for the predict function
?predict()

## Creating the df
newOrange = data.frame(tree.age,tree.circ)
## Getting the confidence intervals
conf_int = predict(simp.mod, newOrange, interval="confidence", level=0.95) 
## Getting the preidction intervals
pred_int = predict(simp.mod, newOrange, interval="predict", level=0.95) 


## The prediction intervals have more variance than the confidence intervals

## Adding Lines to this. The lines in red are the confidence intervals and the lines in blue are the prediction intervals
lines(tree.age, conf_int[,2], col="red", lty=2)
lines(tree.age, conf_int[,3], col="red", lty=2)
lines(tree.age,  pred_int[,2], col="blue", lty=2)
lines(tree.age,  pred_int[,3], col="blue", lty=2)


## Lecture 2 notes for the Oranges Dataset

## Store the number of observations for the dataset
n=length(Orange[,1])

## Creating the predictor variables and the response variables
tree.circ = Orange$circumference              ##Response variable 
tree.age = Orange$age                         ##Predictor variable 1

## Creating a soil variable using random numbers
tree.soil.ph = round(rnorm(n, 6.75, 1), 2)    ##Predictor variable 2

## Creating a model based on the variables
mod1 = lm(tree.circ ~ tree.age + tree.soil.ph)
summary(mod1)

## Adding a categorical variable 
tree.type = Orange$Tree                      ##Predictor variable 3
## Making categorical levels
is.factor(tree.type)
is.ordered(tree.type)
tree.type = factor(tree.type, ordered=FALSE)

## Creating a new model with a categorical variable
mod2 = lm(tree.circ ~ tree.age + tree.soil.ph + tree.type)
summary(mod2)

names(Orange)

## Lecture 3 notes for the Orange dataset

n=length(Orange[,1])

## Creating the response and the predictor variables
tree.circ = Orange$circumference + rnorm(n, 0, 1)               ##Response variable 
tree.age = tree.circ/pi * (4.5 + rnorm(n,0,.8))                 ##Predictor variable 1
tree.soil.ph = round(rnorm(n, 6.75, 1), 2)                      ##Predictor variable 2
mean(tree.soil.ph)

## Creating the Linear Model
mod1 = lm(tree.circ ~ tree.age + tree.soil.ph)
summary(mod1)

## Creating the plot of the residuals
plot(mod1)

## Adding a categorical variable
tree.type = Orange$Tree
is.factor(tree.type)
is.ordered(tree.type)

tree.type = factor(tree.type, ordered=FALSE)

## Creating the linear model. Type 3 is set as the reference level
mod2 = lm(tree.circ ~ tree.age + tree.soil.ph + tree.type)
summary(mod2)

## Changing the reference level to 1, and rerunning the model
## Let's change the reference group to Type 1
tree.type = relevel(tree.type, ref="1")
mod3 = lm(tree.circ ~ tree.age + tree.soil.ph + tree.type)
summary(mod3)

## Now we see that the only significant change is from Type 1 to Type 3

## Is the average value of the response significantly different for each type of tree? 
## (if we only model circumpherence by tree type)
## It is not significantly different because there are sign changes for a tree of each pair. All p-values are high
TukeyHSD(aov(tree.circ ~ tree.type), conf.level =  0.95)
plot(tree.circ ~ tree.type)


## Which variables are significant?  
aov(mod2)
TukeyHSD(aov(mod2), conf.level =  0.95)

## Comparing two models, one that includes tree type and another that does not include it.
anova(mod2)
anova(mod1,mod2)  ##Tree type appears significant 

## It seems that soil ph and tree type together are not that significant.
mod0 = lm(tree.circ ~ tree.age)
anova(mod0, mod2)



## Lecture 4 for the oranges dataset
## Make-up some Data for the example
n=length(Orange[,1])
tree.age = round(Orange$circumference/pi * (4.5 + rnorm(n,0,1)), 1)       ##Predictor variable 1
tree.soil.ph = round(rnorm(n, 6.75, 1), 2)                                ##Predictor variable 2 
tree.type = factor(Orange$Tree, ordered=FALSE)                            ##Predictor variable 3 (Reference level is Type 1)
tree.type = relevel(tree.type, ref="1")                       
tree.circ = rep(NA, 35)
tree.circ[1:7] = tree.age[1:7] + 0.8*tree.soil.ph[1:7] - 0.8 + rnorm(7, 0, 2)
tree.circ[8:14] = tree.age[8:14] + 1.5*tree.soil.ph[8:14] - 0.8 + rnorm(7, 0, 2)
tree.circ[15:21] = tree.age[15:21] + 2.2*tree.soil.ph[15:21] - 0.8 + rnorm(7, 0 , 2)
tree.circ[22:28] = tree.age[22:28] + 0.1*tree.soil.ph[22:28] - 0.8 + rnorm(7, 0, 2)
tree.circ[29:35] = tree.age[29:35] - 0.5*tree.soil.ph[29:35] - 0.8 + rnorm(7, 0, 2)
tree.circ = round(tree.circ, 2)                                            ##Response variable 
setwd()
save.image("myOranges.Rdata")

## Store the data in a variable
myOranges = data.frame(cbind(tree.circ, tree.age, tree.soil.ph, tree.type))



## Fit the MLR Model 
mod = lm(tree.circ ~ tree.age + tree.soil.ph + tree.type)
mod.sum = summary(mod)


## Checking the linear hupothesis for changing a set of variable
library(gmodels)

T.mtx = rbind(c(0,1,0,0,0,0,0), c(0,0,1,0,0,0,0))
glh.test(mod, T.mtx)

## Low p-value, high F-statistic. This is a significant statistic

mod$coefficients
mod$residuals
mod$fitted.values

## Code to get the design matrix and the values
mod2 = lm(tree.circ ~ tree.age + tree.soil.ph + tree.type, x=TRUE, y=TRUE)
mod2$x ## Design matrix
mod2$y ## Observed Y vector

## Compute the hat matrix
X = mod2$x
Y = matrix(mod2$y, ncol=1)

## This is the calculation for the hat matrix
H.mtx = X %*% solve(t(X)%*%X) %*% t(X)

## Look at the dimensions
dim(H.mtx)
## checking if the values of the hat matrix equal the fitted values matrix
as.vector(round(H.mtx %*% Y,2)) == round(mod2$fitted.values, 2)

as.vector(round((diag(35)-H.mtx)%*%Y,2)) == round(mod2$residuals, 2)


## Performing the lack of fit test
## Lack of fit test
library(alr3)
anova(mod)
pureErrorAnova(mod)   ## Same here because there is no pure error in this example model

## Checking the null hypothesis if all the betas are 0
mod.sum$fstatistic

## As we can see the F-statistic is high so not all the betas are 0

## Influence Measures
print(influence.measures(mod))


## Plots
library(car)
library(MASS)y

## Scatter plots for individual variables
pairs(tree.circ ~ tree.age + tree.soil.ph + tree.type)

# Normality of errors
qqPlot(mod, distribution="norm", main="QQ Plot")
qqnorm(studres(mod))

## Lookinga at the distribution of the residuals
hist(studres(mod), freq=FALSE, main="Distribution of Studentized Residuals")
xfit=seq(min(studres(mod)),max(studres(mod)),length=50)
yfit=dnorm(xfit)
lines(xfit, yfit) 

# Constant variance
spreadLevelPlot(mod)

## Residuals vs. Predictor Variables - residuals seem normal
plot(tree.age, studres(mod), xlab = "Tree Age", ylab = "Studentized Residuals")
plot(tree.soil.ph, studres(mod), xlab = "Tree Soil Ph", ylab = "Studentized Residuals")
plot(tree.type, studres(mod), xlab = "Tree Type", ylab = "Studentized Residuals")
plot(mod$fitted.values, studres(mod), xlab = "Fitted Values", ylab = "Studentized Residuals")


# Leverage points
cutoff = 2*length(mod$coefficients)/n
lev = hat(model.matrix(mod))
plot(lev)
lev[lev>cutoff]

## There are no high leverage points in the data

# Cook's D - identify D values > 4/(n-k-1)
cutoff = 4/((nrow(myOranges)-length(mod$coefficients)-2))
plot(mod, which=4, cook.levels=cutoff)

## Based on Cook's distnce there are no points with values > 1 but 2 points with > 4/(n-k-1), which is 0.15 in this case


## Interaction vs. Correlation

et.seed(100)
## Simulate artificial data for this example 
x1 = rnorm(50, 7, 1.5)
x2 = x1 + rnorm(50, 0, 1.5)
x3 = rnorm(50, 2, 2)
rcoef = runif(5, -10, 10)
y = rcoef[5] + rcoef[2]*x1 + rcoef[4]*x2 + rcoef[1]*x3 + rcoef[3]*x3*x1 + rnorm(50, 0, 0.2)

par(mfrow=c(1,3))

plot(x1, x2, pch=16)   ##correlated
plot(x1, x3, pch=16)   ##uncorrelated
plot(x2, x3, pch=16)   ##uncorrelated 

par(mfrow=c(1,1))     ##interaction between X1 and X3 (the lines interset)

plot(x1,y, xlim=c(-5,14), ylim=c(-200, 200), pch = 16)
abline(lm(y~x1))


points(x3, y, col = "green", pch=16)
abline(lm(y~x3), col="green")

points(mean(x1), mean(y), col="darkgreen", pch = 8, lwd=2)
points(mean(x3), mean(y), col="darkgreen", pch = 8, lwd=2)
legend("topleft", legend= c("x1", "x3"), lty=c(1,1), lwd=c(1,1), col=c("black", "green"))


par(mfrow=c(1,1))     ##NO interaction between X1 and X2 (the lines are "nearly" parallel)
plot(x1,y,  pch = 16)
abline(lm(y~x1))
points(x2, y, col = "turquoise", pch=16)
abline(lm(y~x2), col = "turquoise")
points(mean(x1), mean(y), col="blue", pch = 8, lwd=2)
points(mean(x2), mean(y), col="blue", pch = 8, lwd=2)
legend("topleft", legend= c("x1", "x2"), lty=c(1,1), lwd=c(1,1), col=c("black", "turquoise"))


set.seed(100)
n = 500
x  = runif(n, 0, 1)                     ##Simulate some pretend predictor values
beta0 = 1.5
beta1 =  3.5

ynew = resid = y = v = rep(NA, n)       

for(i in 1:n){
  #v[i]= x[i]*(1-x[i])
  v[i] = x[i]^4
  #v[i] = x[i]^3 
  #v[i] = x[i]^2 
  #v[i] = x[i]       
  
  resid[i] = rnorm(1, 0, v[i])          ##Make sure errors have non-constant variance but are uncorrelated
  y[i] = beta0 + beta1*x[i] + resid[i]
  
  #ynew[i] = asin(sqrt(y[i]))            ##Note this will produce warnings and error if y is not between (0,1)
  ynew[i] = y[i]^(-1)
  #ynew[i] = y[i]^(-1/2)
  #ynew[i] = log(y[i])
  #ynew[i] = sqrt(y[i]) 
}

fit1 = lm(y~x)
fit2 = lm(ynew~x)


## Transforming the response using educated guess transformation
par(mfrow=c(1,2))
plot(fit1$fitted.values,fit1$residuals)  
plot(fit2$fitted.values,fit2$residuals, ylim=c(min(fit1$residuals),max(fit1$residuals)),col="green")

##Perform Weighted least squares on the untransformed-data
summary(fit1)
plot(x,y)
abline(fit1, col="red", lwd = 2)



##Unknown weights so use computed-assisted values
wts = 1/fitted(lm(abs(residuals(fit1)) ~ x))^2  ##weight = 1/fitted.value^2
fit3 = lm(y ~ x, weights=wts)
summary(fit3)       
summary(fit2) ##Note the variances of the estimates are much smaller!
abline(fit3, col="blue", lwd=2)

conf_int1 = predict(fit1, data.frame(cbind(x,y)), interval="confidence", level=0.95)
conf_int3 = predict(fit3, data.frame(cbind(x,y)), interval="confidence", level=0.95) 
mean(conf_int1[,3]-conf_int1[,2])
mean(conf_int3[,3]-conf_int3[,2])

plot(fit3$fitted.values,fit3$residuals)

