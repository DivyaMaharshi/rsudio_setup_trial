library(ggplot2)
library(car)
library(caret)
library(corrplot)
data(mtcars)  

# categorical variables must be factors 
mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)

mtcars_a = subset(mtcars, select = -c(mpg))
#Identifying numeric variables
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]
#Calculating Correlation
descrCor <- cor(numericData)

# Checking Variables that are highly correlated
highlyCorrelated = findCorrelation(descrCor, cutoff=0.7)

#Identifying Variable Names of Highly Correlated Variables
highlyCorCol = colnames(numericData)[highlyCorrelated]
#### remove
dat3 = mtcars[, -which(colnames(mtcars) %in% highlyCorCol)]
dim(dat3)

##### building a regression model 
##### #Build Linear Regression Model
fit = lm(mpg ~ ., data=dat3)

#Check Model Performance
summary(fit)

#Extracting Coefficients
summary(fit)$coeff
anova(fit)

par(mfrow=c(2,2))
plot(fit)

summary(fit)$r.squared
AIC(fit)
BIC(fit)

#Stepwise Selection based on AIC
library(MASS)
step <- stepAIC(fit, direction="both")
summary(step)


#Backward Selection based on AIC
step <- stepAIC(fit, direction="backward")
summary(step)


#Forward Selection based on AIC
step <- stepAIC(fit, direction="forward")
summary(step)

n = dim(dat3)[1]
stepBIC = stepAIC(fit,k=log(n))
summary(stepBIC)

AIC(stepBIC)
BIC(stepBIC)

###########  5 k fold cross validation
library(DAAG)
kfold = cv.lm(data=dat3, stepBIC, m=5)