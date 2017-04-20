#Read Data File
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

#Summary
summary(mydata)

#Cross Tab
xtabs(~admit + rank, data = mydata)

#Data Preparation
mydata$rank <- factor(mydata$rank)

# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
train<-mydata[dt,]
val<-mydata[-dt,] 

#Run Logistic Regression
mylogistic <- glm(admit ~ ., data = train, family = "binomial")
summary(mylogistic)$coefficient

#Stepwise Logistic Regression
mylogit = step(mylogistic)

#Logistic Regression Coefficient
summary.coeff0 = summary(mylogit)$coefficient

#Calculating Odd Ratios
OddRatio = exp(coef(mylogit))
summary.coeff = cbind(Variable = row.names(summary.coeff0), OddRatio, summary.coeff0)
row.names(summary.coeff) = NULL



#R Function : Standardized Coefficients
stdz.coff <- function (regmodel) 
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
beta <-(3^(1/2))/pi * sx * b
return(beta)
}

std.Coeff = data.frame(Standardized.Coeff = stdz.coff(mylogit))
std.Coeff = cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) = NULL

#Final Summary Report
final = merge(summary.coeff, std.Coeff, by = "Variable", all.x = TRUE)

#Prediction
pred = predict(mylogit,val, type = "response")
finaldata = cbind(val, pred)

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(pred ,finaldata$admit)


