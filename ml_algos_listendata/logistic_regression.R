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
pred = predict(mylogit,val[,-(1)], type = "response")
finaldata = cbind(val, pred)

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(pred ,finaldata$admit)

# Maximum Accuracy and prob. cutoff against it
acc.perf <- performance(pred_val, "acc")
ind = which.max( slot(acc.perf, "y.values")[[1]])
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]

# Print Results
print(c(accuracy= acc, cutoff = cutoff))

# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
perf_val

# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Plot the ROC curve
perf_val2 <- performance(pred_val, "tpr", "fpr")
plot(perf_val2, col = "green", lwd = 1.5)

#Calculating KS statistics
ks1.tree <- max(attr(perf_val2, "y.values")[[1]] - (attr(perf_val2, "x.values")[[1]]))
ks1.tree
