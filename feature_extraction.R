library(C50)
library(FSelector)
library(randomForest)
data(churn)
library('rpart')

#churntrain and churntest features
churnTrain = churnTrain[,! names(churnTrain) %in% c("state","area_code", "account_length") ]

###training and testing dataset
set.seed(2)
rows = sample(1:nrow(churnTrain), .70*(nrow(churnTrain)))
trainset = churnTrain[rows,]
testset = churnTrain[-rows,]

### weights of predictors
weights = random.forest.importance(churn~., trainset,importance.type = 1)
print(weights())
subset = cutoff.k(weights, 5)
f = as.simple.formula(subset, "Class")
print(f)


###evaluator functiomn

evaluator = function(subset) {
  print(subset)
  k = 5
  set.seed(2)
  ind = sample(5, nrow(trainset), replace = TRUE)
  results = sapply(1:k, function(i) {
    train = trainset[ind ==i,]
    test = trainset[ind !=i,]
    tree = rpart(as.simple.formula(subset, "churn"), trainset)
    error.rate = sum(test$churn != predict(tree, test,type="class")) / nrow(test)
    return(1 - error.rate)
  })
  return(mean(results))
}

attr.subset = hill.climbing.search(names(trainset)[!names(trainset) %in% "churn"], evaluator)
f = as.simple.formula(attr.subset, "churn")
print(f)


###random forest

###accuracy = 4.33 with 9 variables
churn.rf = randomForest(f , data = trainset, importance =T,ntree=1000)
churn.prediction = predict(churn.rf, testset)
table(churn.prediction, testset$churn)


### accuracy = 4.33 with 17 variables 
churn.rf = randomForest(churn ~. , data = trainset, importance =T,ntree=1000)
churn.prediction = predict(churn.rf, testset)
table(churn.prediction, testset$churn)