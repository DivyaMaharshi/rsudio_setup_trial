train <- read.csv('/home/igp/Downloads/train.csv', header=TRUE)
test <- read.csv('/home/igp/Downloads/test.csv', header=TRUE)
###train_data
numTrees <- 25 
set.seed(123)
samp <- sample(nrow(train),.50* nrow(train))
train_data <- train[samp,]
train_data$label <- as.factor(train_data$label)
###test data
test_data <- test

library(e1071)
model <- svm(label ~ ., train_data, cost = 64, epsilon = 0.01)
predictY <- predict(model,test_data)
submit <- data.frame(Id = test$Id, Cover_Type = predictY)
write.csv(submit, file = "svmForestCover.csv", row.names = FALSE)


