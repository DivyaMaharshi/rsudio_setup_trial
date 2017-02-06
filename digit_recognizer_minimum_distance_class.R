source("packages.R")
source("logging.R")
library(party)
library(randomForest)
library(caret)
library(kernlab)
library(skmeans)
library(cluster)
library(fpc)

train <- read.csv('/home/igp/Downloads/train.csv', header=TRUE)
test <- read.csv('/home/igp/Downloads/test.csv', header=TRUE)

#####sampling && printing
set.seed(71)
data <- sample(as.integer(row.names(train[train$label==1,])),100)
par(mfrow=c(10,10),mar=c(0.1,0.1,0.1,0.1))

for (k in data)
{
  row <- NULL
  for (n in 2:785)
    row[n-1] <- train[k,n]
  
  matrix1 <- matrix(row,28,28,byrow=FALSE)
  image(matrix2, axes=FALSE, col=heat.colors(12))
}

rm(i,n,j,k,row,matrix1,matrix2)

#### train and test data
set.seed(25)
intest <- sample(1:42000,10000)
X_train <- train[-intest,-1]
Y_train <- train[-intest,1]
X_test <- train[intest,-1]
Y_test <- train[intest,1]

table(Y_train)
table(Y_test)


### REduce the size of training set
raw_train_set <- train[-intest,]
final_train_set <- NULL

for (i in 0:9)
{
  digit <- raw_train_set[raw_train_set$label==i,-1]
  set.seed(222)
  cluster <- kmeans(x = digit,centers = 50, iter.max = 20)
  new_data <- cbind(rep(i,50),cluster$centers)
  final_train_set<- rbind(final_train_set,new_data)
}
final_train_set<-as.data.frame(final_train_set)

#X_train <- final_train_set[,-1]
#Y_train <- final_train_set[,1]
#table (Y_train)

rm(digit,cluster,new_data,i,raw_train_set)
final_train_set <- round(final_train_set)
row.names(final_train_set) <- seq(1:nrow(final_train_set))
View(final_train_set)

#### random forest predictions
rf   <-  randomForest(V1~.,data=final_train_set,ntree=500)
#### predictions
pred <- predict(rf,X_test)


prediction <- data.frame(ImageId=1:nrow(test),Label=as.integer(pred))
write.csv(prediction, "rf_benchmark.csv")


