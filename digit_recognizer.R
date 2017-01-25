source("packages.R")
source("logging.R")
library(party)
library(randomForest)
library(randomForest)
library(caret)
library(kernlab)

train <- read.csv('/home/igp/Downloads/train.csv', header=TRUE)
test <- read.csv('/home/igp/Downloads/test.csv', header=TRUE)

sample <- sample(1:nrow(train),50)
var <- t(train[sample,-1])
var_matrix <- lapply(1:50,function(x) matrix(var[,x],ncol=28))
opar <- par(no.readonly = T)
par(mfrow=c(5,10),mar=c(.1,.1,.1,.1))

for(i in 1:50) {
  for(j in 1:28) {
    var_matrix[[i]][j,] <- rev(var_matrix[[i]][j,])
  }
  image(var_matrix[[i]],col=grey.colors(225),axes=F)
}

nzr <- nearZeroVar(train[,-1],saveMetrics=T,freqCut=10000/1,uniqueCut=1/7)
sum(nzr$zeroVar)
cutvar <- rownames(nzr[nzr$nzv==TRUE,])
var <- setdiff(names(train),cutvar)
train <- train[,var]


label <- as.factor(train[[1]])
train$label <- NULL
train <- train/255
covtrain <- cov(train)


train_pc <- prcomp(covtrain)

#######variance vs components
varex <- train_pc$sdev^2/sum(train_pc$sdev^2)
varcum <- cumsum(varex)
result <- data.frame(num=1:length(train_pc$sdev),
                     ex=varex,
                     cum=varcum)

plot(result$num,result$cum,type="b",xlim=c(0,100),
     main="Variance Explained by Top 100 Components",
     xlab="Number of Components",ylab="Variance Explained")
abline(v=25,lty=2)

train_score <- as.matrix(train) %*% train_pc$rotation[,1:25]
#train_score <- as.matrix(train) %*% train_pc$rotation[,1:25]
train <- cbind(label,as.data.frame(train_score))
colors <- rainbow(length(unique(train$label)))
names(colors) <- unique(train$label)
plot(train$PC1,train$PC2,type="n",main="First Two Principal Components")
text(train$PC1,train$PC2,label=train$label,col=colors[train$label])

#####svm model 
svm_mdl <- randomForest(label~.,data=train,ntree= 2000, do.trace= T)

#### predictions
test <- test[,var[-1]]/255
test <- as.matrix(test) %*% train_pc$rotation[,1:25]
test <- as.data.frame(test)
pred <- predict(svm_mdl,test,type="response")
prediction <- data.frame(ImageId=1:nrow(test),Label=pred)
write.csv(prediction, "rf_benchmark.csv")
