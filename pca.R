library(C50)
library(FSelector)
library(randomForest)
library(rpart)
library('caret')
data(churn)

#churntrain and churntest features
churnTrain = churnTrain[,! names(churnTrain) %in% c("state","area_code", "account_length") ]

###training and testing dataset
set.seed(2)
rows = sample(1:nrow(churnTrain), .70*(nrow(churnTrain)))
trainset = churnTrain[rows,]
testset = churnTrain[-rows,]
###pca
trainset$international_plan1 = ifelse(trainset$international_plan == 'no', 0, 1)
trainset$voice_mail_plan1 = ifelse(trainset$voice_mail_plan == 'no', 0, 1)
#trainset <- trainset[c(-1,-2, -17)]
#trainset_pca <- trainset[c(-1,-2,-17)]
#trainset_pca <- data.matrix(trainset_pca,rownames.force = 'NA')
#trainset.pca = prcomp(trainset_pca,center = TRUE,scale = TRUE)
#trainset.pca.data = predict(trainset.pca, newdata =  trainset)
#screeplot(churn.pca, type="barplot")

### prediction
#trainset_pca.df <- cbind(trainset_pca,trainset.pca.data[,1:12])
#trainset_pca.df <- data.frame(churn = trainset$churn, trainset_pca.df)
#trainset_pca.df <- trainset_pca.df[,1:14]

###rpart
#important_columns <- c('churn','international_plan1','voice_mail_plan1' , 'number_vmail_messages' , 'total_day_minutes' , 'total_day_calls' , 'total_eve_minutes' ,'total_eve_charge' , 'total_intl_minutes' , 'total_intl_calls','total_intl_charge', 'number_customer_service_calls')
# important_columns <- c('churn','PC1','PC2','PC3','PC4','PC5','PC6')
# trainset_pca.df = trainset_pca.df[,names(trainset_pca.df) %in% important_columns ]
#bestmtry <- tuneRF(trainset_pca.df[,2:15], trainset_pca.df[,1], stepFactor=1.5, improve=0.01, ntree=500,mtry=4)
#churn.rf = randomForest(churn ~ ., data=trainset_pca.df,ntree = 1000,importance=TRUE,mtry=4)
#plot(churn.rp, margin= 0.05)
#text(churn.rp, all=TRUE, use.n = TRUE)


# ####transform test to pca.test
# testset$international_plan1 = ifelse(testset$international_plan == 'no', 0, 1)
# testset$voice_mail_plan1 = ifelse(testset$voice_mail_plan == 'no', 0, 1)
# testset.pca <- testset[c(-1,-2,-17)]
# testset.pca <- predict(churn.pca, newdata = testset)
# testset.pca <- as.data.frame(testset.pca)
# testset.pca <- testset.pca[,1:10]
# 
###rf prediction
###
# tuneRF(trainset[-15], trainset$churn , stepFactor=2, improve=0.05,trace=TRUE,doBest=FALSE)
# churn.rf = randomForest(churn ~ ., data=trainset,ntree = 2000, importance=TRUE, mtry=4,do.trace=T)
# 
# testset$international_plan1 = ifelse(testset$international_plan == 'no', 0, 1)
# testset$voice_mail_plan1 = ifelse(testset$voice_mail_plan == 'no', 0, 1)
# testset <- testset[c(-1,-2)]
# # test11 <- testset[c(-1,-2,-17)]
# rf.prediction <- predict(churn.rf, testset[-15])
# confusionMatrix(table(rf.prediction, testset$churn))       


###pca analysis
trainset_pca <- trainset[c(-1,-2,-17)]
trainset_pca <- data.matrix(trainset_pca,rownames.force = 'NA')
trainset_pca <- scale(trainset_pca,center = TRUE,scale = TRUE )
#pca <- prcomp(trainset_pca)
trainset.pca = prcomp(trainset_pca)
#trainset.pca.data = predict(trainset.pca, newdata =  testset)
#screeplot(churn.pca, type="barplot")

### prediction
trainset_pca.df <- cbind(churn = trainset$churn,trainset.pca$x)
#trainset_pca.df <- trainset.pca.data[,1:12]
#trainset_pca.df  <- trainset.pca.data
#trainset_pca.df <- data.frame(churn = trainset$churn, trainset.pca[,1:10])
churn.pca.rf = randomForest(churn ~ ., data=trainset_pca.df,ntree = 2000, importance=TRUE, mtry=4,do.trace=T)
testset_pca <- testset[c(-1,-2,-17)]
testset_pca <- data.matrix(testset_pca,rownames.force = 'NA')
testset_pca <- scale(testset_pca,center = TRUE,scale = TRUE )
#pca <- prcomp(trainset_pca)
testset.pca = prcomp(testset_pca)
testset_pca.df <- cbind(churn = testset$churn,testset.pca$x)
rf.prediction <- predict(churn.pca.rf, data = testset_pca.df[-1])
confusionMatrix(table(rf.prediction, trainset$churn))    
