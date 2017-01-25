library(C50)
library(FSelector)
library(randomForest)
library(rpart)
library('caret')
data(churn)
library(e1071)

#churntrain and churntest features
churnTrain = churnTrain[,! names(churnTrain) %in% c("state","area_code", "account_length") ]

###training and testing dataset
set.seed(100)

yes_train <- churnTrain[which(churnTrain$churn == 'yes'),]
no_train  <- churnTrain[which(churnTrain$churn == 'no'),]

trainset <- rbind(yes_train[1:483,],no_train[1:2850,])
#rows = sample(1:nrow(churnTrain), .70*(nrow(churnTrain)))
#trainset = churnTrain[rows,]
testset = rbind(yes_train[301:483,],no_train[301:2850,])

####weights of predictors
# churn.rp = rpart(churn ~ ., data=trainset)
# #plot(churn.rp, margin= 0.05)
# #text(churn.rp, all=TRUE, use.n = TRUE)
# 
# 
# ##prediction
# predictions = predict(churn.rp, testset, type="class")
# table(testset$churn, predictions)
# 
# 
# ###confusiomatrix
# confusionMatrix(table(predictions, testset$churn))
# which.min(churn.rp$cptable[,"xerror"])
# churn.cp = churn.rp$cptable[8,"CP"]
# 
# 
#####pruning
# prune.tree = prune(churn.rp, cp= churn.cp)
# plot(prune.tree, margin= 0.1)
# text(prune.tree, all=TRUE , use.n=TRUE)
# predictions = predict(prune.tree, testset, type="class")
# table(testset$churn, predictions)


####random forest
trainset = trainset[,names(trainset) %in% c('churn','international_plan','voice_mail_plan' , 'number_vmail_messages' , 'total_day_minutes' , 'total_day_calls' , 'total_eve_minutes' ,'total_eve_charge' , 'total_intl_minutes' , 'total_intl_calls','total_intl_charge', 'number_customer_service_calls' ) ]
churn.rf = randomForest(churn ~ ., data = trainset,ntree=1000,mtry=4)
#churn.rf1= errorest(churn ~ ., data = trainset, model =randomForest)
churn.prediction = predict(churn.rf, testset)
table(churn.prediction, testset$churn)
#importance(churn.rf)
#varImp(churn.rf)
#varImpPlot(churn.rf)


#####svm 
# trainset = trainset[,names(trainset) %in% c('churn','international_plan','voice_mail_plan' , 'number_vmail_messages' , 'total_day_minutes' , 'total_day_calls' , 'total_eve_minutes' ,'total_eve_charge' , 'total_intl_minutes' , 'total_intl_calls','total_intl_charge', 'number_customer_service_calls' ) ]
# #obj <- tune.svm(churn~., data = trainset, gamma = 2^(-1:1), cost = 2^(2:4))
# churn.svm = svm(churn ~ ., data = trainset,cost=4,gamma=0.5)
# #churn.rf1= errorest(churn ~ ., data = trainset, model =randomForest)
# churn.prediction = predict(churn.svm, testset)
# table(churn.prediction, testset$churn)

