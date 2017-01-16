library("C50")
library("gmodels")

credit <- read.csv("credit.csv")
credit <- credit[c(-18,-19,-20,-21)]
set.seed(123)
train_sample <- sample(1000, 900)

###training and testing labels
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

### decision tree classification model c50
credit_train$default<-as.factor(credit_train$default)
# credit_model <- C5.0(credit_train[-17], credit_train$default)
# 
# #####prediction 
credit_pred <- predict(credit_model, credit_test)

#### check errors 25% error rate
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


###########improve 
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

