<<<<<<< HEAD
#Problem Statement: 
# Instacart Market Analysis aimed at predicting which products will the consumer reorder, which can be
# considered as the classification problem that given the `user id` and `product id`, whether the products
# will be reordered by the user. 

#Logic flow: 
# There are four basic things that we need to do in this case:
# - EDA: Exploring the data helps us get a better understanding of the data, and helps us 
#        determine which feature we want to use / calculate to reflect the consumer behavior;
# - Feature Engineering: This step is vital. In order to analyze on which factors may lead to
#        consumer reordering behavior, we need to either extract or calculate some fields as input 
#        to help build the classifcation model;
# - Split data and merge features: While we have multiple files containing separate features, we need to do the splitting
#        and joining work before stepping into building the model; besides, since we donnot have labels
#        in the test data, we split training data into train and validation data for prediction
# - Model selection, evaluation and comparsion: After we did the previous steps, we can select the model based on
#        the features that we chose and the characteristics of the dataset; Since it is a classification
#        problem, we may need metrics to evaluate the results. In this case, we use confusion matrix.

#Step 1: load libraries and read tables
rm(list=ls())
library(dplyr)
library(data.table)
library(caret)
library(e1071)
library(ROCR)
library(pROC)
library(randomForest)
library(lightgbm)

aisles <- fread('aisles.csv')
category <- fread('departments.csv')
orders <- fread('orders.csv')
products <- fread('products.csv')
train_order <- fread('order_products__train.csv')
prior_order <- fread('order_products__prior.csv')

#Step 1.1 Data exploration
# Since we have three dataset: `order`, `order_products__train`, and `order_products__prior`, we want to
# check the data size of last two data and count the number of each eval set in `order` data to have an idea 
# of the relationship between three datasets
length(orders$eval_set[orders$eval_set == "prior"])#3214874
dim(prior_order)#(32434489,4), not all prior data are in the orders dataset
length(orders$eval_set[orders$eval_set == "train"])#131209
dim(train_order)#(1384617,4), not all train data are in the orders dataset
#check how many products have been purchased before
mean(prior_order$reordered)#0.5896975 --> 59% of products have been purchased before
table(prior_order$add_to_cart_order)#range from 1 - 145, the smaller add to order number is, the more items it will be
#check how many product categories
length(unique(category$department_id))#21 --> difficult to analyze on whether the product category has an influence on reorder behavior, so skipping this analysis in this case

#Step 2: feature engineering
# For feature engineering, we mainly consider two factors: 
# - factors regarding user feature, that is to say, the consumer habits that may influence their reorder behavior; 
# - factors regarding product feature, that is to say, the properties of the product that may influence the reorder behavior.

#Step 2-1: user feature
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}#write function mode for aggregation
#unique user id (key in user_fea)
user_id <- sort(unique(orders$user_id))
#2-1.1 number of orders by user
user_order_count <- aggregate(orders$order_id, by=list(orders$user_id), FUN=length)
#2-1.2 shopping frequency: avg shopping period since last shopping
user_days_since_prior_order_mean <- aggregate(orders$days_since_prior_order, by=list(orders$user_id), FUN = mean, na.rm = TRUE)

order_product_prior <- merge(x = prior_order, y = orders, by='order_id', all.x = TRUE)
#2-1.3 user product preference: count the type of products made by each user
user_product_count <- aggregate(order_product_prior$product_id, by=list(order_product_prior$user_id), FUN=n_distinct)
#number of products purchased by user per time
user_product_orderid_ratio <- user_product_count / user_order_count
#2-1.4 shopping time preference: mode of user order in dow and hod
user_order_dow_mode <- aggregate(order_product_prior$order_dow, by=list(order_product_prior$user_id), FUN=getmode)
user_order_hod_mode <- aggregate(order_product_prior$order_hour_of_day, by=list(order_product_prior$user_id), FUN=getmode)
#2-1.5 reorder situation: reorder mean by user
user_reorder_mean <- aggregate(order_product_prior$reordered, by=list(order_product_prior$user_id), FUN = mean)
#2-1.6 user shopping frenzy: maximum number of items purchased by the user in all orders
tmp <- aggregate(order_product_prior$add_to_cart_order, by=list(order_product_prior$user_id, order_product_prior$order_id), FUN=max)
tmp <- rename(tmp, user_id=Group.1, order_id=Group.2, add_to_cart_order=x)
max_purchased_item <- aggregate(tmp$add_to_cart_order, by=list(tmp$user_id), FUN = max)
#create user feature dataframe
user_fea <- data.frame(user_id,user_order_count,user_days_since_prior_order_mean,
                       user_product_count,user_product_orderid_ratio,user_reorder_mean,
                       user_order_dow_mode,user_order_hod_mode,max_purchased_item)
user_fea <- user_fea[ -c(2,4,6,8,10,12,14,16) ]
user_fea <- rename(user_fea, user_order_count=x, avg_shopping_period=x.1, user_product_count=x.2,
                   user_product_orderid_ratio=x.3,user_reorder_mean=x.4,user_order_dow_mode=x.5,
                   user_order_hod_mode=x.6,max_purchased_item=x.7)

#Step 2-2: product feature
#unique product id
product_id <- sort(unique(order_product_prior$product_id))
#2-2.1 popularity of product: number of times the products being purchased 
product_purchased_times <- aggregate(order_product_prior$user_id, by=list(order_product_prior$product_id), FUN=n_distinct)
#2-2.2 time period that the products being purchased most frequently
product_dow_mode <- aggregate(order_product_prior$order_dow, by=list(order_product_prior$product_id),FUN=getmode)#what is the most frequent day in the week the products have been ordered
product_hod_mode <- aggregate(order_product_prior$order_hour_of_day, by=list(order_product_prior$product_id),FUN=getmode)#what is the most frequent period in a day the products have been ordered
#2-2.3 reorder mean of products
product_reorder_mean <- aggregate(order_product_prior$reordered, by=list(order_product_prior$product_id), FUN=mean)
#create product feature dataframe
product_fea <- data.frame(product_id,product_purchased_times,product_dow_mode,product_hod_mode,product_reorder_mean)
product_fea <- product_fea[-c(2,4,6,8)]
product_fea <- rename(product_fea, product_purchased_times=x, product_dow_mode=x.1,product_hod_mode=x.2, product_reorder_mean=x.3)

#Step 3.1: split dataset
#because of memory issue, we only select 5% of data for training and validating
f.sample <- function(a, percent) a[sample(nrow(a), nrow(a)*percent, replace = TRUE),]
#separate orders data based on the eval_set
train_order_subset<-subset(orders,eval_set %in% c("train"))
train_order_subset <- f.sample(train_order_subset, 0.05)
prior_order_subset<-subset(orders,eval_set %in% c("prior"))
prior_order_subset <- f.sample(prior_order_subset, 0.05)
#we didn't use test dataset in our trail for testing because there is no label in the test dataset
test_order_subset<-subset(orders,eval_set %in% c("test"))
test_order_subset <- f.sample(test_order_subset, 0.05)

prior <- merge(x = prior_order, y = prior_order_subset, by='order_id', all.x = TRUE)#left join to get the prior dataset
train <- merge(x = train_order, y = train_order_subset, by='order_id', all.x = TRUE)#left join to get the train dataset

#user_product data can be considered as the historical record of `user_id` and corresponding `product_id`, and whether the product has been reordered
user_product <- data.frame(prior$user_id, prior$product_id)
user_product <- rename(user_product, user_id=prior.user_id, product_id=prior.product_id)
user_product$user_X_product <- user_product$user_id* 10**5  + user_product$product_id
user_product$label <- 0

train_user_X_product <- train$user_id* 10**5 + train$product_id
train_user <- train_order_subset$user_id
test_user <- test_order_subset$user_id

#train data is when `user_id` in the train_user list
train_data <- filter(user_product, user_product$user_id %in% train_user)
# if user_x_product is not in the train_user_x_product, label it as 1 (has been reordered before)
train_data$label[train_data$user_X_product %in% train_user_X_product] <- 1
train_data <- merge(x= train_data, y = train_order_subset, by='user_id', all.x = TRUE)
# same idea as test data, however, there is no label for test data --> kaggle restrictions; we list it here for code completeness but not for use
test_data <- filter(user_product, user_product$user_id %in% test_user)
test_data <- merge(x= test_data, y = test_order_subset, by='user_id', all.x = TRUE)

#Step 3.2: merge features and dataset
rm(tmp)#free memory
gc(reset = TRUE) 

train_data <- merge(x= train_data, y = user_fea, by='user_id', all.x = TRUE)
train_data <- merge(x= train_data, y = product_fea, by='product_id', all.x = TRUE)
train_data <- merge(x= train_data, y = products, by='product_id', all.x = TRUE)
table(train_data$label) # 0:42028 1:10772
test_data <- merge(x= test_data, y = user_fea, by='user_id', all.x = TRUE)
test_data <- merge(x= test_data, y = product_fea, by='product_id', all.x = TRUE)
test_data <- merge(x= test_data, y = products, by='product_id', all.x = TRUE)

#Step 4: Model selection and evaluation
train_feature <- train_data[-c(3,6,23,24,25)]
train_feature$label <-as.factor(train_feature$label)#change data type
#Step 4.1 split train and validation set
trainIndex <- createDataPartition(train_feature$label, 
                                  p = .7,  
                                  list = FALSE,
                                  times = 1)
train_data_final <- train_feature[trainIndex[, 1], ]  #train set 
valid_data_final <- train_feature[-trainIndex[, 1], ] #validation set
# save to csv for memory purpose
# write.csv(train_data_final, "D:\\academic\\wm\\Spring 2022\\ml2\\materials\\final_project\\instacart-market-basket-analysis\\train_data_final.csv", row.names=TRUE)
# write.csv(valid_data_final, "D:\\academic\\wm\\Spring 2022\\ml2\\materials\\final_project\\instacart-market-basket-analysis\\valid_data_final.csv", row.names=TRUE)
# not need in this case
test_feature <- test_data[-c(3,6,23,24,25)]
test_feature$label <-as.factor(test_feature$label)

#Step 4.2-1 Model Selection - SVM
star.time.svm <- Sys.time()
svm.cart <- svm(label~.-user_id-product_id-order_id, data=train_data_final, cost=2, scale=TRUE)
pred.svm <- predict(svm.cart,valid_data_final)
end.time.svm <- Sys.time()
(running.time.svm <- end.time.svm - star.time.svm)
svm.cm <- confusionMatrix(pred.svm,valid_data_final$label)
#accuracy: 80.26%
#recall rate: 5.79%

#Step 4.2-2 Model Selection - Random Forest
star.time.rf <- Sys.time()
rf.cart <- randomForest(label~.-user_id-product_id-order_id, data=train_data_final, importance=TRUE, ntree = 20, mtry=4)
pred.rf <- predict(rf.cart,valid_data_final)
end.time.rf <- Sys.time()
(running.time.rf <- end.time.rf - star.time.rf)
rf.cm <- confusionMatrix(pred.rf,valid_data_final$label)
#confusion matrix
print(rf.cart)
importance(rf.cart)
varImpPlot(rf.cart)
#prediction and calculate performance metrics
yhat <- data.frame(pred.rf)
which_product_to_reorder <- valid_data_final[c(1,2,3,4)]
which_product_to_reorder$prediction <- yhat

#accuracy: 84.71%
#recall rate for reorder: 1474/(1757+1474) = 45.6%

#Step 4.2-3 lightgbm
dtrain_matrix <- as.matrix(train_data_final[c(-3)])
dtrain_label <- as.vector(train_data_final$label)
dtrain <- lgb.Dataset(dtrain_matrix, label = dtrain_label)
dtest_matrix <- as.matrix(valid_data_final[c(-3)])
dtest_label <- as.vector(valid_data_final$label)
dtest <- lgb.Dataset.create.valid(dtrain, dtest_matrix, label = dtest_label)
valids <- list(test = dtest)
# model parameters
params = list(max_bin = 100,
              learning_rate = 0.1,
              objective = "binary",
              metric = 'binary_logloss')
# train lgbm model
start.time.lgbm <- Sys.time()
lgbm.cart <- lgb.train(
                      params = params,
                      data = dtrain,
                      nrounds = 20,
                      valids = valids
)
#print(lgbm.cart)
pred.lgbm=as.factor(round(predict(lgbm.cart,dtest_matrix)>.5))
end.time.lgbm <- Sys.time()
(running.time.lgbm <- end.time.lgbm - start.time.lgbm)
yhat <- data.frame(pred.lgbm)
lgbm.cm <- confusionMatrix(pred.lgbm, valid_data_final$label)
#accuracy: 80.28%
#recall rate for reorder: 169/(3062+169) = 5.23%

#Step 4.3 Model Comparison
#Step 4.3-1 SVM
svm.prob <- svm(label ~.-user_id-product_id-order_id, data=train_data_final, cost=2, scale=TRUE, probability=TRUE)
pred.svm.perf <- predict(svm.prob,valid_data_final, probability = TRUE)
perf.svm <- prediction(attr(pred.svm.perf, "probabilities")[,2], valid_data_final$label)
perf_auc_svm <- performance(perf.svm, "auc")
# 2. True Positive and Negative Rate
pred_tpr_fpr_svm <- performance(perf.svm, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred_tpr_fpr_svm,main="ROC Curve for SVM",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 4. Calculate AUC for SVM
roc.svm <- roc(valid_data_final$label,attr(pred.svm.perf, "probabilities")[,2])
auc(roc.svm)#0.7075

#Step 4.3-2 Random Forest
pred.rf.perf <- predict(rf.cart,valid_data_final,type='prob')
perf.rf = prediction(pred.rf.perf[,2], valid_data_final$label)
perf_auc_rf <- performance(perf.rf, "auc")
# 2. True Positive and Negative Rate
pred_tpr_fpr_rf = performance(perf.rf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred_tpr_fpr_rf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 4. Calculate AUC for RF
roc.rf <- roc(valid_data_final$label,pred.rf.perf[,2])
auc(roc.rf)#0.8227

#Step 4.3-3 LGBM
pred.lgbm.perf <- predict(lgbm.cart,dtest_matrix)
perf_lgbm = prediction(pred.lgbm.perf, valid_data_final$label)
auc.lgb = performance(perf_lgbm, "auc")
# 2. True Positive and Negative Rate
pred_tpr_fpr_lgbm = performance(perf_lgbm, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred_tpr_fpr_lgbm,main="ROC Curve for lgbm",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 4. Calculate AUC for LGBM
roc.lgbm <- roc(valid_data_final$label, pred.lgbm.perf)
auc(roc.lgbm)#0.7386

# Conclusion
# We selected three algorithms in this case: SVM, Random Forest, and lightgbm. We compared these three
# algorithms based on runtime, recall rate for reorder, and AUC.

# We first used SVM as the benchmark. However, because it is computational intense. Viewed from the result,
# the recall rate of reorder is only 5.79%, which means it has 5.79% accuracy in predicting which product will be 
# be reordered and those products have actually been reordered. Besides that, AUC of SVM is 0.7075.
# Random Forest runs fast when we only set a few ntrees, it will be computational intense when setting default ntrees(500).
# However, its recall rate is 45.6% and auc is 0.8227.
# LightGBM runs fast compared with the above two methods. However, its recall rate is only 5.23% if we set the threshold of 0.5,
# however, its AUC is 0.7386, which is ok.
# In summary, we can say in this case, Random Forest outperforms as it got the highest recall rate and AUC, with a relatively satisfying
# run time.
=======
#Problem Statement: 
# Instacart Market Analysis aimed at predicting which products will the consumer reorder, which can be
# considered as the classification problem that given the `user id` and `product id`, whether the products
# will be reordered by the user. 

#Logic flow: 
# There are four basic things that we need to do in this case:
# - EDA: Exploring the data helps us get a better understanding of the data, and helps us 
#        determine which feature we want to use / calculate to reflect the consumer behavior;
# - Feature Engineering: This step is vital. In order to analyze on which factors may lead to
#        consumer reordering behavior, we need to either extract or calculate some fields as input 
#        to help build the classifcation model;
# - Split data and merge features: While we have multiple files containing separate features, we need to do the splitting
#        and joining work before stepping into building the model; besides, since we donnot have labels
#        in the test data, we split training data into train and validation data for prediction
# - Model selection, evaluation and comparsion: After we did the previous steps, we can select the model based on
#        the features that we chose and the characteristics of the dataset; Since it is a classification
#        problem, we may need metrics to evaluate the results. In this case, we use confusion matrix.

#Step 1: load libraries and read tables
rm(list=ls())
library(dplyr)
library(data.table)
library(caret)
library(e1071)
library(ROCR)
library(randomForest)
library(lightgbm)

aisles <- fread('aisles.csv')
category <- fread('departments.csv')
orders <- fread('orders.csv')
products <- fread('products.csv')
train_order <- fread('order_products__train.csv')
prior_order <- fread('order_products__prior.csv')

#Step 1.1 Data exploration
# Since we have three dataset: `order`, `order_products__train`, and `order_products__prior`, we want to
# check the data size of last two data and count the number of each eval set in `order` data to have an idea 
# of the relationship between three datasets
length(orders$eval_set[orders$eval_set == "prior"])#3214874
dim(prior_order)#(32434489,4), not all prior data are in the orders dataset
length(orders$eval_set[orders$eval_set == "train"])#131209
dim(train_order)#(1384617,4), not all train data are in the orders dataset
#check how many products have been purchased before
mean(prior_order$reordered)#0.5896975 --> 59% of products have been purchased before
table(prior_order$add_to_cart_order)#range from 1 - 145, the smaller add to order number is, the more items it will be
#check how many product categories
length(unique(category$department_id))#21 --> difficult to analyze on whether the product category has an influence on reorder behavior, so skipping this analysis in this case

#Step 2: feature engineering
# For feature engineering, we mainly consider two factors: 
# - factors regarding user feature, that is to say, the consumer habits that may influence their reorder behavior; 
# - factors regarding product feature, that is to say, the properties of the product that may influence the reorder behavior.

#Step 2-1: user feature
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}#write function mode for aggregation
#unique user id (key in user_fea)
user_id <- sort(unique(orders$user_id))
#2-1.1 number of orders by user
user_order_count <- aggregate(orders$order_id, by=list(orders$user_id), FUN=length)
#2-1.2 shopping frequency: avg shopping period since last shopping
user_days_since_prior_order_mean <- aggregate(orders$days_since_prior_order, by=list(orders$user_id), FUN = mean, na.rm = TRUE)

order_product_prior <- merge(x = prior_order, y = orders, by='order_id', all.x = TRUE)
#2-1.3 user product preference: count the type of products made by each user
user_product_count <- aggregate(order_product_prior$product_id, by=list(order_product_prior$user_id), FUN=n_distinct)
#number of products purchased by user per time
user_product_orderid_ratio <- user_product_count / user_order_count
#2-1.4 shopping time preference: mode of user order in dow and hod
user_order_dow_mode <- aggregate(order_product_prior$order_dow, by=list(order_product_prior$user_id), FUN=getmode)
user_order_hod_mode <- aggregate(order_product_prior$order_hour_of_day, by=list(order_product_prior$user_id), FUN=getmode)
#2-1.5 reorder situation: reorder mean by user
user_reorder_mean <- aggregate(order_product_prior$reordered, by=list(order_product_prior$user_id), FUN = mean)
#2-1.6 user shopping frenzy: maximum number of items purchased by the user in all orders
tmp <- aggregate(order_product_prior$add_to_cart_order, by=list(order_product_prior$user_id, order_product_prior$order_id), FUN=max)
tmp <- rename(tmp, user_id=Group.1, order_id=Group.2, add_to_cart_order=x)
max_purchased_item <- aggregate(tmp$add_to_cart_order, by=list(tmp$user_id), FUN = max)
#create user feature dataframe
user_fea <- data.frame(user_id,user_order_count,user_days_since_prior_order_mean,
                       user_product_count,user_product_orderid_ratio,user_reorder_mean,
                       user_order_dow_mode,user_order_hod_mode,max_purchased_item)
user_fea <- user_fea[ -c(2,4,6,8,10,12,14,16) ]
user_fea <- rename(user_fea, user_order_count=x, avg_shopping_period=x.1, user_product_count=x.2,
                   user_product_orderid_ratio=x.3,user_reorder_mean=x.4,user_order_dow_mode=x.5,
                   user_order_hod_mode=x.6,max_purchased_item=x.7)

#Step 2-2: product feature
#unique product id
product_id <- sort(unique(order_product_prior$product_id))
#2-2.1 popularity of product: number of times the products being purchased 
product_purchased_times <- aggregate(order_product_prior$user_id, by=list(order_product_prior$product_id), FUN=n_distinct)
#2-2.2 time period that the products being purchased most frequently
product_dow_mode <- aggregate(order_product_prior$order_dow, by=list(order_product_prior$product_id),FUN=getmode)#what is the most frequent day in the week the products have been ordered
product_hod_mode <- aggregate(order_product_prior$order_hour_of_day, by=list(order_product_prior$product_id),FUN=getmode)#what is the most frequent period in a day the products have been ordered
#2-2.3 reorder mean of products
product_reorder_mean <- aggregate(order_product_prior$reordered, by=list(order_product_prior$product_id), FUN=mean)
#create product feature dataframe
product_fea <- data.frame(product_id,product_purchased_times,product_dow_mode,product_hod_mode,product_reorder_mean)
product_fea <- product_fea[-c(2,4,6,8)]
product_fea <- rename(product_fea, product_purchased_times=x, product_dow_mode=x.1,product_hod_mode=x.2, product_reorder_mean=x.3)

#Step 3.1: split dataset
#because of memory issue, we only select 5% of data for training and validating
f.sample <- function(a, percent) a[sample(nrow(a), nrow(a)*percent, replace = TRUE),]
#separate orders data based on the eval_set
train_order_subset<-subset(orders,eval_set %in% c("train"))
train_order_subset <- f.sample(train_order_subset, 0.05)
prior_order_subset<-subset(orders,eval_set %in% c("prior"))
prior_order_subset <- f.sample(prior_order_subset, 0.05)
#we didn't use test dataset in our trail for testing because there is no label in the test dataset
test_order_subset<-subset(orders,eval_set %in% c("test"))
test_order_subset <- f.sample(test_order_subset, 0.05)

prior <- merge(x = prior_order, y = prior_order_subset, by='order_id', all.x = TRUE)#left join to get the prior dataset
train <- merge(x = train_order, y = train_order_subset, by='order_id', all.x = TRUE)#left join to get the train dataset

#user_product data can be considered as the historical record of `user_id` and corresponding `product_id`, and whether the product has been reordered
user_product <- data.frame(prior$user_id, prior$product_id)
user_product <- rename(user_product, user_id=prior.user_id, product_id=prior.product_id)
user_product$user_X_product <- user_product$user_id* 10**5  + user_product$product_id
user_product$label <- 0

train_user_X_product <- train$user_id* 10**5 + train$product_id
train_user <- train_order_subset$user_id
test_user <- test_order_subset$user_id

#train data is when `user_id` in the train_user list
train_data <- filter(user_product, user_product$user_id %in% train_user)
# if user_x_product is not in the train_user_x_product, label it as 1 (has been reordered before)
train_data$label[train_data$user_X_product %in% train_user_X_product] <- 1
train_data <- merge(x= train_data, y = train_order_subset, by='user_id', all.x = TRUE)
# same idea as test data, however, there is no label for test data --> kaggle restrictions; we list it here for code completeness but not for use
test_data <- filter(user_product, user_product$user_id %in% test_user)
test_data <- merge(x= test_data, y = test_order_subset, by='user_id', all.x = TRUE)

#Step 3.2: merge features and dataset
rm(tmp)#free memory
gc(reset = TRUE) 

train_data <- merge(x= train_data, y = user_fea, by='user_id', all.x = TRUE)
train_data <- merge(x= train_data, y = product_fea, by='product_id', all.x = TRUE)
train_data <- merge(x= train_data, y = products, by='product_id', all.x = TRUE)

test_data <- merge(x= test_data, y = user_fea, by='user_id', all.x = TRUE)
test_data <- merge(x= test_data, y = product_fea, by='product_id', all.x = TRUE)
test_data <- merge(x= test_data, y = products, by='product_id', all.x = TRUE)

#Step 4: Model selection and evaluation
train_feature <- train_data[-c(3,6,23,24,25)]
train_feature$label <-as.factor(train_feature$label)#change data type
#Step 4.1 split train and validation set
trainIndex <- createDataPartition(train_feature$label, 
                                  p = .7,  
                                  list = FALSE,
                                  times = 1)
train_data_final <- train_feature[trainIndex[, 1], ]  #train set 
valid_data_final <- train_feature[-trainIndex[, 1], ] #validation set
# save to csv for memory purpose
# write.csv(train_data_final, "D:\\academic\\wm\\Spring 2022\\ml2\\materials\\final_project\\instacart-market-basket-analysis\\train_data_final.csv", row.names=TRUE)
# write.csv(valid_data_final, "D:\\academic\\wm\\Spring 2022\\ml2\\materials\\final_project\\instacart-market-basket-analysis\\valid_data_final.csv", row.names=TRUE)
# not need in this case
test_feature <- test_data[-c(3,6,23,24,25)]
test_feature$label <-as.factor(test_feature$label)

#Step 4.2-1 Model Selection - SVM
svm.cart <- svm(label~.-user_id-product_id-order_id, data=train_data_final, cost=2, scale=TRUE)
pred.svm <- predict(svm.cart,valid_data_final)
svm.cm <- confusionMatrix(pred.svm,valid_data_final$label)
#accuracy: 80.26%
#recall rate: 5.79%

#Step 4.2-2 Model Selection - Random Forest
rf.cart <- randomForest(label~.-user_id-product_id-order_id, data=train_data_final, importance=TRUE, ntree = 20, mtry=4)
#confusion matrix
print(rf.cart)
importance(rf.cart)
varImpPlot(rf.cart)
#prediction and calculate performance metrics
pred.rf=predict(rf.cart,valid_data_final)
yhat <- data.frame(pred.rf)
which_product_to_reorder <- valid_data_final[c(1,2,3,4)]
which_product_to_reorder$prediction <- yhat
rf.cm <- confusionMatrix(pred.rf,valid_data_final$label)
#accuracy: 84.71%
#recall rate for reorder: 1474/(1757+1474) = 45.6%

#Step 4.2-3 lightgbm
dtrain_matrix <- as.matrix(train_data_final[c(-3)])
dtrain_label <- as.vector(train_data_final$label)
dtrain <- lgb.Dataset(dtrain_matrix, label = dtrain_label)
dtest_matrix <- as.matrix(valid_data_final[c(-3)])
dtest_label <- as.vector(valid_data_final$label)
dtest <- lgb.Dataset.create.valid(dtrain, dtest_matrix, label = dtest_label)
valids <- list(test = dtest)
# model parameters
params = list(max_bin = 100,
              learning_rate = 0.1,
              objective = "binary",
              metric = 'binary_logloss')
lgbm.cart <- lgb.train(
                      params = params,
                      data = dtrain,
                      nrounds = 20,
                      valids = valids,
)
print(lgbm.cart)
pred.lgbm=as.factor(round(predict(lgbm.cart,dtest_matrix)>.5))
yhat <- data.frame(pred.lgbm)
lgbm.cm <- confusionMatrix(pred.lgbm, valid_data_final$label)
#accuracy: 80.28%
#recall rate for reorder: 169/3062+169 = 5.23%

#Step 4.3 Model Comparison
#Step 4.3-1 SVM
svm.prob <- svm(label ~.-user_id-product_id-order_id, data=train_data_final, cost=2, scale=TRUE, probability=TRUE)
pred.svm.perf <- predict(svm.prob,valid_data_final, probability = TRUE)
perf.svm <- prediction(attr(pred.svm.perf, "probabilities")[,2], valid_data_final$label)
perf_auc_svm <- performance(perf.svm, "auc")
# 2. True Positive and Negative Rate
pred_tpr_fpr_svm <- performance(perf.svm, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred_tpr_fpr_svm,main="ROC Curve for SVM",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 4. Calculate AUC for SVM
roc.svm <- roc(valid_data_final$label,attr(pred.svm.perf, "probabilities")[,2])
auc(roc.svm)#0.7075

#Step 4.3-2 Random Forest
pred.rf.perf <- predict(rf.cart,valid_data_final,type='prob')
perf.rf = prediction(pred.rf.perf[,2], valid_data_final$label)
perf_auc_rf <- performance(perf.rf, "auc")
# 2. True Positive and Negative Rate
pred_tpr_fpr_rf = performance(perf.rf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred_tpr_fpr_rf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 4. Calculate AUC for RF
roc.rf <- roc(valid_data_final$label,pred.rf.perf[,2])
auc(roc.rf)#0.8227

#Step 4.3-3 LGBM
pred.lgbm.perf <- predict(lgbm.cart,dtest_matrix)
perf_lgbm = prediction(pred.lgbm.perf, valid_data_final$label)
auc.lgb = performance(perf_lgbm, "auc")
# 2. True Positive and Negative Rate
pred_tpr_fpr_lgbm = performance(perf_lgbm, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred_tpr_fpr_lgbm,main="ROC Curve for lgbm",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 4. Calculate AUC for LGBM
roc.lgbm <- roc(valid_data_final$label, pred.lgbm.perf)
auc(roc.lgbm)#0.7386

# Conclusion
# We selected three algorithms in this case: SVM, Random Forest, and lightgbm. We compared these three
# algorithms based on runtime, recall rate for reorder, and AUC.

# We first used SVM as the benchmark. However, because it is computational intense. Viewed from the result,
# the recall rate of reorder is only 5.79%, which means it has 5.79% accuracy in predicting which product will be 
# be reordered and those products have actually been reordered. Besides that, AUC of SVM is 0.0.7075.
# Random Forest runs fast when we only set a few ntrees, it will be computational intense when setting default ntrees(500).
# However, its recall rate is 45.6% and auc is 0.8227.
# LightGBM runs fast compared with the above two methods. However, its recall rate is only 5.23% if we set the threshold of 0.5,
# however, its AUC is 0.7386, which is ok.

# In summary, we can say in this case, Random Forest outperforms as it got the highest recall rate and AUC, with a relatively satisfying
# run time.
################################################
>>>>>>> 45d3cff69b5a7610e156724c51d7ed44af4b7f54
