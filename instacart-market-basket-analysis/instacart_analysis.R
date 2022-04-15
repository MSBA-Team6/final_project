#load libraries
rm(list=ls())
library(dplyr)
library(plyr)
library(data.table)
#Step 1: read data and join table
aisles <- fread('aisles.csv')
category <- fread('departments.csv')
orders <- fread('orders.csv')
products <- fread('products.csv')
train_order <- fread('order_products__train.csv')
prior_order <- fread('order_products__prior.csv')

#Count number in each dataset
length(orders$eval_set[orders$eval_set == "prior"])#3214874
dim(prior_order)#(32434489,4), not all prior data are in the orders dataset
length(orders$eval_set[orders$eval_set == "train"])#131209
dim(train_order)#(1384617,4), not all train data are in the orders dataset
#separate orders data based on the eval_set
train_order_subset<-subset(orders,eval_set %in% c("train"))
test_order_subset<-subset(orders,eval_set %in% c("test"))
prior_order_subset<-subset(orders,eval_set %in% c("prior"))
#check how many products have been purchased before
mean(prior_order$reordered)#0.5896975 --> 59% of products have been purchased before
table(prior_order$add_to_cart_order)#range from 1 - 145, the smaller add to order number is, the more items it will be

#Step 2: feature engineering

#Step 2-1: user feature
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#unique user id
unique_user_id <- sort(unique(orders$user_id))
#total shopping times by user
user_id_count <- aggregate(orders$order_id, by=list(orders$user_id), FUN=length)
#shopping frequency: avg shopping period since last shopping
user_days_since_prior_order_mean <- aggregate(orders$days_since_prior_order, by=list(orders$user_id), FUN = mean, na.rm = TRUE)
order_product_prior <- merge(x = prior_order, y = orders, by='order_id', all.x = TRUE)
#count the type of products made by each user
user_product_count <- aggregate(order_product_prior$product_id, by=list(order_product_prior$user_id), FUN=n_distinct)
#number of products purchased by each user per time
user_product_orderid_ratio <- user_product_count / user_id_count
#mode of user order
user_order_dow_mode <- aggregate(order_product_prior$order_dow, by=list(order_product_prior$user_id), FUN=getmode)
#reorder mean by user
user_reorder_mean <- aggregate(order_product_prior$reordered, by=list(order_product_prior$user_id), FUN = mean)
#create user feature dataframe
user_fea <- data.frame(unique_user_id,user_id_count,user_days_since_prior_order_mean,
                       user_product_count,user_product_orderid_ratio,user_reorder_mean,)
user_fea <- user_fea[ -c(2,4,6,8,10) ]
user_fea <- rename(user_fea, user_id_count=x, avg_shopping_period=x.1, user_product_count=x.2,
                   user_product_orderid_ratio=x.3,user_reorder_mean=x.4)

#Step 2-2: product feature
#unique product id
unique_product_id <- sort(unique(order_product_prior$product_id))
#number of times the products being purchased 
product_purchased_frequency <- aggregate(order_product_prior$user_id, by=list(order_product_prior$product_id), FUN=length)
product_dow_mode <- aggregate(order_product_prior$order_dow, by=list(order_product_prior$product_id),FUN=getmode)
product_hod_mode <- aggregate(order_product_prior$order_hour_of_day, by=list(order_product_prior$product_id),FUN=getmode)
#reorder mean of products
product_reorder_mean <- aggregate(order_product_prior$reordered, by=list(order_product_prior$product_id), FUN=mean)
#create product feature dataframe
product_fea <- data.frame(unique_product_id,product_purchased_frequency,
                          product_dow_mode,product_hod_mode,product_reorder_mean)
product_fea <- product_fea[ -c(2,4,6,8) ]
product_fea <- rename(product_fea, product_purchased_frequency=x, product_dow_mode=x.1, 
                      product_hod_mode=x.2,product_reorder_mean=x.3)



#Step 3: split dataset
prior <- merge(x = prior_order, y = prior_order_subset, by='order_id', all.x = TRUE)#left join to get the prior dataset
train <- merge(x = train_order, y = train_order_subset, by='order_id', all.x = TRUE)#left join to get the train dataset
user_product <- data.frame(prior$user_id, prior$product_id,user_x_product)
user_product <- rename(user_product, user_id=prior.user_id, product_id=prior.product_id)

train_user <- train_order_subset$user_id
test_user <- test_order_subset$user_id
user_product$label <- 0
train_data <- filter(user_product, user_product$user_id %in% train_user)
train_data$label[train_data$user_x_product %in% train_user_x_product] <- 1

train_data <- merge(x= train_data, y = train_order_subset, by='user_id', all.x = TRUE)
test_data <- filter(user_product, user_product$user_id %in% test_user)
test_data <- merge(x= test_data, y = test_order_subset, by='user_id', all.x = TRUE)

#Step 4: merge features and dataset