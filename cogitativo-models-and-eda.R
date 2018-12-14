##### Author: Naveed Sharif
##### Data Source: Cogitativo
##### Topic: Data Science Challenge
##### Due Date: 10/28/2018

### set working directory
setwd("C:/Users/Naveed/Desktop/Employment/Employment Applications/Cogitativo")

### Install and load dependent packages
install.packages("tidyverse")
install.packages("dummies")
library(tidyverse)
library(dummies)

### entropy function used to evaluate the cardinality of categorical variables 
# Using Shannons Entropy. 0 in this context means very low cardinality
entropy = function(cat.vect){
  px  = table(cat.vect)/length(cat.vect)
  lpx = log(px, base=2)
  ent = -sum(px*lpx)
  return(ent)
} 

### load dataset into enviornment
df_main <- read.csv("claim.sample.csv", stringsAsFactors = FALSE, na.strings=c(" ","NA"))

### examine dataset
glimpse(df_main)
summary(df_main)
mean(is.na(df_main))

### lowercase all column names
for( i in colnames(df_main)){
  colnames(df_main)[which(colnames(df_main)==i)] = tolower(i)
}

### remove column 'v1'. Do not need a row number index.
df_main <- within(df_main, remove(v1))

### Q1_A: Find the number of claim lines that have J-codes.   
Q1_A <- df_main %>%
  filter(substr(procedure.code,1,1) == "J") %>%
  summarize(n.jcodes = n())

### Q1_B: How much was paid for J-codes to providers for 'in network' claims?
Q1_B <- df_main %>%
  filter((substr(procedure.code,1,1) == "J") &
           in.out.of.network == "I") %>%
  summarise(sum(provider.payment.amount))

### Q1_C: What are the top five J-codes based on the payment to providers?
Q1_C <- df_main %>%
  group_by(procedure.code) %>%
  filter((substr(procedure.code,1,1) == "J")) %>%
  summarise(payment.to.provider = sum(provider.payment.amount)) %>%
  arrange(desc(payment.to.provider)) %>%
  head(5)

### Q2: Determine the number of providers that were paid for at least one J-code
Q2 <- df_main %>%
  group_by(provider.id) %>%
  filter((substr(procedure.code,1,1) == "J") & 
           (provider.payment.amount > 0)) %>%
  summarise(payment.to.provider = sum(provider.payment.amount))

nrow(Q2)

### Q2_A: Create a scatter plot that displays the number of unpaid claims 
### (lines where the 'Provider.Payment.Amount' field is equal to zero) for each 
### provider versus the number of paid claims. 
### Use the J-code claims for these providers to complete the following exercises.
Q2_A <- df_main %>%
  group_by(provider.id) %>%
  filter((substr(procedure.code,1,1) == "J")) %>%
  summarise(total.unpiad.claims = sum(provider.payment.amount == 0)
            , total.piad.claims = sum(provider.payment.amount != 0))

Q2_A %>%
ggplot(aes(x = total.piad.claims, y = total.unpiad.claims)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  ggtitle("Chart 1: Number of Unpaid Claims vs Paid Claims by Provider") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Total Number of Paid Claims") + ylab("Total Number of Unpaid Claims") 

### Q2_B: What insights can you suggest from the graph? 
Q2_B_model <- lm(total.unpiad.claims~total.piad.claims,data = Q2_A)
summary(Q2_B_model)

### Q2_C: Based on the graph, is the behavior of any of the providers concerning? 
### Explain.
fitted <- round(Q2_B_model$fitted.values,0)
actuals_over_fitted <- round(Q2_A$total.unpiad.claims/fitted,2)

Q2_C <- cbind(Q2_A, fitted, actuals_over_fitted)

Q2_C_outliers <- Q2_C %>%
  filter(actuals_over_fitted >= 2)

### Q3: Consider all claim lines with a J-code.
Q3 <- df_main %>%
  filter((substr(procedure.code,1,1) == "J"))

### Q3_A: What percentage of J-code claim lines were unpaid?
Q3_A <- df_main %>%
  filter((substr(procedure.code,1,1) == "J")) %>%
  summarise(total.unpiad.claims = sum(provider.payment.amount == 0)
            , total.piad.claims = sum(provider.payment.amount != 0)) %>%
  mutate(pct.unpaid.claims = total.unpiad.claims / sum(total.unpiad.claims,total.piad.claims))

### Q3_B: Create a model to predict when a J-code is unpaid.
### Explain why you choose the modeling approach.
### Install and load neccessary packages
# install.packages("lattice")
install.packages("caret")
install.packages("e1071")
install.packages("ranger")
install.packages("ipred")
install.packages("caTools")
install.packages("xgboost")
install.packages("gbm")
install.packages("Metrics")
library(Metrics)
library(lattice)
library(caret)
library(e1071)
library(ranger)
library(ipred)
library(caTools)
library(xgboost)
library(gbm)

### Feature engineering
df_main <- df_main %>%
  group_by(claim.number) %>%
  arrange(claim.number,claim.line.number) %>%
  mutate(total.count.claim.line = n()) %>% # total count of claim line per claim number
  mutate(total.rank.claim.charge = row_number(claim.charge.amount)) # rank claim amount by claim number

df_main <- df_main %>%
  group_by(group.index) %>%
  arrange(group.index,member.id) %>%
  mutate(count.members.in.group = n_distinct(member.id)) # total members per group

Q3_B_df <- df_main %>%
  filter((substr(procedure.code,1,1) == "J")) %>% # filter for only J procedure code 
  group_by(claim.number) %>%
  arrange(claim.number,claim.line.number) %>%
  mutate(total.count.jclaim.line = n()) %>% # total count of claim line per j claim number
  mutate(rank.jclaim.charge = row_number(claim.charge.amount))  %>% # rank claim amount by j claim number
  mutate(jclaim.line.number = row_number(claim.line.number))  # rank j claim line by j claim number

### Since this is a classification problem. let's first create a dependent variable
sum(is.na(Q3_B_df$provider.payment.amount)) # no missing values

Q3_B_df$unpaid.jclaim <- ifelse(Q3_B_df$provider.payment.amount == 0, 1, 0)
table(Q3_B_df$unpaid.jclaim) # numbers tie to Q3_A

### evaluate the claims charge amount
### there should be no negative claim charges. unless its a refund. 
### i will therfore remove the observations (since its a very small subset, n=60)
Q3_B_df <- Q3_B_df %>%
  filter(claim.charge.amount > 0)

### the distribution of claims charge amount
hist(Q3_B_df$claim.charge.amount) # is skewed
hist(log(Q3_B_df$claim.charge.amount)) # much better
Q3_B_df$log.claim.charge.amount <- log(Q3_B_df$claim.charge.amount)

### Review features that will be used. 
### Evaluate distribution, missing values, entropy, and create dummy variables
### i will store them in seperate df's and evaluate the lift i get when included in the model
glimpse(Q3_B_df)
mean(is.na(Q3_B_df))

length(unique(Q3_B_df$provider.id))
mean(is.na(Q3_B_df$provider.id))
entropy(Q3_B_df$provider.id)
provider_id <- dummy(Q3_B_df$provider.id, data = NULL, sep = ".")

length(unique(Q3_B_df$line.of.business.id))
mean(is.na(Q3_B_df$line.of.business.id)) # ~37% null
entropy(Q3_B_df$line.of.business.id)
lob <- dummy(Q3_B_df$line.of.business.id, data = NULL, sep = ".")

length(unique(Q3_B_df$revenue.code))
mean(is.na(Q3_B_df$revenue.code))
entropy(Q3_B_df$revenue.code)
revenue_code <- dummy(Q3_B_df$revenue.code, data = NULL, sep = ".")

length(unique(Q3_B_df$service.code))
mean(is.na(Q3_B_df$service.code))
entropy(Q3_B_df$service.code)
service_code <- dummy(Q3_B_df$service.code, data = NULL, sep = ".")

length(unique(Q3_B_df$place.of.service.code))
mean(is.na(Q3_B_df$place.of.service.code)) # practically all null 
entropy(Q3_B_df$place.of.service.code) # practically no variability stored

length(unique(Q3_B_df$diagnosis.code))
mean(is.na(Q3_B_df$diagnosis.code))
entropy(Q3_B_df$diagnosis.code) # the entropy is large. might be to noisy to include in model
diagnosis_code <- dummy(Q3_B_df$diagnosis.code, data = NULL, sep = ".")

length(unique(Q3_B_df$denial.reason.code))
mean(is.na(Q3_B_df$denial.reason.code)) # ~80% null 
entropy(Q3_B_df$denial.reason.code)

length(unique(Q3_B_df$price.index))
mean(is.na(Q3_B_df$price.index)) # ~40% null
entropy(Q3_B_df$price.index)
price_index <- dummy(Q3_B_df$price.index, data = NULL, sep = ".")

length(unique(Q3_B_df$in.out.of.network))
mean(is.na(Q3_B_df$in.out.of.network)) # ~37% null
entropy(Q3_B_df$in.out.of.network)
in_out_of_network <- dummy(Q3_B_df$in.out.of.network, data = NULL, sep = ".")

length(unique(Q3_B_df$reference.index))
mean(is.na(Q3_B_df$reference.index)) # ~37% null
entropy(Q3_B_df$reference.index)
reference_index <- dummy(Q3_B_df$reference.index, data = NULL, sep = ".")

length(unique(Q3_B_df$pricing.index))
mean(is.na(Q3_B_df$pricing.index)) # ~37% null
entropy(Q3_B_df$pricing.index)
pricing_index <- dummy(Q3_B_df$pricing.index, data = NULL, sep = ".")

length(unique(Q3_B_df$capitation.index))
mean(is.na(Q3_B_df$capitation.index)) # 63% null
entropy(Q3_B_df$capitation.index)
capitation_index <- dummy(Q3_B_df$capitation.index, data = NULL, sep = ".")

length(unique(Q3_B_df$group.index))
mean(is.na(Q3_B_df$group.index))
entropy(Q3_B_df$group.index)
group_index <- dummy(Q3_B_df$group.index, data = NULL, sep = ".")

length(unique(Q3_B_df$claim.type))
mean(is.na(Q3_B_df$claim.type))
entropy(Q3_B_df$claim.type) # low entropy
claim_type <- dummy(Q3_B_df$claim.type, data = NULL, sep = ".")

length(unique(Q3_B_df$claim.subscriber.type))
mean(is.na(Q3_B_df$claim.subscriber.type))
entropy(Q3_B_df$claim.subscriber.type) # very low entropy. will not use in model

length(unique(Q3_B_df$claim.pre.prince.index))
mean(is.na(Q3_B_df$claim.pre.prince.index)) # 78% null
entropy(Q3_B_df$claim.pre.prince.index) # not much variance. will not use in the model

length(unique(Q3_B_df$claim.current.status))
mean(is.na(Q3_B_df$claim.current.status))
entropy(Q3_B_df$claim.current.status)
claim_current_status <- dummy(Q3_B_df$claim.current.status, data = NULL, sep = ".")

length(unique(Q3_B_df$network.id))
mean(is.na(Q3_B_df$network.id)) # 37% null
entropy(Q3_B_df$network.id)
network_id <- dummy(Q3_B_df$network.id, data = NULL, sep = ".")

length(unique(Q3_B_df$agreement.id))
mean(is.na(Q3_B_df$agreement.id)) # 37% null
entropy(Q3_B_df$agreement.id)
agreement_id <- dummy(Q3_B_df$agreement.id, data = NULL, sep = ".")

### First two models will be based on an ensemble method (bagging vs boosted)
### In these iterations, I will only include feauture engineered variables from above
Q3_B_df2 <- Q3_B_df %>%
  select(claim.number
         , unpaid.jclaim
         , claim.line.number
         , total.count.claim.line
         , total.rank.claim.charge
         , count.members.in.group
         , total.count.jclaim.line
         , rank.jclaim.charge
         , jclaim.line.number
         , log.claim.charge.amount)

glimpse(Q3_B_df2)

### Shuffle row indices & randomly order data
set.seed(987)
rows <- sample(nrow(Q3_B_df2))
Q3_B_df2 <- Q3_B_df2[rows, ]

claim_number <- Q3_B_df2$claim.number # storing claim number in seperate df
Q3_B_df2 <- within(Q3_B_df2, remove(claim.number))

### Create traning and test split
split <- nrow(Q3_B_df2) * .80
train <- Q3_B_df2[1:split, ]
test <- Q3_B_df2[(split + 1):nrow(Q3_B_df2), ]

### Develop bagging model
set.seed(1234)

model_1_bagging <- bagging(formula =  unpaid.jclaim ~ .,
                        data = train,
                        coob = TRUE)

print(model_1_bagging)

model_1_bagging_predictions <- predict(object = model_1_bagging, 
                             newdata = test, 
                             type = "class") 

### ROC curve
colAUC(model_1_bagging_predictions, (test$unpaid.jclaim), plotROC = TRUE)

model_1_bagging_predictions <- as.factor(ifelse(model_1_bagging_predictions >= .70, 1, 0)) ### optimum threshold

### Confusion matrix and AUC metric
confusionMatrix(data = model_1_bagging_predictions,	        # predicted classes
                reference = as.factor(test$unpaid.jclaim))	# actual classes

auc(as.factor(test$unpaid.jclaim), model_1_bagging_predictions) # AUC metric

### Althogh bagging algorithim had a high accuracy, the sensitivity rate was low (due to unbalanced dataset)
### Develop gradient boosted model
set.seed(123)
model_1_boosting <- gbm(formula = unpaid.jclaim ~ .,
             distribution = "bernoulli", 
             data = train,
             n.trees = 1000)

summary(model_1_boosting) # feature importance

model_1_boosting_predictions <- predict(object = model_1_boosting,
                             newdata = test,
                             type = "response",
                             n.trees = 1000)

### ROC curve
colAUC(model_1_boosting_predictions, (test$unpaid.jclaim), plotROC = TRUE)

model_1_boosting_predictions <- as.factor(ifelse(model_1_boosting_predictions >= .75, 1, 0)) ### optimum threshold

### Confusion matrix and AUC metric
confusionMatrix(data = model_1_boosting_predictions,	        # predicted classes
                reference = as.factor(test$unpaid.jclaim))	# actual classes

auc(as.factor(test$unpaid.jclaim), model_1_boosting_predictions) # AUC metric

### I will now use a more complex boosting model: Extreme Gradient Boosted Decision Tree
###  partition the datasets by trianing, validation, and test 
split_1 <- round(nrow(Q3_B_df2)*.80) # for training and validation set
split_2 <- round(split_1*.90) # for test set

train <- Q3_B_df2[1:split_2, ]
nrow(train)

validation <- Q3_B_df2[(split_2+1):split_1, ]
nrow(validation)

test <- Q3_B_df2[(split_1+1):nrow(Q3_B_df2), ]
nrow(test)

### Validate the proportion of the dependent variable samples across 
### training, validation, and test datasets
prop.table(table(train$unpaid.jclaim)) 
prop.table(table(validation$unpaid.jclaim)) 
prop.table(table(test$unpaid.jclaim)) 

### create a matrix for the training, validation, and test datasets. 
trainm_1 <- within(train, remove(unpaid.jclaim))
train_label_1 <- train$unpaid.jclaim # store label seperately
train_matrix_1 <- xgb.DMatrix(data = as.matrix(trainm_1), label = train_label_1)

validationm_1 <- within(validation, remove(unpaid.jclaim))
validation_label_1 <- validation$unpaid.jclaim  # store label seperately
validaiton_matrix_1 <- xgb.DMatrix(data = as.matrix(validationm_1), label = validation_label_1)

testm_1 <- within(test, remove(unpaid.jclaim))
test_label_1 <- test$unpaid.jclaim  # store label seperately
test_matrix_1 <- xgb.DMatrix(data = as.matrix(testm_1), label = test_label_1)

### Define the parameters for the algorithim
nc <- length(unique(train_label_1))

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)

watchlist <- list(train = train_matrix_1, test = validaiton_matrix_1)

### Create the Extreme Gradient Boosting model
set.seed(12)

model_1_xboosted <- xgb.train(params = xgb_params,
                         data = train_matrix_1,
                         nrounds = 1000,
                         watchlist = watchlist,
                         max.depth = 10,
                         gamma = 1, 
                         eta = 0.01) 

model_1_xboosted

### training & validaiton error plot
e1 <- data.frame(model_1_xboosted$evaluation_log)
plot(e1$iter, e1$train_mlogloss, col = 'dark green'
     , main = "Chart 2: Early Stopping Plot"
     , xlab="Capacity", ylab="Error")
lines(e1$iter, e1$test_mlogloss, col = 'orange')
legend("topright", inset=.05, title="Legend",
       c("Training Error","Validation Error"), fill=terrain.colors(3), horiz=TRUE)

min(e1$test_mlogloss)
e1[e1$test_mlogloss == 0.188671,]

imp1 <- xgb.importance(colnames(train_matrix_1), model = model_1_xboosted) # feature importance
print(imp1) # print feature importance

model_1_xboosted_predictions <- predict(model_1_xboosted, newdata = test_matrix_1)

### prediciton and confusion matrix. 
head(model_1_xboosted_predictions)
pred2 <- matrix(model_1_xboosted_predictions, nrow = nc, ncol = length(model_1_xboosted_predictions)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(test_actual_label = test_label_1, predicted_label = ifelse(X2 >= .85, 1, 0)) # optimum threshold

### ROC curve
colAUC(pred2$X2, (pred2$test_actual_label), plotROC = TRUE)

### Confusion matrix and AUC metric
confusionMatrix(data = as.factor(pred2$predicted_label),	# predicted classes
                reference = as.factor(pred2$test_actual_label))	# actual classes

auc(pred2$test_actual_label, pred2$predicted_label) # AUC metric
