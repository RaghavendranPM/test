setwd('/home/babuji/Documents/Dummy')
getwd()

len_df = 500
ind_start = 10001
n_order_type = 5
n_days_stage1 = 8
n_days_stage2 = 10
n_days_stage3 = 12
n_days_stage4 = 8

order_id <- c(ind_start:(ind_start + len_df - 1))
order_type <- sample(1:n_order_type, len_df, replace=T)
stage1 <- sample(1:n_days_stage1+1, len_df, replace=T)
stage2 <- sample(1:n_days_stage2+1, len_df, replace=T)
stage3 <- sample(1:n_days_stage3+1, len_df, replace=T)
stage4 <- sample(1:n_days_stage4+1, len_df, replace=T)
order_status <- rep('success', each=len_df)

df <- data.frame(order_id, order_type, stage1, stage2, stage3, stage4, order_status)

levels(df$order_status) = c('success', 'failed')

df$order_status[df$stage1 > n_days_stage1 | df$stage2 > n_days_stage2 | df$stage3 > n_days_stage3 | df$stage4 > n_days_stage4] <- 'failed'
df$stage2[df$stage1 > n_days_stage1] <- 0
df$stage3[df$stage1 > n_days_stage1 | df$stage2 > n_days_stage2] <- 0
df$stage4[df$stage1 > n_days_stage1 | df$stage2 > n_days_stage2 | df$stage3 > n_days_stage4] <- 0

str(df)
summary(df)
table(df$order_status)

# null model accuracy
max(table(df$order_status))/sum(table(df$order_status))
max(round(prop.table(table(df$order_status)) * 100, 2))
# 0.606

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(df$order_status, SplitRatio = 0.65)

# Split up the data using subset
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)

log_model = glm(order_status ~ . - order_id, data = train, family=binomial)
summary(log_model)


# train dataset accuracy
predictTrain = predict(log_model, type="response")
threshold = 0.65
table(train$order_status, predictTrain > threshold)
sum(diag(round(prop.table(table(train$order_status, predictTrain > threshold)) * 100, 2)))
# 84.31

# test dataset accuracy
predictTest = predict(log_model, type="response", newdata = test)
threshold = 0.65
table(test$order_status, predictTest > threshold)
sum(diag(round(prop.table(table(test$order_status, predictTest > threshold)) * 100, 2)))
# 86.29

# area under the curve value
library(ROCR)
ROCRpred = prediction(predictTest, ifelse((test$order_status == "success"), 0, 1))
as.numeric(performance(ROCRpred, "auc")@y.values)
# 0.7300113

# roc curve
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)


# support vector machine model
library("e1071")
svm_model <- svm(order_status ~ . - order_id, data=train)
summary(svm_model)

# train dataset accuracy
svm_predictTrain <- predict(svm_model)
sum(diag(round(prop.table(table(train$order_status, svm_predictTrain)) * 100, 2)))
# 87.69

# test dataset accuracy
svm_predictTest <- predict(svm_model, newdata = test)
sum(diag(round(prop.table(table(test$order_status, svm_predictTest)) * 100, 2)))
# 91.43

# Decision Tree model
library(rpart)
library(rpart.plot)

dt_model <- rpart(order_status ~ . - order_id, data=train, method="class")

prp(dt_model)

# train dataset accuracy
dt_predictTrain = predict(dt_model, type = "class")
sum(diag(round(prop.table(table(train$order_status, dt_predictTrain)) * 100, 2)))
# 94.77

# test dataset accuracy
dt_predictTest <- predict(svm_model, newdata = test, type = "class")
sum(diag(round(prop.table(table(test$order_status, dt_predictTest)) * 100, 2)))
# 91.43

write.csv(df, file = "sample.csv", row.names = FALSE)
