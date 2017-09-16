# Author: Jeremy Dong 

# Goal: make accurate predictions on whether or not an employee is going
# to leave

# Data Analytics Pipeline
#   Tidy Data
#   Exploratory Analysis
#   Feature Selection
#   Model Building
#   plotly + ggplot2 visualization 

-------------------------------------------------
  
library(readr)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(ggplot2)
library(plotly)

-------------------------------------------------
  
# Tidy Data
  
# create an employees df
employees <- read_csv("hr.csv", col_types = "ddiiiiiicc") %>%
  mutate(sales = factor(sales, level = c("sales","technical","support","IT",
                                         "product_mng","marketing","RandD",
                                         "accounting","hr","management")),
         salary = factor(salary, level = c("low","medium","high"))) %>%
  rename("average_monthly_hours" = average_montly_hours)

set.seed(1) 
# create training data set
inTrain <- createDataPartition(employees$left, p = 0.6, list = FALSE)
train <- employees[inTrain,]
testing <- employees[-inTrain,]

# create a test + validation data set
inTest <- createDataPartition(testing$left, p = 0.5, list = FALSE)
test <- testing[inTest,]
validation <- testing[-inTest,]



# Exploratory Analysis

# Var1 -> Satisfaction Level
# Bar Graph (4 categories - [0,0.25),[0.25,0.5),[0.5,0.75),[0.75,1.0))
train %>%
  mutate(satisfaction_level_f = cut(satisfaction_level, breaks = c(0,0.25,0.5,0.75,1),
                                    labels = c("very dissatisfied","dissatisfied",
                                               "satisfied","very satisfied"))) %>%
  ggplot(aes(x = satisfaction_level_f)) + 
  geom_bar()

# Density Plot
ggplot(train, aes(x = satisfaction_level)) + 
  geom_histogram(aes(y = ..density.. ), fill = "white", color = "black") +
  geom_density(fill = "red", alpha = 0.2) 

# Density Plots split by Sales
ggplot(train, aes(x = satisfaction_level)) + 
  geom_density(aes(fill = sales, alpha = 0.2)) + 
  facet_grid(. ~ sales)

# Density Plots split by salary
ggplot(train, aes(x = satisfaction_level)) + 
  geom_density(aes(fill = salary, alpha = 0.2)) + 
  facet_grid(. ~ salary)


# CLUSTERS OF EMPLOYEES THAT LEFT

# Cluster 1
cluster1 <- train %>%
  filter(satisfaction_level < 0.47, satisfaction_level > 0.35, 
         average_monthly_hours < 162, average_monthly_hours > 125, left == 1)

# Cluster 2
cluster2 <- train %>%
  filter(satisfaction_level < 0.93, satisfaction_level > 0.72, 
         average_monthly_hours > 216, average_monthly_hours < 276, left == 1)

# Cluster 3
cluster3 <- train %>%
  filter(satisfaction_level < 0.12, 
         average_monthly_hours < 311, average_monthly_hours > 242, left == 1)




# Var 2 -> Average Monthly Hours
# Density Plot
amh_summary <- as.numeric(summary(train$average_monthly_hours))

ggplot(train, aes(x = average_monthly_hours)) + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
  geom_density(fill = "red", alpha = 0.2) +
  geom_vline(xintercept = amh_summary[-c(1,3,6)], color = "dark blue")

# Scatterplot - AMH vs SL w/ color = number_project
ggplot(train, aes(x = average_monthly_hours, y = satisfaction_level)) +
  geom_point(aes(color = as.factor(number_project)), alpha = 0.2, size = 1) +
  geom_vline(xintercept = amh_summary[-c(1,3,6)], color = "navy", linetype = "dotdash")

# Scatterplot - AMH vs SL w/ color = left
ggplot(train, aes(x = average_monthly_hours, y = satisfaction_level)) +
  geom_point(aes(color = as.factor(left)), alpha = 0.2, size = 1) +
  geom_vline(xintercept = amh_summary[-c(1,3,6)], color = "navy", linetype = "dotdash")




# Var 3 -> number_project
ggplot(train, aes(x = number_project)) + 
  geom_bar(fill = "navy") + 
  scale_x_continuous(breaks = 2:7)

cluster1 %>%
  group_by(number_project) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = number_project, y = n, fill = as.factor(number_project))) + 
  geom_bar(stat = "identity")




# Var 4 -> time_spend_company
# Bar Graph 
ggplot(train, aes(x = time_spend_company)) + 
  geom_bar(fill = "navy") + 
  scale_x_continuous(breaks = 2:10)

# Line Graph - Time Spend Company vs avg Number Projects
train %>%
  group_by(time_spend_company) %>%
  summarize(avg_number_project = mean(number_project)) %>%
  ggplot(aes(x = time_spend_company, y = avg_number_project)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 2:10) +
  scale_y_continuous(breaks = seq(3.3,4.7,0.1))

# Bar Graph - time spend company w/ each bar as % of obs in each number_project level
train %>%
  group_by(time_spend_company, number_project) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count)) %>%
  ggplot(aes(x = time_spend_company, y = perc, fill = as.factor(number_project))) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = 2:10)

# Bar Graph - distribution of time_spend_company for each level in number_project
train %>%
  group_by(number_project, time_spend_company) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count)) %>%
  ggplot(aes(x = number_project, y = perc, fill = as.factor(time_spend_company))) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = 2:10)

# Scatterplot - AMH vs SL w/ color = time_spend_company
ggplot(train, aes(x = average_monthly_hours, y = satisfaction_level, 
                  color = as.factor(time_spend_company))) + 
  geom_point(alpha = 0.4, size = 0.6)



# Bar Graph of TSC for employees in Cluster 1
cluster1 %>%
  group_by(time_spend_company) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = time_spend_company, y = n, fill = as.factor(time_spend_company))) + 
  geom_bar(stat = "identity")

# Bar Graph of TSC for employees in Cluster 2
cluster2 %>%
  group_by(time_spend_company) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = time_spend_company, y = n, fill = as.factor(time_spend_company))) + 
  geom_bar(stat = "identity")

# Bar Graph of TSC for employees in Cluster 3
cluster3 %>%
  group_by(time_spend_company) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = time_spend_company, y = n, fill = as.factor(time_spend_company))) + 
  geom_bar(stat = "identity")




# Var 5 -> last_evaluation
# Density Plot
ggplot(train, aes(x = last_evaluation)) +
  geom_histogram(aes(y =..density..), fill = "white", color = "black") +
  geom_density(fill = "red", alpha = 0.2)

# Scatterplot
train1 <- train %>%
  mutate(last_evaluation_group = cut(last_evaluation, breaks = seq(0, 1, 0.1),
                                     labels = 1:10))

train1 %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, 
             color = last_evaluation_group)) +
  geom_point(alpha = 0.2, size = 0.6)


t1 <- cluster1 %>%
  mutate(last_evaluation_group = cut(last_evaluation, breaks = seq(0, 1, 0.1),
                                     labels = 1:10)) 
t1 %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, 
             color = last_evaluation_group)) +
  geom_point(alpha = 0.4, size = 0.6)

t2 <- cluster2 %>%
  mutate(last_evaluation_group = cut(last_evaluation, breaks = seq(0, 1, 0.1),
                                     labels = 1:10)) 

t2 %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, 
             color = last_evaluation_group)) +
  geom_point(alpha = 0.4, size = 0.6)

t3 <- cluster3 %>%
  mutate(last_evaluation_group = cut(last_evaluation, breaks = seq(0, 1, 0.1),
                                     labels = 1:10)) 

t3 %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, 
             color = last_evaluation_group)) +
  geom_point(alpha = 0.4, size = 0.6)


# Var 6 -> Work_accident
# Bar Graph
ggplot(train, aes(x = Work_accident)) + 
  geom_bar()

# Scatterplot - work accidents w/ color = left 
train %>%
  filter(Work_accident == 1) %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, color = as.factor(left))) +
  geom_point(alpha = 0.2, size = 0.6)

# Bar Graph - work accidents ~ sales
train %>%
  group_by(sales, Work_accident) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count)) %>%
  ggplot(aes(x = Work_accident, y = perc, fill = sales)) + 
  geom_bar(stat = "identity") + 
  facet_grid(sales ~ .)




# Var 7 -> promotion_last_5years
# Bar Graph
ggplot(train, aes(x = as.factor(promotion_last_5years))) + 
  geom_bar(fill = "navy")

# Scatterplot - see if AMH vs SL can predict promotions
ggplot(train, aes(x = average_monthly_hours, y = satisfaction_level, 
                  color = as.factor(promotion_last_5years))) + 
  geom_point(alpha = 0.4, size = 0.6)

# Scatterplot of people who were promoted + people working > 225 AMH
train %>%
  filter(promotion_last_5years == 1 | 
           (promotion_last_5years == 0 & average_monthly_hours > 225)) %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, 
             color = as.factor(promotion_last_5years), shape = as.factor(left))) +
  geom_point(alpha = 0.4)

promoted_left <- train %>%
  filter(left == 1) %>%
  group_by(promotion_last_5years) %>%
  summarize(n = n())

promoted_stay <- train %>%
  filter(left == 0) %>%
  group_by(promotion_last_5years) %>%
  summarize(n = n())

# Tables of Promotions - dissatisfied vs satisfied 
lower <- train %>%
  filter(satisfaction_level <= 0.5) 
upper <- train %>%
  filter(satisfaction_level > 0.5)

table(lower$promotion_last_5years) / nrow(lower)
table(upper$promotion_last_5years) / nrow(upper)

# Bar Graph - % of each department that received a promotion
train %>%
  group_by(sales, promotion_last_5years) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count)) %>%
  ggplot(aes(x = sales, y = perc, fill = as.factor(promotion_last_5years))) + 
  geom_bar(stat = "identity")




# Var 8 -> sales
# Bar Graph 
train %>%
  group_by(sales) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  mutate(sales = as.factor(sales)) %>%
  ggplot(aes(x = sales, y = n)) + 
  geom_bar(stat = "identity", fill = "navy")

# Scatterplot - AMH vs SL w/ color = sales
ggplot(train, aes(x = average_monthly_hours, y = satisfaction_level, color = sales)) + 
  geom_point(alpha = 0.2, size = 0.6)


total_by_department <- train %>%
  group_by(sales) %>%
  summarize(total = n())

# % of people from every department that left
train %>%
  filter(left == 1) %>%
  group_by(sales) %>%
  summarize(n = n()) %>%
  inner_join(total_by_department) %>%
  mutate(perc = n / total) %>%
  ggplot(aes(x = sales, y = perc, fill = sales)) + 
  geom_bar(stat = "identity")


# Bar Graph - analyzing department #'s for people with SL < 0.12
train %>%
  filter(satisfaction_level < 0.12) %>%
  group_by(sales) %>%
  summarize(n = n()) %>%
  inner_join(total_by_department) %>%
  mutate(perc = n / total) %>%
  ggplot(aes(x = sales, y = perc, fill = sales)) + 
  geom_bar(stat = "identity")

# Bar Graph - analyzing department #'s for people with SL < 0.47 & AMH < 161
train %>%
  filter(satisfaction_level < 0.47, average_monthly_hours < 161) %>%
  group_by(sales) %>%
  summarize(n = n()) %>%
  inner_join(total_by_department) %>%
  mutate(perc = n / total) %>%
  ggplot(aes(x = sales, y = perc, fill = sales)) + 
  geom_bar(stat = "identity")

# Bar Graph - analyzing department #'s for people with AMH > 245
train %>%
  filter(average_monthly_hours > 245) %>%
  group_by(sales) %>%
  summarize(n = n()) %>%
  inner_join(total_by_department) %>%
  mutate(perc = n / total) %>%
  ggplot(aes(x = sales, y = perc, fill = sales)) + 
  geom_bar(stat = "identity")




# Var 9 -> salary
# Bar Graph
ggplot(train, aes(x = salary)) + 
  geom_bar(fill = "navy")

# Bar Graph - time spend company w/ each bar as % of obs in each salary level
train %>%
  group_by(time_spend_company, salary) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count)) %>% # sum only sums up rows in same time_spend_company grouping
  ggplot(aes(x = time_spend_company, y = perc*100, fill = salary)) +
  geom_bar(stat = "identity") +
  labs(y = "%") +
  scale_x_continuous(breaks = 2:10)

# Scatterplot - see if AMH vs SL can predict salary level 
ggplot(train, aes(x = average_monthly_hours, y = satisfaction_level, color = salary)) + 
  geom_point(alpha = 0.4, size = 0.6)

# Bar Graph - % of each salary level that left
salary_count <- train %>%
  group_by(salary) %>%
  summarize(total = n())

train %>%
  filter(left == 1) %>%
  group_by(salary) %>%
  summarize(n = n()) %>%
  inner_join(salary_count) %>%
  mutate(perc = n / total) %>%
  ggplot(aes(x = salary, y = perc, fill = salary)) +
  geom_bar(stat = "identity")

# Bar Graph - salaries for people with SL < 0.12 & AMH > 245
train %>%
  filter(satisfaction_level < 0.12, average_monthly_hours > 245) %>%
  group_by(salary) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = salary, y = n, fill = salary)) + 
  geom_bar(stat = "identity")

# Bar Graph - salaries for people with SL < 0.47 & AMH < 161
train %>%
  filter(satisfaction_level < 0.47, average_monthly_hours < 161) %>%
  group_by(salary) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = salary, y = n, fill = salary)) + 
  geom_bar(stat = "identity")

-------------------------------------------------
# Why are employees from cluster 2 leaving?
#   - satisfied
#   - normal # of projects
#   - made it past 4th year

train %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, color = as.factor(left))) +
  geom_point(alpha = 0.2, size = 0.6) +
  geom_vline(xintercept = c(216, 276)) +
  geom_hline(yintercept = c(0.72, 0.93)) +
  geom_point(data = filter(train, (time_spend_company == 5 | time_spend_company == 6)), 
                           aes(x = average_monthly_hours, y = satisfaction_level), 
             color = "navy", alpha = 0.6, size = 0.6)

# Scatterplot of employees who were promoted
train %>%
  filter(time_spend_company >= 5) %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, color = as.factor(promotion_last_5years))) +
  geom_point(alpha = 0.4, size = 0.6)


train %>%
  filter(time_spend_company >= 5) %>%
  ggplot(aes(x = average_monthly_hours, y = satisfaction_level, color = as.factor(salary), shape = as.factor(left))) +
  geom_point(alpha = 0.4)


# 1 possible explanation: after working for 5-6 years, a lot of them haven't been promoted yet
#   - out of all the people >= 5 years at the company, they are the ones who work the longest
#   - maybe they were putting in the effort but seeing no reward?
-------------------------------------------------
  
# Predictive Modeling

# Baseline Accuracy - 0.763 for prediction = 0 for all obs
train_baseline <- max(table(train$left) / nrow(train))
test_baseline <- max(table(test$left) / nrow(test))


# Functions

# cm -> builds a confusion matrix with t = 0.5
cm <- function(df, pred) {
  return(as.matrix(table(df$left, as.numeric(pred >= 0.5))))
}
  

# find accuracy -> finds accuracy from a confusion matrix
find_accuracy <- function(t) {
  acc <- 0
  for (i in seq_len(2)) {
    acc = acc + t[i,i]
  }
  return(acc / sum(t))
}


# Model 1 - Logistic Regression
log <- glm(left ~ ., data = train, family = "binomial")
summary(log)

# Accuracy on Training Set - 0.788
log.predTrain <- predict(log, type = "response")
log.cmTrain <- cm(train, log.predTrain)
log.accTrain <- find_accuracy(log.cmTrain)

# Accuracy on Testing Set - 0.789
log.predTest <- predict(log, newdata = test, type = "response")
log.cmTest <- cm(test, log.predTest)
log.accTest <- find_accuracy(log.cmTest)

# Scatterplot - prob vs satisfaction level w/ color = number_project
# plot captures a lot of the observations seen during exploratory analysis
test %>%
  cbind(prob = log.predTest, pred = log.predTest >= 0.5) %>%
  ggplot(aes(x = satisfaction_level, y = prob, color = as.factor(number_project))) + 
  geom_point(alpha = 0.2, size = 1) +
  geom_hline(yintercept = 0.5)

# Quality of Log Model - AUC = 0.811
log.ROCRpred <- prediction(log.predTest, test$left)
log.ROCRperf <- performance(log.ROCRpred, "tpr", "fpr")
log.aucTest <- as.numeric(performance(log.ROCRpred, "auc") @y.values)



# Model 2 - Decision Trees
tree <- rpart(left ~ ., data = train, method = "class")

# splits are what we expected based on exploratory analysis 
# last_evaluation is a suprisingly important variable
prp(tree)

# Accuracy on Training Set - 0.968
tree.predTrain <- predict(tree, type = "prob")[,2]
tree.cmTrain <- cm(train, tree.predTrain)
tree.accTrain <- find_accuracy(tree.cmTrain)

# Accuracy on Testing Set - 0.968
tree.predTest <- predict(tree, newdata = test, type = "prob")[,2]
tree.cmTest <- cm(test, tree.predTest)
tree.accTest <- find_accuracy(tree.cmTest)

# Scatterplot - shows same clustering as Logistic Regression Scatterplot
test %>%
  cbind(prob = tree.predTest, pred = tree.predTest >= 0.5) %>%
  ggplot(aes(x = satisfaction_level, y = prob, color = as.factor(number_project))) +
  geom_point(alpha = 0.3, size = 0.6) +
  geom_hline(yintercept = 0.5) 
       
# Quality of the Model - 0.811
tree.ROCRpred <- prediction(log.predTest, test$left)
tree.ROCRperf <- performance(log.ROCRpred, "tpr", "fpr")
tree.aucTest <- as.numeric(performance(tree.ROCRpred, "auc")@y.values)



# Model 3 - Random Forests
set.seed(1)
rf <- randomForest(left ~ ., data = train, method = "class")

# Accuracy on Training Set - 0.985
rf.predTrain <- predict(rf) # gives probabilities
rf.cmTrain <- cm(train, rf.predTrain)
rf.accTrain <- find_accuracy(rf.cmTrain)

# Accuracy on Testing Set - 0.987
rf.predTest <- predict(rf, newdata = test)
rf.cmTest <- cm(test, rf.predTest)
rf.accTest <- find_accuracy(rf.cmTest)

# Scatterplot - observations remain similar to those made with decision trees + logistic regression
test %>%
  cbind(prob = rf.predTest) %>%
  ggplot(aes(x = satisfaction_level, y = prob, color = as.factor(number_project))) +
  geom_point(alpha = 0.2, size = 0.6) +
  geom_hline(yintercept = 0.5)

# Quality of the Model - 0.991
rf.ROCRpred <- prediction(rf.predTest, test$left)
rf.ROCRperf <- performance(rf.ROCRpred, "tpr", "fpr")
rf.aucTest <- as.numeric(performance(rf.ROCRpred, "auc")@y.values)



# Predictions on Validation Set

# Logistic Regression - acc (0.777) 
log.predVal <- predict(log, newdata = validation)
log.cmVal <- create_matrix(validation, log.predVal)
log.accVal <- find_accuracy(log.cmVal)


# Decision Tree - acc (0.970) 
tree.predVal <- predict(tree, newdata = validation, type = "prob")[,2]
tree.cmVal <- cm(validation, tree.predVal)
tree.accVal <- find_accuracy(tree.cmVal)


# Random Forest - acc (0.988)
rf.predVal <- predict(rf, newdata = validation)
rf.cmVal <- cm(validation, rf.predVal)
rf.accVal <- find_accuracy(rf.cmVal)


