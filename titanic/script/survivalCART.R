train = read.csv("kaggle/titanic/data/train.csv")
test = read.csv("kaggle/titanic/data/test.csv")

str(train)
summary(train)

train$Age[is.na(train$Age)] = mean(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] = mean(test$Age, na.rm = TRUE)

train = train[,!(colnames(train) %in% c("PassengerId", "Name", "Ticket", "Cabin"))]

# Cross Validation
library(caret)
library(e1071)

folds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0, 1, 0.01))

# gives cp = 0.02
set.seed(11)
train(Survived ~ ., data = train, method = "rpart", trControl = folds, tuneGrid = cpGrid)

tree = rpart(Survived ~ ., data = train, method = "class", cp = 0.02)
tree.pred = predict(tree, newdata = test, type = "class")

PassengerId = test$PassengerId
Survived = tree.pred




