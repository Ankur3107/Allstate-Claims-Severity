library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

train <- read.csv("train.csv", stringsAsFactors = T)
test <- read.csv("test.csv", stringsAsFactors = T)

library(lattice)
library(Metrics)
library(Rtsne)
library(gridExtra)
library(corrplot)
library(caret)
library(ggplot2)
library(e1071)
library(GGally)
library(dyplr)
library(SparkR)

ID = 'id'
TARGET = 'loss'

train_ids <- train[ID]
test_ids <- test[ID]
y_train <- train[TARGET]
loss <- train$loss

#train[,c(ID)] <- NULL  #
#test[,c(ID)] <- NULL           #

catVars <- paste0("cat", seq(1, 116))
contVars <- paste0("cont", seq(1, 14))
targetVar <- "loss"

cor(train$loss,train[contVars[1]])
cor(train$loss,sin(train[contVars[1]])+cos(train[contVars[1]]))


cor(train$loss,train[contVars[2]])
cor(train$loss,sin(train[contVars[2]])+cos(train[contVars[2]]))

cor(train$loss,train[contVars[3]])
cor(train$loss,sin(train[contVars[3]])+cos(train[contVars[3]]))

cor(train$loss,train[contVars[4]])
cor(train$loss,sin(train[contVars[4]])+cos(train[contVars[4]]))

cor(train$loss,train[contVars[5]])
cor(train$loss,sin(train[contVars[5]])+cos(train[contVars[5]]))
