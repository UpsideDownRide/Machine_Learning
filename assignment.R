## Machine learning assignment for Coursera

library(dplyr)
library(caret)
library(randomForest)
library(doParallel)
library(e1071)


training_url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
testing_url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

## Import the train and test data making sure we have correctly labaled NA in the dataset

train_src <- read.csv(training_url, na.strings=c("NA","", "#DIV/0!"), stringsAsFactors = FALSE)
test_src <- read.csv(testing_url, na.strings=c("NA","", "#DIV/0!"), stringsAsFactors = FALSE)

## Create a vector with percentage value of NA values for that variable, variables either have all the data or almost no data (below 5%)

na_columns <- sapply(train_src, function(y) sum(length(which(is.na(y))))/nrow(train_src))
column_names <- names(na_columns[na_columns==0])

## Remove columns that are lacking in data or with abundance of NAs

train_src <- select_(train_src, .dots=column_names) %>% select(-(1:2),-(5:8))
test_column_names <- train_src %>% select(-classe) %>% colnames %>% c('problem_id')
test_src <- select_(test_src, .dots=test_column_names)

## Split the training data into training and validation sets

split_data<- split_data<- createDataPartition(train_src$classe,p=0.8,list=FALSE)
train <- train_src[split_data,]
valid <- train_src[-split_data,]

registerDoParallel(cores = 4)
model_fit <- train(classe~.,data=train,method='parRF',allowParallel=TRUE,prox=TRUE)

## Save the model to cut down on computing time

saveRDS(model_fit,file='model_fit.RDS')

valid_predictions <- predict(model_fit,newdata=valid)
confusionMatrix(valid_predictions,valid$classe)

testing_predictions <- predict(model_fit,newdata=test_src)

## Generate the answer files

pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}

pml_write_files(testing_predictions)