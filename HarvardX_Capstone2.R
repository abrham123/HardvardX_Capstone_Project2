##Install Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") 
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") 
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org") 
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org") 
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org") 
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org") 
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org") 

##Download the dataset
data<- read.csv("https://raw.githubusercontent.com/IoannisDim/HarvardX-Data-Science-Capstone-part2/master/adult.csv")

##Data exploration
###Dimensions
dim(data)
###Structure
str(data)
#First 6 Observations
head(data)

##Data Cleaning
data<- data%>% filter(!workclass=="?", !occupation=="?", !native.country=="?")
dim(data)

##Summary of the data
summary(data)

##Remove unnecessary variables
data<- data%>% select(-c(education, fnlwgt))

##Create Train and Validation sets
set.seed(1,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(1) 
test_index <- createDataPartition(data$income, times = 1, p = 0.25, list = FALSE) 
validation<- data[test_index, ] 
train_set<- data[-test_index, ] 


##Data Visualization
###Áge
train_set%>% ggplot(aes(age)) + 
  geom_histogram(aes(fill=income),color='blue',binwidth=1) +   
  labs(title= "Age Distribution per Income")+
  theme(plot.title = element_text(hjust = 0.5))
###Education.num
train_set%>% ggplot(aes(education.num))+
  geom_histogram(aes(fill=income), color='blue', binwidth = 1)+
  labs(title = "Education Number Distribution for each income")+
  theme(plot.title = element_text(hjust = 0.5))
###Marital.status
train_set%>% ggplot(aes(marital.status))+
  geom_histogram(aes(fill=income),stat = "count", color='blue')+
  labs(title = "Marital Status Distribution for each income")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###Occupation
qplot(income,data = train_set, fill=occupation)+ facet_grid(.~occupation)+
  labs(title = "Income Distribution for each occupation")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###Sex
train_set%>% ggplot(aes(sex))+
  geom_bar(aes(fill=income), stat = "count")+
  labs(title = "Sex distribution for each income")+
  theme(plot.title = element_text(hjust = 0.5))

###Race
train_set %>% ggplot(aes(race))+
  geom_histogram(aes(fill=income),stat="count")+
  labs(title = "Race distribution for each income")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Machine Learning Models
##Split the Train set to run models more efficiently 
set.seed(10,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(10) 
test_split_index <- createDataPartition(train_set$income, times = 1, p = 0.2, list = FALSE) 
testing <- train_set[test_split_index, ] 
training <- train_set[-test_split_index, ] 

###Knn (K nearest neighbors) Model
#Using a 10 fold cross-validation  
set.seed(9,sample.kind = "Rounding")  
control <- trainControl(method = "cv", number = 10, p = .9) 
train_knn <- train(income ~ .,  
                   method = "knn",  
                   data = training,  
                   tuneGrid = data.frame(k = seq(5,33,2)),  
                   trControl = control) 
#See the best k value 
train_knn$bestTune 
#Compute the accuracy of the knn model on the validation dataset 
knn_accuracy <- confusionMatrix(predict(train_knn, testing, type = "raw"),  
                                testing$income)$overall["Accuracy"] 
#Create a table to save our results for each model 
accuracy_results <- tibble(method = "knn", Accuracy = knn_accuracy) 
#View the knn accuracy results in our table 
accuracy_results %>% knitr::kable() 

### Classification Tree Model
#Train a Classification Tree model 
set.seed(300,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(300) 
train_rpart <- train(income ~ ., 
                     method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.01, len=100)), 
                     data = training) 
#See the best cp value 
train_rpart$bestTune 
#Compute the accuracy the Classification Tree model on the testing dataset
rpart_accuracy <- confusionMatrix(predict(train_rpart, testing), 
                                  testing$income)$overall["Accuracy"] 

#Save the Classification Tree model accuracy results to our table 
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method="rpart", Accuracy = rpart_accuracy)) 
#View the rpart accuracy results in our table 
accuracy_results %>% knitr::kable() 

###Random Forest Forest
set.seed(3,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(3) 
train_rf <- randomForest(income ~ ., data = training) 
#Compute the accuracy of the random forest model on the testing dataset 
rf_accuracy <- confusionMatrix(predict(train_rf, testing), 
                               testing$income)$overall["Accuracy"] 
#Save the random forest accuracy results to our table 
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method="random forest", Accuracy = rf_accuracy)) 
#View the random forest accuracy results in our table 
accuracy_results %>% knitr::kable() 

##Final Random Forest model
set.seed(3, sample.kind = "Rounding") #if using R3.5 or earlier set.seed(3) 
final_train_rf <- randomForest(income ~ ., data = train_set) 

#Compute the accuracy of our final random forest model on the validation set 
final_accuracy <- confusionMatrix(predict(final_train_rf, 
                                          validation), 
                                  validation$income)$overall["Accuracy"] 

#Save the random forest accuracy results to our table. 
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method="Final Random Forest Model", 
                                     Accuracy = final_accuracy)) 
#View the final random forest model accuracy results in our table 
accuracy_results %>% knitr::kable() 


#Results 
accuracy_results %>% knitr::kable()



