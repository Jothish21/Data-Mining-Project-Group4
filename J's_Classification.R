library(caret) 
library(tidyverse)

library(e1071)  # For SVM
library(caret) 

shopping_data <- read.csv("data/customer_shopping_data.csv")

shopping_data

library(dplyr)

# Encode categorical variables
shopping_data$gender <- as.factor(shopping_data$gender)
shopping_data$category <- as.factor(shopping_data$category)
shopping_data$payment_method <- as.factor(shopping_data$payment_method)
shopping_data$shopping_mall <- as.factor(shopping_data$shopping_mall)

# Split the data into training and testing sets (e.g., 70-30 split)
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(shopping_data$category, p = 0.7, 
                                  list = FALSE, times = 1)
train_data <- shopping_data[trainIndex, ]
test_data <- shopping_data[-trainIndex, ]

# Classification model for predicting 'category'
# Using a Random Forest classifier for demonstration purposes
model_category <- train(category ~ ., data = train_data, method = "rf")

# Predicting on test data
predictions_category <- predict(model_category, newdata = test_data)

# Classification model for predicting 'gender'
# Using a Decision Tree classifier for demonstration purposes
model_gender <- train(gender ~ ., data = train_data, method = "rpart")

# Predicting on test data
predictions_gender <- predict(model_gender, newdata = test_data)


#----------------------------------------------------------------------
#SVM


shopping_data <- read.csv("data/customer_shopping_data.csv")

View(shopping_data)

shopping_data$gender <- as.factor(shopping_data$gender)

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(shopping_data$gender, p = 0.7, list = FALSE)
training_data <- shopping_data[trainIndex, ]
testing_data <- shopping_data[-trainIndex, ]

svm_model <- svm(gender ~ age + category + quantity + price + payment_method + shopping_mall,
                 data = training_data,
                 kernel = "radial")

predictions <- predict(svm_model, newdata = testing_data)

# Evaluate the model (e.g., accuracy)
confusionMatrix(predictions, testing_data$gender)
#-------------------------------------------------------------

# feature selection 
selected_features <- c("category", "gender")

subset_data <- shopping_data[selected_features]
subset_data$gender <- as.factor(subset_data$gender)

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(subset_data$gender, p = 0.7, list = FALSE)
training_data <- subset_data[trainIndex, ]
testing_data <- subset_data[-trainIndex, ]

svm_model <- svm(gender ~ ., data = training_data, kernel = "radial")

predictions <- predict(svm_model, newdata = testing_data[, -ncol(testing_data)])

confusionMatrix(predictions, testing_data$gender)

#-----------------------------------------------------------
library(randomForest) #works

shopping_data$gender <- as.factor(shopping_data$gender)

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(shopping_data$gender, p = 0.7, list = FALSE)
training_data <- shopping_data[trainIndex, ]
testing_data <- shopping_data[-trainIndex, ]

rf_model <- randomForest(gender ~ ., data = training_data)

predictions_rf <- predict(rf_model, newdata = testing_data)

confusionMatrix(predictions_rf, testing_data$gender)
#---------------------------------------------------------------------

library(class)

knn_model <- knn(train = training_data[, -ncol(training_data)],
                 test = testing_data[, -ncol(testing_data)],
                 cl = training_data$gender,
                 k = 5)

#---------------------------------------------

library(e1071) #works

nb_model <- naiveBayes(gender ~ ., data = training_data)

predictions_nb <- predict(nb_model, newdata = testing_data)

confusionMatrix(predictions_nb, testing_data$gender)

typeof(predictions_nb)
typeof(testing_data$gender)
#-----------------------------------------------

library(rpart)

shopping_data <- read.csv("data/customer_shopping_data.csv")

selected_data <- shopping_data |>
  select(gender, age, category, quantity, payment_method)

new_trainIndex <- createDataPartition(selected_data$gender, p = 0.7, list = FALSE)

new_train_data <- selected_data[new_trainIndex, ]
new_test_data <- selected_data[-new_trainIndex, ]

unique(new_train_data$category)
unique(new_test_data$category)

tree_model <- rpart(gender ~ ., data = new_train_data)

predictions_tree <- predict(tree_model, newdata = new_test_data, type = "class")

new_test_data$gender <- as.integer(new_test_data$gender)
new_test_data <- new_test_data |>
  na.omit()



confusionMatrix(predictions_tree, new_test_data$gender)

typeof(new_test_data$gender)


typeof(predictions_tree)

predictions_tree

# Create a dataframe with actual and predicted labels
predictions_summary <- data.frame(Actual = new_test_data$category, Predicted = predictions_tree)

View(predictions_summary)

# Display a sample of actual vs. predicted labels
head(predictions_summary, n = 10)  # Display first 10 rows
#------------------------------------------------------------------------





















