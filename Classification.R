library(caret) 
library(tidyverse)

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
