library(caret) 
library(tidyverse)
library(randomForest)
library(e1071)
library(dplyr)


shopping_data <- read.csv("data/customer_shopping_data.csv")

shopping_data



# Encode categorical variables
shopping_data$gender <- as.factor(shopping_data$gender)
shopping_data$category <- as.factor(shopping_data$category)
shopping_data$payment_method <- as.factor(shopping_data$payment_method)
shopping_data$shopping_mall <- as.factor(shopping_data$shopping_mall)

#----------------------------------------------------------------------
 #works

shopping_data$gender <- as.factor(shopping_data$gender)

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(shopping_data$gender, p = 0.7, list = FALSE)
training_data <- shopping_data[trainIndex, ]
testing_data <- shopping_data[-trainIndex, ]

rf_model <- randomForest(gender ~ ., data = training_data)

predictions_rf <- predict(rf_model, newdata = testing_data)
predictions_rf 
conf_matrix1 <- confusionMatrix(predictions_rf, testing_data$gender)


print(conf_matrix1)

# Plot Confusion Matrix
plot(conf_matrix1$table, 
     col = conf_matrix1$byClass,
     main = paste("Confusion Matrix - Accuracy:", round(conf_matrix1$overall["Accuracy"], 3)))

#---------------------------------------------

library(e1071) #works

nb_model <- naiveBayes(gender ~ ., data = training_data)

predictions_nb <- predict(nb_model, newdata = testing_data)

conf_matrix <- confusionMatrix(predictions_nb, testing_data$gender)

# Print Confusion Matrix
print(conf_matrix)

# Plot Confusion Matrix
plot(conf_matrix$table, 
     col = conf_matrix$byClass,
     main = paste("Confusion Matrix - Accuracy:", round(conf_matrix$overall["Accuracy"], 3)))




















