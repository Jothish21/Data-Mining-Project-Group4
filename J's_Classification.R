library(caret) 
library(tidyverse)
library(randomForest)
library(e1071)
library(dplyr)
library(pROC)

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
precision <- conf_matrix1$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Plot Confusion Matrix
plot(conf_matrix1$table, 
     col = conf_matrix1$byClass,
     main = paste("Confusion Matrix - Accuracy:", round(conf_matrix1$overall["Accuracy"], 3)))

roc_curve <- roc(testing_data$gender, as.numeric(predictions_rf == "Male"))
roc_curve 
plot(roc_curve, main = "ROC Curve", col = "blue")

# You can also calculate the AUC (Area Under the Curve)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")
#---------------------------------------------

library(e1071) #works

nb_model <- naiveBayes(gender ~ ., data = training_data)

predictions_nb <- predict(nb_model, newdata = testing_data)

conf_matrix <- confusionMatrix(predictions_nb, testing_data$gender)

# Print Confusion Matrix
print(conf_matrix)

precision1 <- conf_matrix$byClass["Pos Pred Value"]
recall1 <- conf_matrix$byClass["Sensitivity"]
f1_score1 <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision1, "\n")
cat("Recall:", recall1, "\n")
cat("F1 Score:", f1_score1, "\n")
# Plot Confusion Matrix
plot(conf_matrix$table, 
     col = conf_matrix$byClass,
     main = paste("Confusion Matrix - Accuracy:", round(conf_matrix$overall["Accuracy"], 3)))




















