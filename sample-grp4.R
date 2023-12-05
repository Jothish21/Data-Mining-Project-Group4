# Install required packages if not installed
install.packages(c("dbscan",  "kmeans"))

# Load necessary libraries
library(arules)      # for association rules
library(caret)       # for classification and performance evaluation
library(dbscan)      # for clustering
library(ggplot2)     # for visualization
library(e1071)       # for SVM classification
library(kmeans)      # for k-means clustering

# Load your dataset
shopping_data <- read.csv("data/customer_shopping_data.csv")
View(shopping_data)
# Data Pre-processing
# Check for missing values
missing_values <- colSums(is.na(shopping_data))
print("Missing Values:")
print(missing_values)

# Check for duplicated rows
duplicated_rows <- shopping_data[duplicated(shopping_data), ]
print("Duplicated Rows:")
print(duplicated_rows)

# Remove duplicated rows if necessary
# Example: Remove duplicated rows
shopping_data <- unique(shopping_data)

# Convert categorical variables to factors
# Example: Convert "category" and "payment_method" to factors
shopping_data$category <- as.factor(shopping_data$category)
shopping_data$payment_method <- as.factor(shopping_data$payment_method)
unique(shopping_data$category)
# Explore the data after pre-processing
summary(shopping_data)

# Detect and handle outliers using a more robust method (Tukey's method)
# Example: Detect and remove outliers in the "quantity" column
outliers <- boxplot.stats(shopping_data$quantity)$out
print("Outliers:")
print(outliers)

shopping_data <- shopping_data[!(shopping_data$quantity %in% outliers), ]

# Association Rules
# Example: Apriori algorithm
association_rules <- apriori(shopping_data[, c("category", "payment_method")], parameter = list(support = 0.01, confidence = 0.7))
inspect(association_rules)

# Classification
# Example 1: Decision Tree
set.seed(123)
train_index <- createDataPartition(shopping_data$payment_method, p = 0.8, list = FALSE)
train_data <- shopping_data[train_index, ]
test_data <- shopping_data[-train_index, ]

print(test_data$payment_method)

test_data$payment_method <- factor(test_data$payment_method, levels = levels(train_data$payment_method))

model_decision_tree <- train(payment_method ~ age + category + quantity, data = train_data, method = "rpart")
predictions_decision_tree <- predict(model_decision_tree, newdata = test_data)
predictions_decision_tree 
# Ensure that the predicted levels match the reference levels
predictions_decision_tree <- factor(predictions_decision_tree, levels = levels(test_data$payment_method))
unique(predictions_decision_tree)
confusionMatrix(predictions_decision_tree, test_data$payment_method)

# Example 2: SVM
model_svm <- svm(price ~ age  + quantity, data = train_data)
predictions_svm <- predict(model_svm, newdata = test_data)
predictions_svm
confusionMatrix(predictions_svm, test_data$payment_method)

# Clustering
# Example 1: DBSCAN
shopping_features <- shopping_data[, c("age", "quantity", "price")]

dbscan_result <- dbscan(shopping_features, eps = 3, minPts = 1000)
table(dbscan_result$cluster)
dbscan_result
cluster_data <- data.frame(shopping_features, Cluster = dbscan_result$cluster)
ggplot(cluster_data, aes(x = quantity, y = age, color = factor(Cluster))) +
  geom_point() +
  labs(title = "DBSCAN Clustering Results",
       x = "quantity", y = "Age", color = "Cluster") 
 #


# Example 2: k-means
kmeans_result <- kmeans(shopping_features, centers = 3)
table(kmeans_result$cluster)
kmeans_result$cluster
shopping_data$cluster <- as.factor(kmeans_result$cluster)

#cluster visual

ggplot(shopping_data, aes(x = quantity, y = age)) +
  geom_point(aes(color = factor(kmeans_result$cluster))) +
  labs(title = "Scatter Plot with 3 Clusters", x = "quantity", y = "age", color = "Cluster") +
  scale_color_manual(values = c("blue", "green", "red"))  # Customize colors if needed

# Visualization
# Example: Scatter plot
ggplot(shopping_data, aes(x = age, y = quantity, color = category)) +
  geom_point() +
  labs(title = "Shopping Data Visualization", x = "Age", y = "Quantity", color = "Category")

# Performance Evaluation (for classification, in this case)
# Example: ROC curve for SVM
roc_curve_dt <- roc(test_data$payment_method, as.numeric(predictions_decision_tree))
plot(roc_curve_dt, col = "blue", main = "ROC Curve (SVM)", lwd = 2)

# Additional performance metrics for SVM
confusionMatrix(predictions_svm, test_data$payment_method)



# Install required packages if not installed
# install.packages(c("shiny", "shinydashboard", "plotly"))
q
# Load necessary libraries
library(shiny)
library(shinydashboard)
library(plotly)

# Load your dataset
shopping_data <- read.csv("your_dataset.csv")

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Shopping Data Analysis"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Association Rules",
        plotOutput("association_plot")
      ),
      box(
        title = "Decision Tree",
        plotOutput("decision_tree_plot")
      )
    ),
    fluidRow(
      box(
        title = "DBSCAN Clustering",
        plotOutput("dbscan_plot")
      ),
      box(
        title = "Scatter Plot",
        plotlyOutput("scatter_plot")
      )
    ),
    fluidRow(
      box(
        title = "ROC Curve",
        plotlyOutput("roc_curve_plot")
      ),
      box(
        title = "Confusion Matrix",
        tableOutput("confusion_matrix")
      )
    )
  )
)

# Server definition
server <- function(input, output) {
  
  # Association Rules Plot
  output$association_plot <- renderPlot({
    # Your association rules plot code here
    # Example: plot(association_rules)
  })
  
  # Decision Tree Plot
  output$decision_tree_plot <- renderPlot({
    # Your decision tree plot code here
    # Example: plot(model)
  })
  
  # DBSCAN Clustering Plot
  output$dbscan_plot <- renderPlot({
    # Your DBSCAN clustering plot code here
    # Example: plot(dbscan_result)
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlotly({
    # Your scatter plot code here
    # Example: ggplotly(ggplot(shopping_data, aes(x = age, y = quantity, color = category)) + geom_point())
  })
  
  # ROC Curve Plot
  output$roc_curve_plot <- renderPlotly({
    # Your ROC curve plot code here
    # Example: plotly::plot(roc_curve)
  })
  
  # Confusion Matrix Table
  output$confusion_matrix <- renderTable({
    # Your confusion matrix table code here
    # Example: as.data.frame(confusionMatrix(predictions, test_data$payment_method))
  })
}

# Run the Shiny app
shinyApp(ui, server)
