
# Install required package if not installed
# install.packages("e1071")

# Load necessary libraries
library(arules)      # for association rules
library(caret)       # for classification and performance evaluation
library(dbscan)      # for clustering
library(ggplot2)     # for visualization
library(e1071)       # for SVM classification
library(kmeans)       # for k-means clustering

# Load your dataset
shopping_data <- read.csv("your_dataset.csv")

# Explore the data
summary(shopping_data)

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

model_decision_tree <- train(payment_method ~ age + category + quantity, data = train_data, method = "rpart")
predictions_decision_tree <- predict(model_decision_tree, newdata = test_data)
confusionMatrix(predictions_decision_tree, test_data$payment_method)

# Example 2: SVM
model_svm <- svm(payment_method ~ age + category + quantity, data = train_data)
predictions_svm <- predict(model_svm, newdata = test_data)
confusionMatrix(predictions_svm, test_data$payment_method)

# Clustering
# Example 1: DBSCAN
shopping_features <- shopping_data[, c("age", "quantity", "price")]

dbscan_result <- dbscan(shopping_features, eps = 3, minPts = 5)
table(dbscan_result$cluster)

# Example 2: k-means
kmeans_result <- kmeans(shopping_features, centers = 3)
table(kmeans_result$cluster)

# Visualization
# Example: Scatter plot
ggplot(shopping_data, aes(x = age, y = quantity, color = category)) +
  geom_point() +
  labs(title = "Shopping Data Visualization", x = "Age", y = "Quantity", color = "Category")

# Performance Evaluation (for classification, in this case)
# Example: ROC curve for SVM
roc_curve_svm <- roc(test_data$payment_method, as.numeric(predictions_svm))
plot(roc_curve_svm, col = "blue", main = "ROC Curve (SVM)", lwd = 2)

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
