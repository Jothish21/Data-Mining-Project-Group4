#install.package("pacman")

library(pacman)


pacman::p_load(
  tidyverse,
  dplyr,
  here,
  shiny,
  ggplot2,
  caret
)

shopping_data <- read.csv("data/customer_shopping_data.csv")

View(shopping_data)


# to check if there are any NA's in the dataset 
unique(is.na(shopping_data))

# Data-Cleaning

#Check missing values
missing_values <- colSums(is.na(shopping_data))
missing_values

# Check for duplicated rows
duplicated_rows <- shopping_data[duplicated(shopping_data), ]
duplicates_rows

# Convert categorical variables to factors
shopping_data$gender <- as.factor(shopping_data$gender)
shopping_data$category <- as.factor(shopping_data$category)
shopping_data$payment_method <- as.factor(shopping_data$payment_method)
shopping_data$shopping_mall <- as.factor(shopping_data$shopping_mall)

# Detect and handle outliers using a more robust method (Tukey's method)
outliers <- boxplot.stats(shopping_data$quantity)$out
outliers

# Association





# Classification







# Clustering


# Visualization





# performance evaluation



# miscellaneous

