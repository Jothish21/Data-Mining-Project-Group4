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

# Creating transactions & finding the maximal sets
transactions <- as(shopping_data, "transactions")

sets <- apriori(transactions, parameter = list(support = 0.05, target = "frequent itemsets"))
inspect(sets)

maximal = sets[is.maximal(sets)]
summary(maximal)
inspect(head(maximal, n = 5, by = "support"))

maximal

# Making a maximal set with main focus on Category variable
maximal_cat_cloth <- subset(maximal, subset=(items %in% "category=Clothing"))
maxi_cloth_df <- as(maximal_cat_cloth, "data.frame")

maxi_cloth_df

# Making a maximal set with main focus on Female value from Gender variable
maximal_gen_female <- subset(maximal, subset=(items %in% "gender=Female"))
gen_female_df <- as(maximal_gen_female, "data.frame")

gen_female_df








# Classification







# Clustering


# Visualization





# performance evaluation



# miscellaneous

