library(tidyverse)
library(ggplot2)

shopping_data <- read.csv("data/customer_shopping_data.csv")

head(shopping_data)

# so plot a circular bar chart with 2 colored bars(female, male) and category on x axis

shopping_data$gender <- factor(shopping_data$gender)
shopping_data$category <- factor(shopping_data$category)

bc_data <- shopping_data |>
  group_by(gender, category) |>
  summarise(total_quantity = sum(quantity)) |>
  ungroup()

bc_data

bc_data |>
  ggplot(aes(x = category, y = total_quantity, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), aes(label = total_quantity), vjust = -0.5, size = 3) +
  labs(title = "Total Quantity by Category (Split by Gender)",
       x = "Category",
       y = "Total Quantity") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, min(circularbc_data$total_quantity) * 11)

#--------------------------------------------------------------------------------------------

library(ggplot2)

# ... (your data loading and preprocessing code)

# Pie Chart for Category Distribution
category_distribution <- table(shopping_data$category)
category_percentages <- prop.table(category_distribution) * 100
category_data <- data.frame(Category = names(category_distribution), Count = category_distribution)

ggplot(category_data, aes(x = "", y = category_percentages , fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  geom_text(aes(label = paste0(round(category_percentages, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  scale_fill_manual(values = rainbow(length(category_distribution))) +
  labs(title = "Distribution of all category items",
       fill = "Category") +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right")

shopping_data$age_group <- cut(shopping_data$age, breaks = c(15, 25, 35, 45, 55, 65, Inf), labels = c("15-25", "26-35", "36-45", "46-55", "56-65", "66+"))

# Bar Graph for Age Group vs Payment Method
bar_data <- table(shopping_data$age_group, shopping_data$payment_method)
bar_data <- as.data.frame(bar_data)

ggplot(bar_data, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Age Group vs Payment Method",
       x = "Age Group", y = "Frequency") +
  scale_fill_manual(values = c("blue", "green", "red")) +  # Customize colors if needed
  theme_minimal()

#---------------------------------------------------------------------------
install.packages("fmsb")
library(fmsb)

male_data <- shopping_data[shopping_data$gender == "Male", "category"]
female_data <- shopping_data[shopping_data$gender == "Female", "category"]

# Create spider plots for each gender
spider_plot <- function(data, gender) {
  # Convert data to matrix format
  data_matrix <- as.data.frame(matrix(data, ncol = 1))
  
  # Set column names for the spider plot
  colnames(data_matrix) <- c("Clothing", "Shoes", "Books", "Cosmetics", "Food & Beverage", "Toys", "Technology", "Souvenir")
  
  # Create and display the spider plot
  radarchart(data_matrix, axistype = 0, pcol = ifelse(gender == "Male", "blue", "pink"), 
             plty = 1, pty = 16, pfcol = ifelse(gender == "Male", "blue", "pink"), 
             title = paste("Spider Plot - Category for", gender))
}

# Create spider plots for both genders
spider_plot(male_data, "Male")
spider_plot(female_data, "Female")
#----------------------------------------------------------------


library(fmsb)  # Ensure the 'fmsb' package is installed and loaded for 'radarchart'

# Example data structure
shopping_data <- data.frame(
  gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  category = c("Clothing", "Shoes", "Books", "Cosmetics", "Food & Beverage", "Toys", "Technology", "Souvenir")
)

# Function to create spider plots
spider_plot <- function(data, gender) {
  # Create a table with counts of each category
  category_counts <- table(data)
  
  # If there are fewer than three categories, duplicate categories to meet the minimum requirement
  if (length(category_counts) < 3) {
    category_counts <- c(category_counts, rep(0, 3 - length(category_counts)))
  }
  
  # Set names for the counts (categories)
  names(category_counts) <- c("Clothing", "Shoes", "Books", "Cosmetics", "Food & Beverage", "Toys", "Technology", "Souvenir")
  
  # Create and display the spider plot
  radarchart(matrix(category_counts), axistype = 0, pcol = ifelse(gender == "Male", "blue", "pink"),
             plty = 1, pty = 16, pfcol = ifelse(gender == "Male", "blue", "pink"),
             title = paste("Spider Plot - Category for", gender))
}

# Fetch data for Male and Female categories
male_data <- shopping_data$category[shopping_data$gender == "Male"]
female_data <- shopping_data$category[shopping_data$gender == "Female"]

# Create spider plots for both genders
spider_plot(male_data, "Male")
spider_plot(female_data, "Female")


