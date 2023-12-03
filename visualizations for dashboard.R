library(tidyverse)
library(ggplot2)

shopping_data <- read.csv("data/customer_shopping_data.csv")

head(shopping_data)

# so plot a circular bar chart with 2 colored bars(female, male) and category on x axis

shopping_data$gender <- factor(shopping_data$gender)
shopping_data$category <- factor(shopping_data$category)

circularbc_data <- shopping_data |>
  group_by(gender, category) |>
  summarise(total_quantity = sum(quantity)) |>
  ungroup()

View(circularbc_data)
circularbc_data

circularbc_data |>
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



