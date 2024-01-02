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

glimpse(shopping_data)


# to check if there are any NA's in the dataset 
unique(is.na(shopping_data))

## Data-Cleaning

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

## Data Exploration
# Total quantity purchase of all category by gender

bc_data <- shopping_data |>
  group_by(gender, category) |>
  summarise(total_quantity = sum(quantity)) |>
  ungroup()

glimpse(bc_data)

# Visualization of total quantity purchased by gender for each category
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
  ylim(0, min(bc_data$total_quantity) * 11)

# Preparering data for finding out the distribution of all category items
category_distribution <- table(shopping_data$category)
category_percentages <- prop.table(category_distribution) * 100
category_data <- data.frame(Category = names(category_distribution), Count = category_distribution)

# Visualizing Distribution of all Category items

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

# Finding out the payment method usage by ages

shopping_data$age_group <- cut(shopping_data$age, breaks = c(15, 25, 35, 45, 55, 65, Inf), labels = c("15-25", "26-35", "36-45", "46-55", "56-65", "66+"))

bar_data <- table(shopping_data$age_group, shopping_data$payment_method)
bar_data <- as.data.frame(bar_data)

glimpse(bar_data)

# Visualizing the payment methods used by different age groups

ggplot(bar_data, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Age Group vs Payment Method",
       x = "Age Group", y = "Frequency",
       fill = "Payment Methods") +
  scale_fill_manual(values = c("blue", "green", "red")) +  # Customize colors if needed
  theme_minimal()

# Visualizing Price vs Quantity purchased based on gender

ggplot(shopping_data, aes(x = price, y = quantity, size = quantity, color = gender)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Bubble Plot for Price vs. Quantity by Gender",
       x = "Price", y = "Quantity", size = "Quantity", color = "Gender") +
  theme_minimal()

## Association Rule Mining

# Creating transactions & finding the maximal sets
transactions <- as(shopping_data, "transactions")

sets <- apriori(transactions, parameter = list(support = 0.05, target = "frequent itemsets"))
inspect(sets)

maximal = sets[is.maximal(sets)]
summary(maximal)
inspect(head(maximal, n = 5, by = "support"))

head(maximal)

# Making a maximal set with main focus on Category variable
maximal_cat_cloth <- subset(maximal, subset=(items %in% "category=Clothing"))
maxi_cloth_df <- as(maximal_cat_cloth, "data.frame")

head(maxi_cloth_df)

# Interpretation:- 1) Shopping Mall Preferences : Different shopping malls exhibit varying associations with clothing purchases. Metrocity, Kanyon, and Mall of Istanbul show significant connections with customers buying clothing items.
# 2) Payment Method Usage : There's a notable preference for payment methods when purchasing clothing. Debit cards emerge as a favored payment mode among customers.
# 3) Quantity and Price Range Associations : Patterns in the quantity of clothing items bought (e.g., 1-2 items) and their price ranges (e.g., $81.3 to $900) shed light on customer preferences and spending habits.

# Making a maximal set with main focus on Female value from Gender variable
maximal_gen_female <- subset(maximal, subset=(items %in% "gender=Female"))
gen_female_df <- as(maximal_gen_female, "data.frame")

head(gen_female_df)

# Interpretation :- 1) Shopping Mall Preferences for Females : There is a strong association of females with specific shopping malls. Metrocity, Istinye Park, and Metropol AVM indicate a significant correlation with female shoppers, portraying preferences among this demographic for these malls.
# 2) Payment Method Preference : Female customers exhibit a distinct preference for payment methods. Debit cards emerge prominently as the favored payment mode among female shoppers.
# 3) Age Group Preferences : The analysis highlights particular age groups within females that show consistent associations with shopping patterns. Females between 18-35, 35-52, and 52-69 exhibit diverse preferences in purchasing behavior, especially in terms of item categories and price ranges.
# 4) Category-Specific Preferences : Female shoppers exhibit strong associations with specific categories such as Clothing, Shoes, Toys, Cosmetics, and Food & Beverage, showcasing distinct preferences for these product categories.

# Mining rules using apriori & inspceting top rules
rules <- apriori(shopping_data, parameter = list(support = 0.05, confidence = 0.5, target = "rules", minlen = 2))

rules_max <- subset(rules, subset = is.maximal(rules))

head(rules_max)

df_max_apriori_rules <- as(rules_max, "data.frame")

head(df_max_apriori_rules)

# Interpretation :- 1) Association between Product Categories and Price Ranges : Certain categories like Books, Technology, and Souvenir are associated with specific price ranges (e.g., Books tend to be within the price range of 5.23 to 81.3)
# 2) Shopping Mall Preferences and Gender : There are gender preferences observed in certain shopping malls. For instance, at Istinye Park, there's a higher tendency for female shoppers.
# 3) Association between Product Categories and Price Ranges with Gender : Shoes and Toys categories have associations with price ranges and gender. Females tend to purchase Shoes within the price range of 900 to 5250 and Toys within the price range of 81.3 to 900
# 4) Age Group Preferences and Shopping Patterns : Different age groups exhibit distinct shopping patterns. For instance, individuals aged 18 to 35 are inclined towards Clothing within the price range of 900 to 5250, while those aged 52 to 69 exhibit a similar preference but with a wider price range of 81.3 to 900. Moreover, certain age groups, like 35 to 52, exhibit preferences for Food & Beverage within a specific price range.

# Mining rules using eclat & inspecting top rules
maximal_eclat <- eclat(transactions, parameter = list(support = 0.05, maxlen = 10))

rules_from_maximal <- ruleInduction(maximal_eclat, transactions, confidence = 0.5)

rules_max_eclat <- subset(rules_from_maximal, subset = is.maximal(rules_from_maximal))

df_max_eclat_rules <- as(rules_max_eclat, "data.frame")

head(df_max_eclat_rules)

# Interpretation :- 1) Category-Price Range Associations : Books, Technology, Souvenir, and Food & Beverage categories exhibit associations with specific price ranges. For instance, Books are typically priced between 5.23 to 81.3 units, while Technology items are commonly within the price range of 900 to 5250 units.
# 2) Gender-Category Associations : There are gender-specific preferences observed within certain product categories. For instance, there's a clear association between Female shoppers and categories like Food & Beverage, Clothing, Cosmetics, and Toys within various price ranges.
# 3) Age Group Preferences and Categories : Different age groups show distinct preferences for specific categories. For instance, individuals aged 52 to 69 tend to favor Clothing items priced between 900 to 5250 units, whereas those aged 18 to 35 exhibit preferences for Clothing within a wider price range of 81.3 to 900 units.
# 4) Payment Method and Gender Associations : There's a discernible relationship between payment methods and gender. Female shoppers tend to use specific payment methods, such as Cash or Credit Card, for certain categories like Clothing, Cosmetics, and Food & Beverage across varied price ranges.



## Classification
# Pre-Processing
# Converting categorical variables to factors
shopping_data$gender <- as.factor(shopping_data$gender)
shopping_data$category <- as.factor(shopping_data$category)
shopping_data$payment_method <- as.factor(shopping_data$payment_method)
shopping_data$shopping_mall <- as.factor(shopping_data$shopping_mall)

# Indexing and Splitting data-set
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(selected_shopping_data$gender, p = 0.7, list = FALSE)
training_data <- selected_shopping_data[trainIndex, ]
testing_data <- selected_shopping_data[-trainIndex, ]

# Random Forest
rf_model <- randomForest(gender ~ ., data = training_data)

predictions_rf <- predict(rf_model, newdata = testing_data)

head(predictions_rf) 

# Confusion Matrix for Evaluating Random Forest
conf_matrix1 <- confusionMatrix(predictions_rf, testing_data$gender)


conf_matrix1
precision <- conf_matrix1$byClass["Pos Pred Value"]
recall <- conf_matrix1$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Plotting Confusion matrix for Random Forest classification
plot(conf_matrix1$table, 
     col = conf_matrix1$byClass,
     main = paste("Confusion Matrix - Accuracy:", round(conf_matrix1$overall["Accuracy"], 3)))

# Naive Bayes
nb_model <- naiveBayes(gender ~ ., data = training_data)

predictions_nb <- predict(nb_model, newdata = testing_data)

head(predictions_nb)

# Confusion Matrix for Evaluating Naive Bayes
conf_matrix2 <- confusionMatrix(predictions_nb, testing_data$gender)

conf_matrix2
precision1 <- conf_matrix2$byClass["Pos Pred Value"]
recall1 <- conf_matrix2$byClass["Sensitivity"]
f1_score1 <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision1, "\n")
cat("Recall:", recall1, "\n")
cat("F1 Score:", f1_score1, "\n")

# Plotting Confusion matrix for Naive Bayes classification
plot(conf_matrix2$table, 
     col = conf_matrix2$byClass,
     main = paste("Confusion Matrix - Accuracy:", round(conf_matrix2$overall["Accuracy"], 3)))

## Clustering
# Clustering using DBSCAN
shopping_features <- shopping_data[, c("age", "quantity", "price")]

dbscan_result <- dbscan(shopping_features, eps = 3, minPts = 1000)
table(dbscan_result$cluster)
dbscan_result
cluster_data <- data.frame(shopping_features, Cluster = dbscan_result$cluster)

head(cluster_data)

# Visualizing the clustering from DBSCAN
ggplot(cluster_data, aes(x = quantity, y = age, color = factor(Cluster))) +
  geom_point() +
  labs(title = "DBSCAN Clustering Results",
       x = "quantity", y = "Age", color = "Cluster") +
  theme_minimal()

# Clustering using k-means
kmeans_result <- kmeans(shopping_features, centers = 3)
table(kmeans_result$cluster)
head(kmeans_result$cluster)
shopping_data$cluster <- as.factor(kmeans_result$cluster)

# Visualizing the clustering from k-means
ggplot(shopping_data, aes(x = quantity, y = age)) +
  geom_point(aes(color = factor(kmeans_result$cluster))) +
  labs(title = "Scatter Plot with 3 Clusters", x = "quantity", y = "age", color = "Cluster") +
  scale_color_manual(values = c("blue", "green", "red")) +  # Customize colors if needed
  theme_minimal()
