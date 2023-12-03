library(arules)  
library(dplyr)
library(tidyverse)

shopping_data <- read.csv("data/customer_shopping_data.csv")
View(shopping_data)
shopping_data

shopping_data$category <- as.factor(shopping_data$category)
shopping_data$gender <- as.factor(shopping_data$gender)


transactions <- as(shopping_data, "transactions")

sets <- apriori(transactions, parameter = list(support = 0.05, target = "frequent itemsets"))
inspect(sets)

closed = sets[is.closed(sets)]
summary(closed)
inspect(head(closed, n = 5, by = "support"))

maximal = sets[is.maximal(sets)]
summary(maximal)
inspect(head(maximal, n = 5, by = "support"))

View(maximal)

# making a maximal set with main focus on category variable
maximal_cat_cloth <- subset(maximal, subset=(items %in% "category=Clothing"))
maxi_cloth_df <- as(maximal_cat_cloth, "data.frame")
View(maxi_cloth_df)

# making a maximal set with main focus on gender female
maximal_gen_female <- subset(maximal, subset=(items %in% "gender=Female"))
gen_female_df <- as(maximal_gen_female, "data.frame")
View(gen_female_df)
# mining rules using apriori & inspceting top rules
rules <- apriori(shopping_data, parameter = list(support = 0.05, confidence = 0.5, target = "rules", minlen = 2))

rules

rules_max <- subset(rules, subset = is.maximal(rules))

inspect(rules_max)

df_max_apriori_rules <- as(rules_max, "data.frame")
View(df_max_apriori_rules)

# mining rules using eclat & inspecting top rules

maximal_eclat <- eclat(transactions, parameter = list(support = 0.05, maxlen = 10))

rules_from_maximal <- ruleInduction(maximal_eclat, transactions, confidence = 0.5)

rules_max_eclat <- subset(rules_from_maximal, subset = is.maximal(rules_from_maximal))

df_max_eclat_rules <- as(rules_max_eclat, "data.frame")
View(df_max_eclat_rules)


