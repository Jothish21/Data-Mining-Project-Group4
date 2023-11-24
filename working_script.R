# Loading packages
library(tidyverse)
library(dplyr)

shopping_data <- read.csv("data/customer_shopping_data.csv")

glimpse(shopping_data)
summarise(shopping_data)
View(shopping_data)
