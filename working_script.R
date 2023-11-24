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

# Pre-processing

# Association


# Classification


# Clustering


# Visualization


# performance evaluation



# miscellaneous

