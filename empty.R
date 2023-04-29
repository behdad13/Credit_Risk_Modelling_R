data <- read.csv('german_credit_data.csv')








#1
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Target class balance
label_names <- c("Good", "Bad")
color_list <- c("navy", "mediumvioletred")
total <- nrow(data)
title <- "Target Class Balance"

# Create donut plot function
donut_plot <- function(df, col, label_names, colors, title, text) {
  data <- df %>%
    group_by(.data[[col]]) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100,
           label = paste0(label_names, " (", round(percentage, 1), "%)"))
  
  ggplot(data, aes(x = "", y = count, fill = label)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    theme_void() +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5, size = 8),
          legend.title = element_blank(),
          legend.position = "bottom") +
    annotate("text", x = 0, y = 0, label = paste0("Total\n", text),
             size = 6, fontface = "bold", color = "black")
}




#2

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define column names
num_cols <- c("Age", "Credit.amount", "Duration")

# Define color sequence
color_sequence <- c("navy", "mediumseagreen", "navy")

# Create a list to store the plots
numplot_list <- list()

# Loop through the columns and create histograms for each
for (i in 1:length(num_cols)) {
  p <- ggplot(data = data, aes_string(x = num_cols[i])) +
    geom_histogram(fill = color_sequence[i], color = "black", alpha = 0.7, bins = 30) +
    labs(title = paste("Histogram of", num_cols[i]), x = num_cols[i], y = "Frequency") +
    theme_minimal()
  numplot_list[[i]] <- p
}

# Display the plots
for (i in 1:length(numplot_list)) {
  print(numplot_list[[i]])
}
# Visualizing it through a donut chart
donut_plot(df = data, col = 'Risk', label_names = label_names, colors = color_list, title = title, text = total)



#3
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Add 'risk' to the column names
num_cols <- c("Age", "Credit.amount", "Duration")
num_cols <- c(num_cols, "Risk")

# Define color list
color_list <- c("navy", "mediumseagreen", "navy")

# Create a list to store the plots
numplot_list <- list()

# Loop through the columns and create histograms for each, except for the 'risk' column
for (i in 1:(length(num_cols) - 1)) {
  p <- ggplot(data = data, aes_string(x = num_cols[i], fill = "Risk")) +
    geom_histogram(position = "identity", color = "black", alpha = 0.7, bins = 30) +
    scale_fill_manual(values = color_list) +
    labs(title = paste("Histogram of", num_cols[i], "by Risk"), x = num_cols[i], y = "Frequency") +
    theme_minimal()
  numplot_list[[i]] <- p
}

# Display the plots
for (i in 1:length(numplot_list)) {
  print(numplot_list[[i]])
}




#4
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Define the features and hue
features <- c("Age", "Credit.amount", "Duration")
hue <- "Risk"

# Define the color list
color_list <- c("navy", "mediumseagreen", "navy")

# Create a list to store the plots
boxenplot_list <- list()

# Loop through the features and create boxen plots for each
for (i in 1:length(features)) {
  p <- ggplot(data = data, aes_string(x = hue, y = features[i], fill = hue)) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_manual(values = color_list) +
    labs(title = paste(features[i], "by Risk"), x = "Risk", y = features[i]) +
    theme_minimal()
  boxenplot_list[[i]] <- p
}

# Arrange the plots in a grid
grid.arrange(grobs = boxenplot_list, ncol = 3)





#5
data$Sex <- factor(data$Sex)
data$Housing <- factor(data$Housing)
data$Risk <- factor(data$Risk)
data$Saving.accounts <- factor(data$Saving.accounts)
data$Checking.account <- factor(data$Checking.account)
data$Purpose <- factor(data$Purpose)

# Get categorical features
cat_features <- names(data)[sapply(data, is.factor)]

# Function to create bar plots for categorical variables
create_bar_plots <- function(data, cat_features) {
  for (feature in cat_features) {
    plot_data <- data %>%
      filter(!is.na(.data[[feature]])) %>%
      group_by(.data[[feature]]) %>%
      summarize(count = n())

    plot <- ggplot(plot_data, aes(x = .data[[feature]], y = count, fill = .data[[feature]])) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste0("Distribution of ", feature),
           x = feature,
           y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_blank()) +
      coord_flip()

    print(plot)
  }
}

# Call create_bar_plots function
create_bar_plots(data = data, cat_features = cat_features)






#6
# Get categorical features
cat_features <- names(data)[sapply(data, is.factor)]

# Function to create bar plots for categorical variables
create_bar_plots <- function(data, cat_features) {
  for (feature in cat_features) {
    if (feature == "Risk") {
      next # skip Risk feature
    }
    plot_data <- data %>%
      filter(!is.na(.data[[feature]])) %>%
      group_by(.data[[feature]], Risk) %>%
      summarize(count = n(), .groups = "drop")

    plot <- ggplot(plot_data, aes(x = .data[[feature]], y = count, fill = Risk)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste0("Distribution of ", feature),
           x = feature,
           y = "Count",
           fill = "Risk") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_blank(),
            legend.position = "bottom") +
      coord_flip()

    print(plot)
  }
}

# Call create_bar_plots function
create_bar_plots(data = data, cat_features = cat_features)
