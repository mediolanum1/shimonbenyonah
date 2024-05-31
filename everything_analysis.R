# cleaning data from prev sessions
rm(list=ls(all=TRUE))

library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

# setting directory 
# setwd("C:/Users/mediolanum/Desktop/DMA2/")
getwd() # getting current directory

# path to a dataset
file_path = "spotify_data.csv" 

pdf("multiple_plots.pdf", width = 7, height = 5)

# reading the CSV file into a data frame
data <- read.table(file_path, header = TRUE, sep = ",")

# printing first rows of data
head(data) 

summary(data)


# histograms-on-data -----------------------------------------

# Function to create the appropriate plot based on the column type
plot_column <- function(column, col_name) {
  if (is.numeric(column)) {
    # Create a histogram for numeric columns
    hist(column,
         main = paste("Histogram of", col_name),
         xlab = col_name,
         col = "lightblue",
         border = "black")
  } else if (is.character(column)) {
    # Create a bar plot for categorical columns
    barplot(table(column),
            main = paste("Bar Chart of", col_name),
            xlab = col_name,
            ylab = "Frequency",
            col = "lightblue",
            border = "black")
  }
}

# Set up plotting area to show multiple plots
par(mfrow = c(2, 2))  # Adjust the layout based on the number of plots
  
# Loop through each column in the dataset starting from the second column and create the corresponding plot
for (i in 2:ncol(data)) {
  plot_column(data[[i]], names(data)[i])
}

# Reset plotting layout to default
par(mfrow = c(1, 1))


# exploratory data analysis -----------------------------------------------

# Define independent variables (features)
cor <- data[, c('pop', 'dnce', 'nrgy', 'dB', 'spch', 'acous', 'live', 'val', 'bpm', 'year')]
head(cor)
plot(cor)
# Define threshold for popularity
popularity_threshold <- 60

# Add a new column 'is_popular' based on the threshold
data <- data %>%
  mutate(is_popular = ifelse(pop > popularity_threshold, "Popular", "Not Popular"))

head(data)

# Load necessary library for plots
library(ggplot2)

# Plotting function to compare popular vs not popular songs
plot_feature <- function(data, feature) {
  ggplot(data, aes_string(x = "is_popular", y = feature, fill = "is_popular")) +
    geom_boxplot() +
    labs(title = paste("Comparison of", feature, "between Popular and Not Popular Songs"),
         x = "Popularity",
         y = feature) +
    theme_minimal()
}

# List of features to compare
features <- c("dnce", "nrgy", "dB", "spch", "acous", "live", "val", "bpm")

# Create plots for each feature
plots <- lapply(features, function(feature) plot_feature(data, feature))

# Print all plots
for (plot in plots) {
  print(plot)
}

# Summary statistics for popular vs not popular songs
summary_stats <- data %>%
  group_by(is_popular) %>%
  summarise(across(all_of(features), list(mean = ~mean(.), median = ~median(.), sd = ~sd(.))))

# showing summary stats
as.data.frame(summary_stats)


# lm ----------------------------------------------------------------------

# Define independent variables (features)
X <- data[, c('dnce', 'nrgy', 'dB', 'spch', 'acous', 'live', 'val', 'bpm', 'year')]

head(X)

# Define dependent variable
y <- data$pop

# Fit the model
model <- lm(y ~ ., data = X)

# Print summary statistics of the model
summary(model)

# plotting model
plot(model)




# heat-map ----------------------------------------------------------------

# Select the numerical columns for correlation
num_columns <- data[, sapply(data, is.numeric)]

# computing corr matrix
corr_matrix <- cor(num_columns, use = "complete.obs")

# reshaping to suit for ggplot2
melted_corr_matrix <- melt(corr_matrix)

ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap of Spotify Data",
       x = "Features",
       y = "Features")



dev.off()
  #q("no")