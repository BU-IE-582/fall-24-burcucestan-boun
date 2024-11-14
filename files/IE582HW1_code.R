
# Load necessary libraries

library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(caret)


input_data <- read_csv("/Users/burcucestan/Desktop/HW1/hw1_files/hw1_input.csv", show_col_types = FALSE)
real_s11 <- read_csv("/Users/burcucestan/Desktop/HW1/hw1_files/hw1_real.csv" , show_col_types = FALSE)
img_s11 <- read_csv("/Users/burcucestan/Desktop/HW1/hw1_files/hw1_img.csv", show_col_types = FALSE)

#combine the data
data <- bind_cols(input_data, real_s11, img_s11)



# Ensure all variables are on the same scale before performing PCA.
standardized_data <- scale(data)

# The mean and standard deviation of each variable were checked after standardization to confirm that all variables have a mean close to 0 and a standard deviation close to 1. This ensures that no single variable dominates the PCA due to scale differences.

#apply(standardized_data, 2, mean) -- I commented them since this is too long
#apply(standardized_data, 2, sd)

# Conduct PCA on the standardized data to identify main components.
pca_analysis <- prcomp(standardized_data, center = TRUE, scale. = TRUE)


# Display a summary of PCA
# The summary shows the variance explained by each principal component.
summary(pca_analysis)

#We are selecting the first four components (PC1, PC2, PC3, and PC4) because together they explain approximately 87.03% of the total variance in the data.

# Create a data frame for individual variance and cumulative variance
variance <- data.frame(
  Component = paste0("PC", 1:4),
  Variance_Ratio = (summary(pca_analysis)$importance[2, 1:4]),
  Cumulative_Ratio = cumsum(summary(pca_analysis)$importance[2, 1:4])
)

print(variance)



# Display the loadings of each parameter to the first 4 principal components
loadings <- pca_analysis$rotation[1:11, 1:4]
loadings_df <- as.data.frame(loadings)
rownames(loadings_df) <- colnames(data)[1:11]
colnames(loadings_df) <- paste0("PC", 1:4)

print(loadings_df)

### 3.2. Frequency Selection
# Lower magnitude values indicate less signal loss, meaning the antenna operates efficiently at that frequency.
# Therefore, we calculate the magnitude to identify the resonance frequencies (minimum points of |S11|).

# Specify the number of designs to plot
num_designs <- 10  # Set this to the desired number of designs

# Calculate the magnitude of S11 (|S11|)
magnitude_s11 <- sqrt(real_s11^2 + img_s11^2)

# Create a combined data frame for plotting multiple designs in one plot
plot_data <- data.frame(
  Frequency = rep(1:ncol(magnitude_s11), times = num_designs),  # Repeat frequency index for each design
  Magnitude = as.vector(t(magnitude_s11[1:num_designs, ])),  # Stack magnitude values for the selected designs
  Design = rep(paste("Design", 1:num_designs), each = ncol(magnitude_s11))  # Label each design
)

# Plot using ggplot2 with different colors for each design
library(ggplot2)

ggplot(plot_data, aes(x = Frequency, y = Magnitude, color = Design)) +
  geom_line() +
  labs(title = "S11 Magnitude for Multiple Designs",
       x = "Frequency Index",
       y = "|S11| Magnitude") +
  theme_minimal() +
  scale_color_manual(values = rainbow(num_designs))  # Use rainbow colors for a range of colors

# Initialize a vector to store minimum indices for each design
min_indices <- integer()

# Loop through each design to find the index of the minimum |S11| magnitude
for (i in 1:nrow(magnitude_s11)) {
  min_index <- which.min(magnitude_s11[i, ])  # Find the index of the minimum value for each design
  min_indices <- c(min_indices, min_index)  # Append the result to the vector
}

print(min_indices)

# Count the frequency of each minimum index to find common minimum points
min_index_counts <- table(min_indices)
print(min_index_counts)  # Print frequency of minimum points across designs

# Select the most common minimum indices
selected_indices <- as.numeric(names(min_index_counts)[min_index_counts >= 3])  # Select indices appearing in multiple designs
print(selected_indices)


### 3.3. Regression Modeling for S11

# Regression for frequency at 62

# Define the selected frequency indices
selected_indices <- c(62, 88, 105, 132, 177)


# model the all selected frequencies
freq_index <- 62  
real_selected <- as.numeric(real_s11[[freq_index]])  # Real component
img_selected <- as.numeric(img_s11[[freq_index]])    # Imaginary component

# Tüm 11 geometrik parametre ile modelleme
linear_model_real <- lm(real_selected ~ ., data = input_data)
linear_model_img <- lm(img_selected ~ ., data = input_data)

# print the results
summary(linear_model_real)
summary(linear_model_img)   


# Regression for frequency at 88
#frequency= 88
freq_index <- 88  
real_selected <- as.numeric(real_s11[[freq_index]])  
img_selected <- as.numeric(img_s11[[freq_index]])    


linear_model_real <- lm(real_selected ~ ., data = input_data)
linear_model_img <- lm(img_selected ~ ., data = input_data)


summary(linear_model_real)  
summary(linear_model_img)  


# Regression for frequency at 105

#frequency= 105
freq_index <- 105  
real_selected <- as.numeric(real_s11[[freq_index]])  
img_selected <- as.numeric(img_s11[[freq_index]])    


linear_model_real <- lm(real_selected ~ ., data = input_data)
linear_model_img <- lm(img_selected ~ ., data = input_data)


summary(linear_model_real)  
summary(linear_model_img)  


# Regression for frequency at 132
freq_index <- 132  
real_selected <- as.numeric(real_s11[[freq_index]])  
img_selected <- as.numeric(img_s11[[freq_index]])    


linear_model_real <- lm(real_selected ~ ., data = input_data)
linear_model_img <- lm(img_selected ~ ., data = input_data)


summary(linear_model_real)  
summary(linear_model_img) 


# Regression for frequency at 177

#frequency= 177
freq_index <- 177 
real_selected <- as.numeric(real_s11[[freq_index]])  
img_selected <- as.numeric(img_s11[[freq_index]])    


linear_model_real <- lm(real_selected ~ ., data = input_data)
linear_model_img <- lm(img_selected ~ ., data = input_data)


summary(linear_model_real)  
summary(linear_model_img)  


# Initialize vectors to store MSE and R-squared values for each frequency
mse_real_values <- numeric(length(selected_indices))
mse_img_values <- numeric(length(selected_indices))
r_squared_real_values <- numeric(length(selected_indices))
r_squared_img_values <- numeric(length(selected_indices))

# Loop through each selected frequency index to perform regression analysis
for (i in 1:length(selected_indices)) {
  freq_index <- selected_indices[i]
  
  # Extract the selected frequency points for both real and imaginary components
  real_selected <- as.numeric(real_s11[[freq_index]])  # Ensure it's numeric
  img_selected <- as.numeric(img_s11[[freq_index]])    # Ensure it's numeric
  
  # Fit a linear regression model for the real component using all 11 geometric parameters
  linear_model_real <- lm(real_selected ~ ., data = input_data)
  
  # Fit a linear regression model for the imaginary component using all 11 geometric parameters
  linear_model_img <- lm(img_selected ~ ., data = input_data)
  
  
  # Calculate MSE (Mean Squared Error) for real and imaginary components
  mse_real_values[i] <- mean((real_selected - predict(linear_model_real))^2)
  mse_img_values[i] <- mean((img_selected - predict(linear_model_img))^2)
  
  # Calculate R-squared for both real and imaginary components
  r_squared_real_values[i] <- summary(linear_model_real)$r.squared
  r_squared_img_values[i] <- summary(linear_model_img)$r.squared
}

# Create a data frame to display MSE and R-squared values for each frequency
results_df <- data.frame(
  Frequency = selected_indices,
  MSE_Real = mse_real_values,
  MSE_Imag = mse_img_values,
  R_squared_Real = r_squared_real_values,
  R_squared_Imag = r_squared_img_values
)

# Display the results table
print(results_df)



# Load ggplot2 for plotting
library(ggplot2)

# Sample data (assuming the table you showed is stored in a data frame called `results_df`)
# Uncomment the following line if your data is not yet in `results_df`
# results_df <- data.frame(Frequency = c(62, 88, 105, 132, 177), MSE_Real = c(...), MSE_Imag = c(...), R_squared_Real = c(...), R_squared_Imag = c(...))

# Plot MSE values for Real and Imaginary components across frequencies
ggplot(results_df, aes(x = Frequency)) +
  geom_line(aes(y = MSE_Real, color = "Real MSE"), size = 1.2) +
  geom_point(aes(y = MSE_Real, color = "Real MSE"), size = 3) +
  geom_line(aes(y = MSE_Imag, color = "Imaginary MSE"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = MSE_Imag, color = "Imaginary MSE"), size = 3, shape = 21, fill = "white") +
  labs(title = "MSE for Real and Imaginary Components across Frequencies",
       x = "Frequency",
       y = "Mean Squared Error (MSE)",
       color = "Component") +
  theme_minimal()

# Plot R-squared values for Real and Imaginary components across frequencies
ggplot(results_df, aes(x = Frequency)) +
  geom_line(aes(y = R_squared_Real, color = "Real R-squared"), size = 1.2) +
  geom_point(aes(y = R_squared_Real, color = "Real R-squared"), size = 3) +
  geom_line(aes(y = R_squared_Imag, color = "Imaginary R-squared"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = R_squared_Imag, color = "Imaginary R-squared"), size = 3, shape = 21, fill = "white") +
  labs(title = "R-squared for Real and Imaginary Components across Frequencies",
       x = "Frequency",
       y = "R-squared",
       color = "Component") +
  theme_minimal()


# Load ggplot2 for plotting
library(ggplot2)

# Assuming we have a list of frequencies and their corresponding actual and predicted data
# Replace this with actual data frames for each frequency and component
selected_frequencies <- c(62, 88, 105, 132, 177)
plot_data <- data.frame()  # Empty data frame to store all data for plotting

for (freq in selected_frequencies) {
  # Assuming `real_selected` and `img_selected` contain the actual values,
  # and `predicted_real` and `predicted_img` are the predicted values
  # Replace this with actual values from your models
  
  # Example actual and predicted data
  real_selected <- as.numeric(real_s11[[freq]])
  img_selected <- as.numeric(img_s11[[freq]])
  predicted_real <- predict(lm(real_selected ~ ., data = input_data))
  predicted_img <- predict(lm(img_selected ~ ., data = input_data))
  
  # Create data frames for Real and Imaginary components
  real_data <- data.frame(Frequency = freq, Component = "Real",
                          Actual = real_selected, Predicted = predicted_real)
  img_data <- data.frame(Frequency = freq, Component = "Imaginary",
                         Actual = img_selected, Predicted = predicted_img)
  
  # Combine into one data frame for plotting
  plot_data <- rbind(plot_data, real_data, img_data)
}

# Plot Actual vs Predicted for both Real and Imaginary components across frequencies
ggplot(plot_data, aes(x = Actual, y = Predicted, color = as.factor(Frequency))) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ Component, scales = "free") +
  labs(title = "Actual vs Predicted for S11 Components across Selected Frequencies",
       x = "Actual S11 Value",
       y = "Predicted S11 Value",
       color = "Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom")

