
# Load necessary libraries

library(readr)
library(dplyr)
library(ggplot2)


# Load the data set 
data <- read.csv("/Users/burcucestan/Desktop/HW2/match_data", header=TRUE)

# Display the first few rows of data
#cat("First few rows of the data:\n")
#head(data)

#cat("\nStructure of the data:\n")
#str(data)


# Filtering the data
# Remove rows where "suspended" or "stopped" is TRUE
filtered_data <- data[data$suspended == "False" & data$stopped == "False", ]

# Retain 'result' and 'halftime' columns for further steps
results <- filtered_data$result
halftime <- filtered_data$halftime

# Calculate the percentage of missing values for each column
missing_percentage <- colSums(is.na(filtered_data)) / nrow(filtered_data) * 100

# Remove columns with more than 70% missing data
columns_to_remove <- names(missing_percentage[missing_percentage > 70])
filtered_data <- filtered_data[, !(names(filtered_data) %in% columns_to_remove)]

# Fill missing values in columns with less than 10% missing data
columns_to_impute <- names(missing_percentage[missing_percentage <= 10])
for (col in columns_to_impute) {
  if (is.numeric(filtered_data[[col]])) {
    filtered_data[[col]][is.na(filtered_data[[col]])] <- mean(filtered_data[[col]], na.rm = TRUE)
  }
}

# Re-add 'result' and 'halftime' columns to filtered data
filtered_data <- filtered_data %>% 
  mutate(result = results, halftime = halftime)

# Check the dimensions of the filtered dataset
cat("\nDimensions of the filtered dataset:\n")
dim(filtered_data)

## TASK 1

# Create a new data frame 'odds_data' to isolate odds columns and other relevant columns
odds_data <- filtered_data %>% select(fixture_id, X1, X2, X, result, halftime, minute)

# Convert odds columns (X1, X2, X) to numeric data type
odds_data <- odds_data %>% mutate(across(c(X1, X2, X), as.numeric))

# Calculate implied probabilities based on the odds columns
implied_probs <- odds_data %>% 
  mutate(
    `Pr{Home Win}` = 1 / X1,    # Implied probability for Home Win
    `Pr{Away Win}` = 1 / X2,    # Implied probability for Away Win
    `Pr{Tie}` = 1 / X           # Implied probability for Tie
  ) %>% 
  select(`Pr{Home Win}`, `Pr{Away Win}`, `Pr{Tie}`, result, halftime, fixture_id, minute)  # Select relevant columns for the output

# Display the first few rows of the resulting implied probabilities table
cat("Implied probabilities with result and halftime:\n")
head(implied_probs)

#Calculate the normalization factor
implied_probs <- implied_probs %>%
  mutate(normalization_factor = `Pr{Home Win}` + `Pr{Away Win}` + `Pr{Tie}`)

#Normalize probabilities
normalized_probs <- implied_probs %>%
  mutate(
    `Pr{Home Win}` = `Pr{Home Win}` / normalization_factor,  # Normalize Home Win probability
    `Pr{Away Win}` = `Pr{Away Win}` / normalization_factor,  # Normalize Away Win probability
    `Pr{Tie}` = `Pr{Tie}` / normalization_factor             # Normalize Tie probability
  ) %>%
  select(-normalization_factor)  # Remove the normalization factor column

# Display the first few rows of the normalized probabilities table
cat("\nNormalized probabilities:\n")
head(normalized_probs)

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
implied_probs <- implied_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
implied_probs <- implied_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))

# Filter data for the first half
first_half_data <- implied_probs %>%
  filter(halftime == "1st-half")

# Calculate the total number of games in each bin
bin_totals <- first_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties <- first_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities <- bin_totals %>%
  left_join(bin_ties, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws <- first_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data <- tie_probabilities %>%
  left_join(bookmaker_draws, by = "Bin")

# Visualize the results
library(ggplot2)

ggplot(visualization_data, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "blue",
    color = "black"
  ) +
  geom_point(
    aes(
      x = as.numeric(Bin),
      y = BookmakerTieRate
    ),
    color = "red",
    size = 3,    # Marker size
    shape = 17   # Triangle marker for bookmark style
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (First Half, Implied Probabilities)",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
implied_probs <- implied_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
implied_probs <- implied_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))

# Filter data for the second half
second_half_data <- implied_probs %>%
  filter(halftime == "2nd-half")

# Calculate the total number of games in each bin
bin_totals_second_half <- second_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties_second_half <- second_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities_second_half <- bin_totals_second_half %>%
  left_join(bin_ties_second_half, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws_second_half <- second_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data_second_half <- tie_probabilities_second_half %>%
  left_join(bookmaker_draws_second_half, by = "Bin")

# Visualize the results with red triangular markers
library(ggplot2)

ggplot(visualization_data_second_half, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "blue",  # Use blue for the bars
    color = "black"
  ) +
  geom_point(
    aes(x = as.numeric(Bin), y = BookmakerTieRate),
    color = "red",
    shape = 17,  # Use a triangular shape
    size = 3    
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (Second Half, Implied Probabilities)",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
normalized_probs <- normalized_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
normalized_probs <- normalized_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))

# Filter data for the first half
first_half_data <- normalized_probs %>%
  filter(halftime == "1st-half")

# Calculate the total number of games in each bin
bin_totals <- first_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties <- first_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities <- bin_totals %>%
  left_join(bin_ties, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws <- first_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data <- tie_probabilities %>%
  left_join(bookmaker_draws, by = "Bin")

# Visualize the results
library(ggplot2)

ggplot(visualization_data, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "purple",
    color = "black"
  ) +
  geom_point(
    aes(
      x = as.numeric(Bin),
      y = BookmakerTieRate
    ),
    color = "red",
    size = 3,
    shape = 17  # Triangle marker
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (First Half, Normalized Probabilities)",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
normalized_probs <- normalized_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
normalized_probs <- normalized_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))

# Filter data for the second half
second_half_data <- normalized_probs %>%
  filter(halftime == "2nd-half")

# Calculate the total number of games in each bin
bin_totals_second_half <- second_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties_second_half <- second_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities_second_half <- bin_totals_second_half %>%
  left_join(bin_ties_second_half, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws_second_half <- second_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data_second_half <- tie_probabilities_second_half %>%
  left_join(bookmaker_draws_second_half, by = "Bin")

# Visualize the results
library(ggplot2)

ggplot(visualization_data_second_half, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "purple",  # Use purple for the bars
    color = "black"
  ) +
  geom_point(
    aes(
      x = as.numeric(Bin), 
      y = BookmakerTieRate
    ),
    color = "red",
    shape = 17,  # Shape 17 represents a triangle in ggplot2
    size = 3     # Adjust size for visibility
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (Second Half, Normalized Probabilities)",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

## TASK 2

# Add a new column for the "actual minute" of the match
filtered_data <- filtered_data %>%
  mutate(
    actual_minute = ifelse(halftime == "1st-half", minute, minute + 45)
  )

# Case 1: Goals in the first 10 minutes
filtered_data <- filtered_data %>%
  mutate(
    Goal_in_first_10_mins = ifelse(
      actual_minute <= 10 & (`Goals...home` + `Goals...away`) >= 1,
      TRUE, FALSE
    )
  )

# Case 2: Goals after the 85th minute
filtered_data <- filtered_data %>%
  mutate(
    Goal_after_85_mins = ifelse(
      actual_minute > 85 & (`Goals...home` + `Goals...away`) >= 1,
      TRUE, FALSE
    )
  )

# Case 3: Substitutions in the first 30 minutes
filtered_data <- filtered_data %>%
  mutate(
    Substitutions_in_first_30_mins = ifelse(
      halftime == "1st-half" & actual_minute <= 30 &
        (`Substitutions...home` + `Substitutions...away`) >= 1,
      TRUE, FALSE
    )
  )

# Case 4: Penalty Awarded in the First 15 Minutes
filtered_data <- filtered_data %>%
  mutate(
    Penalty_in_first_15_mins = ifelse(
      halftime == "1st-half" & actual_minute <= 15 &
        (`Penalties...home` + `Penalties...away`) >= 1,
      TRUE, FALSE
    )
  )

# Case 5: Multiple Yellow Cards in the First 20 Minutes
filtered_data <- filtered_data %>%
  mutate(
    Multiple_yellow_cards_first_20_mins = ifelse(
      halftime == "1st-half" & actual_minute <= 20 &
        (`Yellowcards...home` + `Yellowcards...away`) > 1,
      TRUE, FALSE
    )
  )

# Calculate and print the number of matches fitting each case
cat("Number of matches with a goal before 10 minutes: ", sum(filtered_data$Goal_in_first_10_mins), "\n")
cat("Number of matches with a goal after 85 minutes: ", sum(filtered_data$Goal_after_85_mins), "\n")
cat("Number of matches with substitutions in the first 30 minutes: ", 
    sum(filtered_data$Substitutions_in_first_30_mins), "\n")
cat("Number of matches with a penalty in the first 15 minutes: ", 
    sum(filtered_data$Penalty_in_first_15_mins), "\n")
cat("Number of matches with multiple yellow cards in the first 20 minutes: ", 
    sum(filtered_data$Multiple_yellow_cards_first_20_mins), "\n")

# Create a new dataset for cleaned data after removing noisy rows
cleaned_data <- filtered_data %>%
  filter(
    !(Goal_in_first_10_mins | 
        Goal_after_85_mins | 
        Substitutions_in_first_30_mins | 
        Penalty_in_first_15_mins | 
        Multiple_yellow_cards_first_20_mins)
  )

# Calculate and print the number of rows removed
original_row_count <- nrow(filtered_data)
removed_row_count <- original_row_count - nrow(cleaned_data)

cat("Number of rows removed as noisy data: ", removed_row_count, "\n")
cat("Number of rows in the cleaned dataset: ", nrow(cleaned_data), "\n")

# Create a new data frame 'cleaned_odds_data' to isolate odds columns and other relevant columns
cleaned_odds_data <- cleaned_data %>% select(X1, X2, X, result, halftime)

# Convert odds columns (X1, X2, X) to numeric data type
cleaned_odds_data <- cleaned_odds_data %>% mutate(across(c(X1, X2, X), as.numeric))

# Calculate implied probabilities based on the odds columns
cleaned_implied_probs <- cleaned_odds_data %>% 
  mutate(
    `Pr{Home Win}` = 1 / X1,    # Implied probability for Home Win
    `Pr{Away Win}` = 1 / X2,    # Implied probability for Away Win
    `Pr{Tie}` = 1 / X           # Implied probability for Tie
  ) %>% 
  select(`Pr{Home Win}`, `Pr{Away Win}`, `Pr{Tie}`, result, halftime)  # Select relevant columns for the output

# Display the first few rows of the resulting implied probabilities table
cat("Implied probabilities with result and halftime:\n")
head(cleaned_implied_probs)

# Calculate the normalization factor
cleaned_implied_probs <- cleaned_implied_probs %>%
  mutate(normalization_factor = `Pr{Home Win}` + `Pr{Away Win}` + `Pr{Tie}`)

# Normalize probabilities
cleaned_normalized_probs <- cleaned_implied_probs %>%
  mutate(
    `Pr{Home Win}` = `Pr{Home Win}` / normalization_factor,  # Normalize Home Win probability
    `Pr{Away Win}` = `Pr{Away Win}` / normalization_factor,  # Normalize Away Win probability
    `Pr{Tie}` = `Pr{Tie}` / normalization_factor             # Normalize Tie probability
  ) %>%
  select(-normalization_factor)  # Remove the normalization factor column

# Display the first few rows of the normalized probabilities table
cat("\nNormalized probabilities:\n")
head(cleaned_normalized_probs)

# Validate that all rows sum to 1
# Calculate row sums for normalized probabilities
row_sums <- rowSums(cleaned_normalized_probs %>% select(`Pr{Home Win}`, `Pr{Away Win}`, `Pr{Tie}`))

# Check if all rows sum to 1
cat("\nDo all rows sum to 1? ", all.equal(row_sums, rep(1, nrow(cleaned_normalized_probs))), "\n")

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
cleaned_implied_probs <- cleaned_implied_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
cleaned_implied_probs <- cleaned_implied_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))

# Filter data for the first half
first_half_data <- cleaned_implied_probs %>%
  filter(halftime == "1st-half")

# Calculate the total number of games in each bin
bin_totals <- first_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties <- first_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities <- bin_totals %>%
  left_join(bin_ties, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws <- first_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data <- tie_probabilities %>%
  left_join(bookmaker_draws, by = "Bin")

# Visualize the results
library(ggplot2)

ggplot(visualization_data, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "blue",
    color = "black"
  ) +
  geom_point(
    aes(
      x = as.numeric(Bin),
      y = BookmakerTieRate
    ),
    color = "red",
    size = 3,    # Marker size
    shape = 17   # Triangle marker for bookmark style
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (First Half, Implied Probabilities) - After Noise Removal",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
cleaned_implied_probs <- cleaned_implied_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
cleaned_implied_probs <- cleaned_implied_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))

# Filter data for the first half
second_half_data <- cleaned_implied_probs %>%
  filter(halftime == "2nd-half")

# Calculate the total number of games in each bin
bin_totals <- second_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties <- second_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities <- bin_totals %>%
  left_join(bin_ties, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws <- second_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data <- tie_probabilities %>%
  left_join(bookmaker_draws, by = "Bin")

# Visualize the results
library(ggplot2)

ggplot(visualization_data, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "blue",
    color = "black"
  ) +
  geom_point(
    aes(
      x = as.numeric(Bin),
      y = BookmakerTieRate
    ),
    color = "red",
    size = 3,    # Marker size
    shape = 17   # Triangle marker for bookmark style
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (Second Half, Implied Probabilities)-After Noise Removal",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
cleaned_normalized_probs <- cleaned_normalized_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
cleaned_normalized_probs <- cleaned_normalized_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))

# Filter data for the first half
first_half_data <- cleaned_normalized_probs %>%
  filter(halftime == "1st-half")

# Calculate the total number of games in each bin
bin_totals <- first_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties <- first_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities <- bin_totals %>%
  left_join(bin_ties, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws <- first_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data <- tie_probabilities %>%
  left_join(bookmaker_draws, by = "Bin")

# Visualize the results for the first half
library(ggplot2)

ggplot(visualization_data, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "purple",
    color = "black"
  ) +
  geom_point(
    aes(
      x = as.numeric(Bin),
      y = BookmakerTieRate
    ),
    color = "red",
    size = 3,    # Marker size
    shape = 17   # Triangle marker for bookmark style
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (First Half, Cleaned and Normalized Probabilities)",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

# Define the bins with an interval of 0.2
bins <- seq(-1, 1, by = 0.2)  # Define bins from -1 to 1 with a step of 0.2

# Calculate the difference between Home Win and Away Win probabilities
cleaned_normalized_probs <- cleaned_normalized_probs %>%
  mutate(Difference = `Pr{Home Win}` - `Pr{Away Win}`)

# Assign each row to a bin based on the calculated difference
cleaned_normalized_probs <- cleaned_normalized_probs %>%
  mutate(Bin = cut(Difference, breaks = bins, include.lowest = TRUE))


second_half_data <- cleaned_normalized_probs %>%
  filter(halftime == "2nd-half")

# Calculate the total number of games in each bin
bin_totals <- second_half_data %>%
  group_by(Bin) %>%
  summarize(TotalGames = n())

# Calculate the number of ties (draws) in each bin
bin_ties <- second_half_data %>%
  filter(result == "X") %>%
  group_by(Bin) %>%
  summarize(TieGames = n())

# Calculate the estimated tie probabilities for each bin
tie_probabilities <- bin_totals %>%
  left_join(bin_ties, by = "Bin") %>%
  mutate(
    TieGames = ifelse(is.na(TieGames), 0, TieGames),  # Replace NA values with 0
    EstimatedTieRate = TieGames / TotalGames
  )

# Calculate the average bookmaker tie probabilities for each bin
bookmaker_draws <- second_half_data %>%
  group_by(Bin) %>%
  summarize(BookmakerTieRate = mean(`Pr{Tie}`, na.rm = TRUE))

# Combine estimated and bookmaker probabilities into one dataset
visualization_data <- tie_probabilities %>%
  left_join(bookmaker_draws, by = "Bin")


library(ggplot2)

ggplot(visualization_data, aes(x = Bin)) +
  geom_bar(
    aes(y = EstimatedTieRate),
    stat = "identity",
    width = 0.2,
    fill = "purple",
    color = "black"
  ) +
  geom_point(
    aes(
      x = as.numeric(Bin),
      y = BookmakerTieRate
    ),
    color = "red",
    size = 3,    # Marker size
    shape = 17   # Triangle marker for bookmark style
  ) +
  labs(
    title = "Estimated Tie Rates vs. Bookmaker Tie Rates (Second Half, Cleaned and Normalized Probabilities)",
    x = "Home Win - Away Win (Binned)",
    y = "Probability"
  ) +
  theme_minimal()

## TASK 3

# Add the target column to the cleaned dataset
cleaned_data$target <- ifelse(cleaned_data$result == "1", 1,  # Home team wins
                              ifelse(cleaned_data$result == "2", 2,  # Away team wins
                                     0))                                  # Tie

# Remove irrelevant columns from the cleaned dataset
columns_to_remove <- c("Penalties...home", "Penalties...away", "Assists...home", "Assists...away",
                       "current_time", "half_start_datetime", "match_start_datetime",
                       "latest_bookmaker_update", "suspended", "stopped", "name", "second", "ticking",
                       "result", "final_score", "X1", "X2", "X")

# Update the cleaned dataset by removing the specified columns
prepared_data <- cleaned_data[, !(names(cleaned_data) %in% columns_to_remove)]

prepared_data$halftime <- ifelse(prepared_data$halftime == "1st-half", 1, 
                                 ifelse(prepared_data$halftime == "2nd-half", 2, NA))

# Display the first few rows of the updated dataset
#head(prepared_data)

# Convert the target column to factor with labels
prepared_data$target <- factor(prepared_data$target,
                               levels = c(0, 1, 2),
                               labels = c("Tie", "Home Win", "Away Win"))

# Verify the conversion
print(table(prepared_data$target))

# Add "Combined_Goals" column (sum of home and away goals)
prepared_data$Combined_Goals <- prepared_data$Goals...home + prepared_data$Goals...away

# Add "Net_Goal_Difference" column (net difference between home and away goals)
prepared_data$Net_Goal_Difference <- prepared_data$Goals...home - prepared_data$Goals...away

# Add "Aggressive_Attack_Balance" column (difference between home and away dangerous attacks)
prepared_data$Aggressive_Attack_Balance <- prepared_data$Dangerous.Attacks...home - prepared_data$Dangerous.Attacks...away

# Add "Possession_Imbalance" column (difference between home and away ball possession percentages)
prepared_data$Possession_Imbalance <- prepared_data$Ball.Possession.....home - prepared_data$Ball.Possession.....away


# Add "Shot_Effort_Difference" column (difference between home and away goal attempts)
prepared_data$Shot_Effort_Difference <- prepared_data$Goal.Attempts...home - prepared_data$Goal.Attempts...away

# Add "Match_Minute_Tracker" column (cumulative match minute tracker)
prepared_data$Match_Minute_Tracker <- with(prepared_data, ifelse(halftime == "1st-half", minute, minute + 45))

# Optional Enhancements: Add new derived features for deeper analysis
# Add "Goal_Ratio" column (ratio of home goals to away goals, avoiding division by zero)
prepared_data$Goal_Ratio <- ifelse(prepared_data$Goals...away == 0, 
                                   prepared_data$Goals...home, 
                                   prepared_data$Goals...home / prepared_data$Goals...away)

# Add "Dangerous_Attacks_Ratio" column (ratio of home to away dangerous attacks)
prepared_data$Dangerous_Attacks_Ratio <- ifelse(prepared_data$Dangerous.Attacks...away == 0, 
                                                prepared_data$Dangerous.Attacks...home, 
                                                prepared_data$Dangerous.Attacks...home / prepared_data$Dangerous.Attacks...away)

# Add "Possession_Dominance_Flag" column (flag indicating significant possession advantage for one team)
prepared_data$Possession_Dominance_Flag <- ifelse(abs(prepared_data$Possession_Imbalance) > 20, 1, 0)


# Display the first few rows of the modified dataset
#head(prepared_data)

# Display column names to ensure the dataset includes necessary features
#print(names(prepared_data))

# Show the first few rows to check data consistency
#print(head(prepared_data))

#Remove any rows with missing target values
prepared_data <- prepared_data[!is.na(prepared_data$target), ]

library(rpart)
library(rpart.plot)

# Define the target variable and features
features <- prepared_data[, !(names(prepared_data) %in% c("target"))]
target <- prepared_data$target

# Combine into a single dataframe
model_data <- prepared_data[, c(names(features), "target")]

# Train-test split (70-30)
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

# Train a decision tree
decision_tree <- rpart(target ~ ., 
                       data = train_data, 
                       method = "class", 
                       control = rpart.control(maxdepth = 5))

# Visualize the decision tree
rpart.plot(decision_tree, main = "Decision Tree for Match Outcome")

# Predict outcomes for the test data
predictions <- predict(decision_tree, newdata = test_data, type = "class")

# Generate a confusion matrix
confusion_matrix <- table(test_data$target, predictions)

# Print the confusion matrix
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate model accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model Accuracy:", round(accuracy, 3)))

library(caret)

# Generate detailed performance metrics
confusion_details <- confusionMatrix(predictions, as.factor(test_data$target))
print(confusion_details)

#Normalize Implied Probabilities from Odds Data
odds_data$Pr_Home_Win <- 1 / odds_data$X1
odds_data$Pr_Draw     <- 1 / odds_data$X
odds_data$Pr_Away_Win <- 1 / odds_data$X2

# Normalize probabilities to ensure they sum to 1
total_probs <- odds_data$Pr_Home_Win + odds_data$Pr_Draw + odds_data$Pr_Away_Win
odds_data$Pr_Home_Win <- odds_data$Pr_Home_Win / total_probs
odds_data$Pr_Draw     <- odds_data$Pr_Draw / total_probs
odds_data$Pr_Away_Win <- odds_data$Pr_Away_Win / total_probs

# Select relevant columns from odds_data
odds_data_selected <- odds_data[, c("fixture_id", "halftime", "minute", 
                                    "Pr_Home_Win", "Pr_Draw", "Pr_Away_Win")]

#Predict Probabilities Using the Decision Tree Model
predicted_probs <- predict(decision_tree, newdata = prepared_data, type = "prob")

# Convert the predicted probabilities to a DataFrame
predicted_probs_df <- as.data.frame(predicted_probs)

# Rename the columns for clarity
colnames(predicted_probs_df) <- c("Pred_Tie", "Pred_Home_Win", "Pred_Away_Win")

# Combine predicted probabilities with prepared_data
prepared_data <- cbind(prepared_data, predicted_probs_df)

# Convert halftime in odds_data to match prepared_data format
odds_data$halftime <- ifelse(odds_data$halftime == "1st-half", 1, 
                             ifelse(odds_data$halftime == "2nd-half", 2, NA))

# Select relevant columns from odds_data
odds_data_selected <- odds_data[, c("fixture_id", "halftime", "minute", 
                                    "Pr_Home_Win", "Pr_Draw", "Pr_Away_Win")]



#Merge Prepared Data with Odds Data Based on Fixture ID, Halftime, and Minute
prepared_data <- merge(prepared_data, odds_data_selected, 
                       by = c("fixture_id", "halftime", "minute"), 
                       all.x = TRUE)

#Calculate Absolute Differences Between Implied and Predicted Probabilities
prepared_data$Diff_Home_Win <- abs(prepared_data$Pr_Home_Win - prepared_data$Pred_Home_Win)
prepared_data$Diff_Draw     <- abs(prepared_data$Pr_Draw - prepared_data$Pred_Tie)
prepared_data$Diff_Away_Win <- abs(prepared_data$Pr_Away_Win - prepared_data$Pred_Away_Win)

#Display the First Few Rows to Confirm Changes
head(prepared_data[, c("fixture_id", "halftime", "minute", 
                       "Pr_Home_Win", "Pred_Home_Win", "Diff_Home_Win",
                       "Pr_Draw", "Pred_Tie", "Diff_Draw", 
                       "Pr_Away_Win", "Pred_Away_Win", "Diff_Away_Win")])


library(ggplot2)

# Home Win Difference
ggplot(prepared_data, aes(x = Diff_Home_Win)) +
  geom_histogram(binwidth = 0.02, fill = "blue", alpha = 0.7) +
  labs(title = "Home Win Probability Differences", x = "Difference", y = "Frequency")

# Draw Difference
ggplot(prepared_data, aes(x = Diff_Draw)) +
  geom_histogram(binwidth = 0.02, fill = "red", alpha = 0.7) +
  labs(title = "Draw Probability Differences", x = "Difference", y = "Frequency")

# Away Win Difference
ggplot(prepared_data, aes(x = Diff_Away_Win)) +
  geom_histogram(binwidth = 0.02, fill = "green", alpha = 0.7) +
  labs(title = "Away Win Probability Differences", x = "Difference", y = "Frequency")


# Combined Histogram for All Differences
ggplot(prepared_data) +
  geom_histogram(aes(x = Diff_Home_Win, fill = "Home Win"), binwidth = 0.02, alpha = 0.5) +
  geom_histogram(aes(x = Diff_Draw, fill = "Draw"), binwidth = 0.02, alpha = 0.5) +
  geom_histogram(aes(x = Diff_Away_Win, fill = "Away Win"), binwidth = 0.02, alpha = 0.5) +
  scale_fill_manual(values = c("Home Win" = "blue", "Draw" = "red", "Away Win" = "green")) +
  labs(title = "Combined Probability Differences",
       x = "Difference (Predicted - Implied)", y = "Frequency", fill = "Type")


# Extract and visualize feature importance from the decision tree
importance <- decision_tree$variable.importance

# Convert to a data frame for better visualization
importance_df <- data.frame(Feature = names(importance), Importance = importance)

# Sort by importance descending
importance_df <- importance_df[order(-importance_df$Importance), ]

# Visualize the feature importance
library(ggplot2)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance in Decision Tree Model",
       x = "Features",
       y = "Importance Score") +
  theme_minimal()


# Filter rows with significant deviations (greater than a threshold, e.g., 0.15)
threshold <- 0.15
significant_devs <- prepared_data[prepared_data$Diff_Home_Win > threshold | 
                                    prepared_data$Diff_Draw > threshold | 
                                    prepared_data$Diff_Away_Win > threshold, ]

# Display rows with significant deviations
head(significant_devs[, c("fixture_id", "halftime", "minute", 
                          "Pr_Home_Win", "Pred_Home_Win", "Diff_Home_Win",
                          "Pr_Draw", "Pred_Tie", "Diff_Draw",
                          "Pr_Away_Win", "Pred_Away_Win", "Diff_Away_Win")])

# Summary statistics of significant deviations
#summary(significant_devs[, c("Diff_Home_Win", "Diff_Draw", "Diff_Away_Win")])

# Find the top 10 rows with the largest deviations
top_deviations <- significant_devs[order(-significant_devs$Diff_Home_Win, 
                                         -significant_devs$Diff_Draw, 
                                         -significant_devs$Diff_Away_Win), ]
#head(top_deviations, 10)


# Boxplot for deviations
ggplot(significant_devs) +
  geom_boxplot(aes(x = "Home Win", y = Diff_Home_Win), fill = "blue", alpha = 0.7) +
  geom_boxplot(aes(x = "Draw", y = Diff_Draw), fill = "red", alpha = 0.7) +
  geom_boxplot(aes(x = "Away Win", y = Diff_Away_Win), fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Significant Deviations", 
       x = "Outcome", y = "Deviation")

