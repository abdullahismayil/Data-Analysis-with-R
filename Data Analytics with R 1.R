# install.packages("tidyverse")
library(tidyverse)

# Load the msleep dataset
data(msleep)

#Task 1 
# Convert specified character variables into factors
msleep <- msleep %>%
  mutate(
    genus = as.factor(genus),
    vore = as.factor(vore),
    order = as.factor(order),
    conservation = as.factor(conservation)
  )

#Task 2 
# Find the animal with the shortest sleep time
shortest_sleep_data <- msleep %>%
  filter(sleep_total == min(sleep_total)) %>%
  select(name, sleep_total)

# Extract the values into variables
shortest_sleep <- shortest_sleep_data$sleep_total
shortest_sleep_mammal <- as.character(shortest_sleep_data$name)

# Print the results
cat("Shortest Sleep Time:", shortest_sleep, "hours\n")
cat("Shortest Sleep Mammal:", shortest_sleep_mammal, "\n")


#Task 3 
# Find the variable with the most missing values
missing_info <- msleep %>%
  summarise_all(~ sum(is.na(.)))

# Extract the variable with the most missing values and its count
most_missing <- names(missing_info)[which.max(missing_info)]
missing_values <- max(missing_info)

# Print the results
cat("Variable with the most missing values:", most_missing, "\n")
cat("Number of missing values:", missing_values, "\n")


#Task 4
# Compute correlations
correlations <- cor(msleep[, sapply(msleep, is.numeric)], use = "pairwise.complete.obs")

# Print the correlations
print(correlations)



#Task 5 
# Compute correlations excluding sleep_total and awake
correlations_without_sleep_awake <- cor(msleep[, sapply(msleep, is.numeric) & !names(msleep) %in% c("sleep_total", "awake")], use = "pairwise.complete.obs")

# Set the diagonal elements to NA
diag(correlations_without_sleep_awake) <- NA

# Identify the pair with the highest correlation
max_corr_pair <- which(correlations_without_sleep_awake == max(correlations_without_sleep_awake, na.rm = TRUE), arr.ind = TRUE)

# Extract the correlation value
highest_corr <- correlations_without_sleep_awake[max_corr_pair]

# Print the variable names and the correlation value
cat("Variables with the highest correlation (excluding sleep_total and awake):", rownames(correlations_without_sleep_awake)[max_corr_pair[1]], "and", colnames(correlations_without_sleep_awake)[max_corr_pair[2]], "\n")
cat("Correlation value:", highest_corr, "\n")

#Task 6 
# Create a ggplot histogram for sleep_total
sleep_histogram <- ggplot(msleep, aes(x = sleep_total)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", aes(y = ..density..)) +
  labs(title = "Distribution of Sleep Times",
       x = "Total Sleep Time",
       y = "Density")

# Print the histogram
print(sleep_histogram)


#Task 7
# Create a ggplot bar chart for food categories
food_barchart <- ggplot(msleep, aes(x = vore, fill = vore)) +
  geom_bar() +
  labs(title = "Number of Mammals in Each Food Category",
       x = "Food Category",
       y = "Number of Mammals") +
  scale_fill_manual(values = c("carni" = "red", "herbi" = "green", "omni" = "blue", "insecti" = "orange", "missing" = "gray"))

# Print the bar chart
print(food_barchart)


#Task 8 
# Create a grouped boxplot for sleep_total
sleep_boxplot <- ggplot(msleep, aes(x = vore, y = sleep_total, fill = vore)) +
  geom_boxplot() +
  labs(title = "Grouped Boxplot for Sleep Time",
       x = "Food Category",
       y = "Total Sleep Time") +
  scale_fill_manual(values = c("herbi" = "green", "carni" = "red", "omni" = "blue", "insecti" = "orange", "missing" = "gray"))

# Print the boxplot
print(sleep_boxplot)


# Task 9
# Calculate the average sleep time for each food category
average_sleep_time <- msleep %>%
  group_by(vore) %>%
  summarize(average_sleep = mean(sleep_total, na.rm = TRUE))

# Find the food category with the longest average sleep time
highest_average <- average_sleep_time %>%
  filter(average_sleep == max(average_sleep)) %>%
  pull(average_sleep)

# Print the result
print(highest_average)



#Task 10
# Create a scatterplot for total sleep time vs. REM sleep time, colored by order
sleep_scatterplot <- ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = order)) +
  geom_point() +
  labs(title = "REM Sleep vs. Total Sleep",
       x = "Total Sleep Time",
       y = "REM Sleep Time") +
  scale_color_discrete(name = "Order")

# Print the scatterplot
print(sleep_scatterplot)


#Task 11
# Find the most common order
most_common_order <- msleep %>%
  count(order) %>%
  filter(n == max(n)) %>%
  pull(order)

# Filter data for the most common order
common_order_data <- msleep %>%
  filter(order == most_common_order)

# Create a scatterplot for total sleep time vs. REM sleep time for the most common order
sleep_scatterplot2 <- ggplot(common_order_data, aes(x = sleep_total, y = sleep_rem, color = order)) +
  geom_point() +
  labs(title = "REM Sleep vs. Total Sleep for the Most Common Order",
       x = "Total Sleep Time",
       y = "REM Sleep Time")

# Print the scatterplot
print(sleep_scatterplot2)







