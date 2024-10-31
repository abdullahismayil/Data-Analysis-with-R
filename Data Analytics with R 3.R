#Inserting Data
library(readr)
house_data <- read_csv("real-estate-taiwan.csv")
head(house_data)
#==============================================================================#

#EDA


#Scatterplots and Correlation Matrix
library(ggplot2)

# Scatter plot for house_age vs. house_price
ggplot(house_data, aes(x = house_age, y = house_price)) +
  geom_point() +
  labs(title = "Scatter Plot: House Age vs. House Price",
       x = "House Age (years)",
       y = "House Price (10000 NTD/Ping)")
# Scatter plot for mrt_distance vs. house_price
ggplot(house_data, aes(x = mrt_distance, y = house_price)) +
  geom_point() +
  labs(title = "Scatter Plot: MRT Distance vs. House Price",
       x = "MRT Distance (meters)",
       y = "House Price (10000 NTD/Ping)")

# Scatter plot for convenience_stores vs. house_price
ggplot(house_data, aes(x = convenience_stores, y = house_price)) +
  geom_point() +
  labs(title = "Scatter Plot: Convenience Stores vs. House Price",
       x = "Number of Convenience Stores",
       y = "House Price (10000 NTD/Ping)")

# Scatter plot for latitude vs. house_price
ggplot(house_data, aes(x = latitude, y = house_price)) +
  geom_point() +
  labs(title = "Scatter Plot: Latitude vs. House Price",
       x = "Latitude (degrees)",
       y = "House Price (10000 NTD/Ping)")

#Correlation Matrix
cor_matrix <- cor(house_data[, c("house_age", "mrt_distance", "convenience_stores", "latitude", "longitude", "house_price")])

# Display correlation matrix
cor_matrix

#Correlation Matrix as Plot
library(corrplot)

# Compute the correlation matrix
cor_matrix <- cor(house_data[, c("house_age", "mrt_distance", "convenience_stores", "latitude", "longitude", "house_price")])

# Plot the correlation matrix as a heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.7)

#======================================================================================================================#

#Regression Modeling


# Fit a linear regression model
model <- lm(house_price ~ house_age + mrt_distance + convenience_stores + latitude + longitude, data = house_data)

# Display the summary of the model
summary(model)


# Extract coefficients and p-values
coefficients_table <- data.frame(
  Predictor = names(coef(model))[-1],
  Estimated_Coefficient = coef(model)[-1],
  P_Value = summary(model)$coefficients[-1, 4]
)


# Display the coefficients table
print(coefficients_table)

#=====================================================================================================================#

#New model

# Filter data for properties newer than 20 years
newer_than_20_years <- subset(house_data, house_age < 20)

# Split the newer_than_20_years data into training and testing sets
set.seed(313) 
splitIndexNew <- sample(1:nrow(newer_than_20_years), 0.7 * nrow(newer_than_20_years))
train_data_new <- newer_than_20_years[splitIndexNew, ]
test_data_new <- newer_than_20_years[-splitIndexNew, ]

# Build a Linear Regression Model for newer properties
linear_model_new <- lm(house_price ~ house_age + mrt_distance + convenience_stores + latitude, data = train_data_new)

# Display the summary of the model for newer properties
summary(linear_model_new)

# Evaluate the Model on the Test Set for newer properties
predictions_new <- predict(linear_model_new, newdata = test_data_new)

# Evaluate the model's performance for newer properties
performance_metrics_new <- data.frame(
  RMSE = sqrt(mean((test_data_new$house_price - predictions_new)^2)),
  R_squared = cor(predictions_new, test_data_new$house_price)^2
)

# Display the performance metrics for newer properties
print(performance_metrics_new)

#------------------------------------------------------------------------------------#
# VIF
library(car)
vif_values <- vif(linear_model_new)

# Rename convenience_stores to a shorter label
names(vif_values)[names(vif_values) == "convenience_stores"] <- "con_store"

# Define colors for each bar
bar_colors <- c("skyblue", "coral", "palegreen", "lightpink")

# Create a barplot with different colors
barplot(vif_values, main = "VIF Values", ylab = "VIF", col = bar_colors, border = "black", ylim = c(0, max(vif_values) + 1))

# Add text labels with exact values to the bars
text(1:length(vif_values), vif_values + 0.1, sprintf("%.2f", vif_values), pos = ifelse(vif_values < max(vif_values) - 1, 3, 1), col = "black", cex = 0.8)

























