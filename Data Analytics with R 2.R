#Part 1

# 1. Correlations

data("iris")

# Compute correlations
correlations <- cor(iris[, 1:4])

# Print the correlations
print(correlations)

# Sepal.Length has positive correlations with Petal.Length (0.8717538) and Petal.Width (0.8179411).
#Sepal.Width has negative correlations with Petal.Length (-0.4284401) and Petal.Width (-0.3661259).
#These correlations make sense in the context of the iris dataset. 
#For example, it's reasonable to expect that as the length of the sepals increases, the length and width of the petals also tend to increase.




# 2. Plot Sepal.Width against Sepal.Length

library(ggplot2)

# Create a scatterplot
plot1 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point() +
  labs(x = "Sepal Width", y = "Sepal Length") +
  ggtitle("Scatterplot of Sepal Width vs Sepal Length")

# Display the plot
print(plot1)

#The strength of the negative correlation is weak, as indicated by the correlation coefficient being close to zero. 
#This is also apparent in the scatterplot, where you can see a lot of variability in Sepal Length for a given Sepal Width.




# 3. Fit a linear model using Sepal.Width as predictor and Sepal.Length as response
model1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)

# Display a summary of the model
summary(model1)

#The estimated coefficient in the linear regression model (-0.2234) aligns with the negative correlation observed in tasks 1 and 2. 
#Both the correlation and the regression coefficient indicate a negative relationship, suggesting that as Sepal Width increases, Sepal Length tends to decrease, and vice versa.
#It's important to note that the magnitude of the coefficient in the linear regression model is larger than the correlation coefficient. 
#This is because the regression coefficient represents the change in the dependent variable (Sepal Length) for a one-unit change in the independent variable (Sepal Width), while the correlation coefficient measures the strength and direction of the linear relationship.




# 4. Setosa correlations

# Subset the iris dataset for only setosa species
setosa_data <- subset(iris, Species == "setosa")

# Compute correlations for setosa
correlations_setosa <- cor(setosa_data[, 1:4])

# Display the result
correlations_setosa

#The correlations within the setosa species are all positive, and the magnitudes are generally smaller compared to the overall correlations computed for all species in the dataset.
#These differences highlight that the relationships between variables can vary when considering specific subsets (in this case, setosa) compared to the overall dataset. 
#It makes sense because different species of iris flowers may exhibit different patterns and relationships among their features.




# 5. Plot Sepal.Width against Sepal.Length, color by species
plot2 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Width", y = "Sepal Length") +
  ggtitle("Scatterplot of Sepal Width vs Sepal Length by Species")

# Display the plot
print(plot2)

# The positive correlations within setosa are consistent with the positive trend in the scatterplot, indicating that as Sepal Width tends to increase, Sepal Length also tends to increase for setosa flowers.




# 6. Fit second model using species and Sepal.Width as predictors and Sepal.Length as response
model2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

# Display a summary of the model
summary(model2)

#Both the correlation and the regression coefficient indicate a positive relationship, suggesting that as Sepal Width increases, Sepal Length tends to increases.
#When we add the species variable to the model, the interpretation of the Sepal.Width coefficient is in the context of holding the species constant. 
#In other words, the effect of Sepal.Width on Sepal.Length is considered while taking into account the different species.
#In summary, the presence of the species variable allows us to account for the fact that different species may have different baseline values for Sepal.Length, and it allows us to estimate the specific effect of Sepal.Width within each species category.




# 7. Predict the sepal length of a setosa with a sepal width of 3.6 cm
# Create a data frame with the predictor variables
new_data <- data.frame(Sepal.Width = 3.6, Species = "setosa")

# Predict sepal length using the model
prediction <- predict(model2, newdata = new_data)

# Display the prediction
prediction

#The predicted sepal length for a setosa with a sepal width of 3.6 cm is approximately 5.144212 cm. 
#This prediction seems reasonable in the context of the model and the dataset. 
#It's within the range of sepal lengths observed for setosa flowers in the dataset.


# 8. Load the Pima Indian diabetes .csv into R 
library(readr)
diabetes_data <- read_csv("a2_diabetes.csv")


# 9. Find a good logistic regression model
library(tidyverse)

# Assuming diabetes_data is your data frame
# If your data is not a tibble, convert it to one
diabetes_data <- as_tibble(diabetes_data)

# Explore the data
summary(diabetes_data)

# Check for missing values
colSums(is.na(diabetes_data))

# Explore relationships with the outcome variable
diabetes_data %>%
  ggplot(aes(x = Glucose, y = BMI, color = factor(Outcome))) +
  geom_point() +
  labs(title = "Scatterplot of Glucose vs BMI by Diabetes Outcome")

# Fit a logistic regression model
logistic_model <- glm(Outcome ~ Pregnancies + Glucose + BMI,
                      data = diabetes_data, family = "binomial")

# Display a summary of the model
summary(logistic_model)

# Evaluate model fit using AIC
AIC(logistic_model)

# Store the final model
logistic_model

#Glucose and BMI appear to be strongly related to diabetes in this model, and the effects are reasonable based on the understanding of these variables in relation to diabetes risk.

# 10. Compute the accuracy

# Filter out observations with missing values
valid_indices <- complete.cases(diabetes_data[, c("Pregnancies", "Glucose", "BMI")])
diabetes_data_valid <- diabetes_data[valid_indices, ]

# Predict probabilities of diabetes
predicted_probabilities <- predict(logistic_model, diabetes_data_valid, type = "response")

# Convert probabilities to binary predictions using a threshold of 0.5
predicted_class <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predicted_class == diabetes_data_valid$Outcome)

# Check variable significance
variable_significance <- summary(logistic_model)$coefficients[, "Pr(>|z|)"]

# Display results
cat("Accuracy:", accuracy, "\n")
cat("All variables significant at 5% level:", all(variable_significance[2:length(variable_significance)]<0.05),"\n")


#Using the same data for both fitting the regression model and evaluating its prediction accuracy may lead to a phenomenon known as overfitting.
#Overfitting occurs when a model learns the details and noise in the training data to the extent that it negatively impacts its ability to generalize to new, unseen data.



