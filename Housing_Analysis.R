library(MASS) # Step-wise regression
library(tidyr)
library(car) # Library for VIFs
library(regclass) # Library to get VIFS
library(ggplot2) # Plotting
library(corrplot) # Correlation plot
library(EnvStats) # Box-cox Transformation
rm(list=ls())

# Read in Data ------------------------------------------------------------
housing_data <- readxl::read_xlsx(paste0(getwd(), "/data/Housing_data.xlsx"))

# Clean Data --------------------------------------------------------------
dropped_columns <- c("Date the Property was offered", "Address",
                     "Garage", "Pool")
housing_data <- housing_data[, !(names(housing_data) %in% dropped_columns)]

# Split Data ------------------------------------------------------------
set.seed(12345)
sample_size = floor(0.75 * nrow(housing_data))
indices <- sample(seq_len(nrow(housing_data)), size = sample_size)
training <- housing_data[indices,]
testing <- housing_data[-indices,]

# MLR Model --------------------------------------------------------------
housing_model <- lm(formula = Price ~. , data = training)
summary(housing_model)
fits <- housing_model$fitted.values
residuals <- rstudent(housing_model)

# Step-wise SLR Model ---------------------------------------------------------
null_model <- lm(formula = Price ~ 1, data = training)
full_model <- lm(formula = Price ~., data = training)
step_linear <- stepAIC(housing_model, direction = "both", scope = list("lower" = null_model,
                                                        "upper" = full_model))
summary(step_linear)

# MLR Model Fits vs. Residuals Plot
training %>%
  ggplot(aes(fits, residuals)) + geom_point() + ggtitle("MLR Residuals vs. Fits") + 
  xlab("Fits") + ylab("Residuals")


# Polynomial Model --------------------------------------------------------
poly_training <- training
for (column in colnames(poly_training[, 2:ncol(poly_training)])){
  poly_training[column] <- print(poly_training[column]**2)
}

poly_model <- lm(formula = Price ~ ., data = poly_training)

full_poly_model_variables <- Price ~ 
  training$`Number of Bedrooms` + poly_training$`Number of Bedrooms` +
  training$`Number of Bathrooms` + poly_training$`Number of Bathrooms`+
  training$`Number of Bedrooms` + poly_training$`Number of Bedrooms` +
  training$`Square Feet` + poly_training$`Square Feet` +
  training$`Average School Rating` + poly_training$`Square Feet` +
  training$`Number of Pictures` + poly_training$`Number of Pictures` +
  training$`Year Built` + poly_training$`Year Built`

null_poly_model <- lm(formula = Price ~ 1, data = poly_training)
full_poly_model <- lm(formula = full_poly_model_variables, data = poly_training)
step_poly <- stepAIC(null_poly_model, direction = "both", 
                     scope = list("lower" = null_poly_model, "upper" = full_poly_model))

poly_training %>%
  ggplot(aes(step_poly$fitted.values, rstudent(step_poly))) + geom_point() + 
  ggtitle("Polynomial Residuals vs. Fits") + xlab("Fits") + ylab("Residuals")


# Centered Polynomial Model -----------------------------------------------

# Center the variables that have VIFs > 10
centered_bathrooms_poly <- (poly_training$`Number of Bathrooms` - mean(poly_training$`Number of Bathrooms`))^2
centered_bathrooms_linear <- training$`Number of Bathrooms` - mean(training$`Number of Bathrooms`)

poly_training$`Number of Bathrooms` <- centered_bathrooms_poly
training$`Number of Bathrooms` <- centered_bathrooms_linear

full_poly_model_variables <- Price ~ 
  training$`Number of Bedrooms` + poly_training$`Number of Bedrooms` +
  centered_bathrooms_linear + centered_bathrooms_poly +
  training$`Number of Bedrooms` + poly_training$`Number of Bedrooms` +
  training$`Square Feet` + poly_training$`Square Feet` +
  training$`Average School Rating` + poly_training$`Square Feet` +
  training$`Number of Pictures` + poly_training$`Number of Pictures` +
  training$`Year Built` + poly_training$`Year Built`

centered_step_poly <- stepAIC(null_poly_model, direction = "both", 
                     scope = list("lower" = null_poly_model, "upper" = full_poly_model))
poly_training %>%
  ggplot(aes(centered_step_poly$fitted.values, rstudent(centered_step_poly))) + 
  geom_point() + ggtitle("Centered Polynomial Residuals vs. Fits") + xlab("Fits") + ylab("Residuals")
