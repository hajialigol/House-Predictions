library(MASS) # Library for 
library(tidyr)
library(car) # Library for VIFs
library(regclass) # Library to get VIFS
library(ggplot2) # Plotting
library(EnvStats) # Box-cox Transformation


# Read in and Split Data ------------------------------------------------------------
housing_data <- read.csv(paste0(getwd(), "/data/house_data.csv"))
set.seed(12345)
sample_size = 0.70
indices <- sample(seq_len(nrow(housing_data)), size = sample_size)


# Clean Data --------------------------------------------------------------
dropped_columns <- c("id", "date", "zipcode", "lat", "long", "sqft_living15", "sqft_lot15")
housing_data <- housing_data[, !(names(housing_data) %in% dropped_columns)]


# MLR Model --------------------------------------------------------------
housing_model <- lm(formula = price ~., data = housing_data)
summary(housing_model)
fits <- housing_model$fitted.values
residuals <- rstudent(housing_model)


# MLR Model Fits vs. Residuals Plot
housing_data %>%
  ggplot(aes(fits, residuals)) + geom_point() + ggtitle("MLR Residuals vs. Fits") + 
  xlab("Fits") + ylab("Residuals")

vif(housing_model)


          