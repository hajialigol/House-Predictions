library(MASS) # Step-wise regression
library(tidyr)
library(car) # Library for VIFs
library(regclass) # Library to get VIFS
library(ggplot2) # Plotting
library(corrplot) # Correlation plot
library(EnvStats) # Box-cox Transformation

# Read in Data ------------------------------------------------------------
housing_data <- read.csv(paste0(getwd(), "/data/house_data.csv"))

# Clean Data --------------------------------------------------------------
dropped_columns <- c("id", "date", "zipcode", "lat", "long", "sqft_living15",
                     "sqft_lot15", "sqft_basement", "sqft_above")
housing_data <- housing_data[, !(names(housing_data) %in% dropped_columns)]
housing_data$waterfront <- as.factor(housing_data$waterfront)
housing_data$grade <- as.factor(housing_data$grade)
housing_data$condition <- as.factor(housing_data$condition)
housing_data$view <- as.factor(housing_data$view)

# Split Data ------------------------------------------------------------
set.seed(12345)
sample_size = floor(0.75 * nrow(housing_data))
indices <- sample(seq_len(nrow(housing_data)), size = sample_size)
training <- housing_data[indices,]
testing <- housing_data[-indices,]

# MLR Model --------------------------------------------------------------
housing_model <- lm(formula = price ~. , data = training)
summary(housing_model)
fits <- housing_model$fitted.values
residuals <- rstudent(housing_model)

# Step-wise SLR Model ---------------------------------------------------------
null_model <- lm(formula = price ~ 1, data = training)
full_model <- lm(formula = price ~., data = training)
step_linear <- stepAIC(housing_model, direction = "both", scope = list("lower" = null_model,
                                                        "upper" = full_model))
summary(step_linear)
VIF(step_linear)

# MLR Model Fits vs. Residuals Plot
training %>%
  ggplot(aes(fits, residuals)) + geom_point() + ggtitle("MLR Residuals vs. Fits") + 
  xlab("Fits") + ylab("Residuals")

