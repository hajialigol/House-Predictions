library(MASS) # Step-wise regression
library(tidyr)
library(car) # Library for VIFs
library(regclass) # Library to get VIFS
library(ggplot2) # Plotting
library(corrplot) # Correlation plot
library(EnvStats) # Box-cox Transformation

# Read in Data ------------------------------------------------------------
#housing_data <- read.csv(paste0(getwd(), "/data/california_housing_data.csv"))
housing_data <- readxl::read_xlsx(paste0(getwd(), "./data/Semester_Project_data.xlsx"))

# Clean Data --------------------------------------------------------------
dropped_columns <- c("Year Built", "Date the Property was offered", "Address")
housing_data <- housing_data[, !(names(housing_data) %in% dropped_columns)]

# Split Data ------------------------------------------------------------
set.seed(12345)
sample_size = floor(0.75 * nrow(housing_data))
indices <- sample(seq_len(nrow(housing_data)), size = sample_size)
training <- housing_data[indices,]
testing <- housing_data[-indices,]

training_new <- housing_data[, c("Price", "Number of Bathrooms", "Number of Bedrooms",
                                 "Square Feet", "Number of Pictures")]

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

null_poly_model <- lm(formula = Price ~ 1, data = poly_training)
full_poly_model <- lm(formula = Price ~ ., data = poly_training)
step_poly <- stepAIC(poly_model, direction = "both", 
                     scope = list("lower" = null_poly_model, "upper" = full_poly_model))

