install.packages("ggplot2") # Installing packages
install.packages("moments")  
install.packages("lmtest")

library(ggplot2) # Loading libraries           
library(moments)
library(readr)  
library(lmtest)

data <- read_csv("C:/Users/Binu Jemima/OneDrive/Desktop/mlr3.csv")

set.seed(23323493)  # Setting the seed according to my student ID

n <- nrow(data)     # Splitting the dataset
train_index <- sample(1:n, size = 0.7 * n)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

colnames(train_data)[1] <- "y"  # Renaming the blank column header  

#EXPLORATORY DATA ANALYSIS

head(train_data)
summary(train_data)

# Plotting histograms
ggplot(train_data, aes(x = y)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Histogram for Distribution of y") +
  xlab("y") +
  theme_minimal()

ggplot(train_data, aes(x = x1)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  ggtitle("Histogram for Distribution of x1") +
  xlab("x1") +
  theme_minimal()

ggplot(train_data, aes(x = x2)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  ggtitle("Histogram for Distribution of x2") +
  xlab("x2") +
  theme_minimal()

ggplot(train_data, aes(x = x3)) +
  geom_bar(fill = "purple", color = "black") +
  ggtitle("Histogram for Distribution of x3") +
  xlab("x3") +
  ylab("Count") +
  theme_minimal()

train_data$x3 <- as.numeric(factor(train_data$x3)) # Converting x3 to numeric

cat("Skewness of x1:", skewness(train_data$x1), "\n") # Finding skewness
cat("Skewness of x2:", skewness(train_data$x2), "\n")
cat("Skewness of x3:", skewness(train_data$x3), "\n")
cat("Skewness of y:", skewness(train_data$y), "\n")

plot(train_data$x1, train_data$y,       # Plotting scatterplots
     main = "Scatterplot of x1 vs y",
     xlab = "x1", ylab = "y", col = "blue", pch = 19)
abline(lm(y ~ x1, data = train_data), col = "red", lwd = 2)

plot(train_data$x2, train_data$y,
     main = "Scatterplot of x2 vs y",
     xlab = "x2", ylab = "y", col = "blue", pch = 19)
abline(lm(y ~ x2, data = train_data), col = "red", lwd = 2)

plot(train_data$x3, train_data$y,
     main = "Scatterplot of x3 vs y",
     xlab = "x3", ylab = "y", col = "blue", pch = 19)
abline(lm(y ~ x3, data = train_data), col = "red", lwd = 2)

cor(train_data[, c("x1", "x2", "x3")]) # Correlation matrix

#DATA PREPARATION

null_values_train <- is.na(train_data) # Cleaning train data
print(paste("Number of null values:",sum(null_values_train)))
print(paste("Number of duplicated values:",sum(duplicated(train_data))))

train_data$x3 <- model.matrix(~ train_data$x3 - 1, data = train_data) # One-hot encoding

z_scores <- as.data.frame(scale(train_data)) # Finding Z-scores for outliers
z_score_outliers <- which(abs(z_scores) > 3)
cleaned_train_data <- train_data[-as.integer(z_score_outliers), ] # Removing outliers from dataset

#MODELING

training_model1 <- lm(y ~ x1 + x2 + x3, data = cleaned_train_data) # Fitting model 1
summary(training_model1)

training_model2 <- lm(y ~ x1 + x2, data = cleaned_train_data) # Fitting model 2
summary(training_model2)

# INTERPRETATION

anova(training_model1, training_model2) # Performing ANOVA test

# DIAGNOSTICS

plot(model, which = 1)  # Residuals vs Fitted plot
mean(residuals(model))  # Finding mean of the residuals of the model
bptest(training_model2) # Breusch-Pagan test
dwtest(training_model2) # Durbin-Watson test
cor(train_data)         #Correlation matrix

# EVALUATION

null_values <- is.na(test_data) # Cleaning test data
print(paste("Number of null values:",sum(null_values)))
print(paste("Number of duplicated values:",sum(duplicated(test_data))))

predictions <- predict(training_model2, newdata = test_data) # Prediction
results <- data.frame(Actual = test_data$y, Predicted = predictions)

metrics <- evaluate(test_data$y, predictions) # Calculate metrics
print(metrics)

coefficients <- coef(model)         # Finding co-efficients of the model
print(coefficients)

