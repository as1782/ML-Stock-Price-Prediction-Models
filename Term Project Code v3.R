# ----------------------------------------
# Imports
# ----------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(ggcorrplot)
library(leaps)
library(caret)
library(glmnet)
library(pls)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(car)
library(broom)
library(tidyverse)

# ----------------------------------------
# Data Extraction and Cleaning
# ----------------------------------------

# API Key for Data Retrieval
api_key <- "99LVOCDAXAEQO8ZT" # Replace with your Alpha Vantage API key which you can get from: https://www.alphavantage.co/support/#api-key

# Function to Fetch Stock Data
fetch_stock_data <- function(api_key, symbol) {
  url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY",
                "&symbol=", symbol, "&apikey=", api_key) # If you want full data set add "&outputsize=full" to API URL parameter for all functions
  response <- GET(url) # Sending GET request 
  data <- fromJSON(content(response, "text"), simplifyDataFrame = TRUE) # Parsing JSON response 
  # Extraction and df formation
  time_series <- data$`Time Series (Daily)` %>%
    map_dfr(~ as.data.frame(.x), .id = "date") %>%
    setNames(c("date", "open", "high", "low", "close", "volume")) %>%
    mutate(
      date = as.Date(date), 
      across(c(open, high, low, close, volume), as.numeric) # Converting coloumns to numeric 
    )
  return(time_series)
}

# Function to Fetch FX Data
fetch_fx_data <- function(api_key, from_symbol, to_symbol) {
  fx_url <- paste0("https://www.alphavantage.co/query?function=FX_DAILY",
                   "&from_symbol=", from_symbol, "&to_symbol=", to_symbol,
                   "&apikey=", api_key, "&outputsize=compact") 
  fx_response <- GET(fx_url)
  fx_data <- fromJSON(content(fx_response, "text"), simplifyDataFrame = TRUE)
  
  fx_series <- fx_data$`Time Series FX (Daily)` %>%
    map_dfr(~ as.data.frame(.x), .id = "date") %>%
    setNames(c("date", "open_fx", "high_fx", "low_fx", "close_fx")) %>%
    mutate(
      date = as.Date(date), 
      across(c(open_fx, high_fx, low_fx, close_fx), as.numeric) 
    ) %>%
    select(date, close_fx) %>%
    rename(!!paste0(from_symbol, "_to_", to_symbol) := close_fx) # Renaming FX coloumn 
  return(fx_series)
}

# Function to Fetch Commodity Data
fetch_commodity_data <- function(api_key, function_type, interval = "daily") {
  commodity_url <- paste0("https://www.alphavantage.co/query?function=", function_type,
                          "&interval=", interval, "&apikey=", api_key)
  commodity_response <- GET(commodity_url)
  commodity_data <- fromJSON(content(commodity_response, "text"), simplifyDataFrame = TRUE)
  
  commodity_series <- commodity_data$data %>%
    rename(date = `date`, value = `value`) %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    )
  return(commodity_series)
}

# Function to Fetch Treasury Yield
fetch_treasury_yield <- function(api_key, maturity = "10year", interval = "daily") {
  treasury_url <- paste0("https://www.alphavantage.co/query?function=TREASURY_YIELD",
                         "&maturity=", maturity, "&interval=", interval, "&apikey=", api_key)
  treasury_response <- GET(treasury_url)
  treasury_data <- fromJSON(content(treasury_response, "text"), simplifyDataFrame = TRUE)
  
  treasury_series <- treasury_data$data %>%
    rename(date = `date`, value = `value`) %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    )
  return(treasury_series)
}

# Function to Fetch Federal Funds Rate
fetch_ffr_data <- function(api_key, interval = "daily") {
  ffr_url <- paste0("https://www.alphavantage.co/query?function=FEDERAL_FUNDS_RATE",
                    "&interval=", interval, "&apikey=", api_key)
  ffr_response <- GET(ffr_url)
  ffr_data <- fromJSON(content(ffr_response, "text"), simplifyDataFrame = TRUE)
  
  ffr_series <- ffr_data$data %>%
    rename(date = `date`, value = `value`) %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    )
  return(ffr_series)
}

# Fetch Stock Data for Apple
symbol <- "AAPL" # Input stock ticker symbol
time_series <- fetch_stock_data(api_key, symbol)

# Log Transform for Volume
time_series <- time_series %>%
  mutate(log_volume = log(volume)) # Adding column to time_series df

# Fetch FX, Commodity, and Economic Data
eur_usd <- fetch_fx_data(api_key, "EUR", "USD")
usd_cny <- fetch_fx_data(api_key, "USD", "CNY")
usd_jpy <- fetch_fx_data(api_key, "USD", "JPY")
wti_data <- fetch_commodity_data(api_key, "WTI") %>% rename(wti_price = value)
copper_data <- fetch_commodity_data(api_key, "COPPER", interval = "monthly") %>% rename(copper_price = value)
treasury_yield_data <- fetch_treasury_yield(api_key) %>% rename(treasury_yield = value)
ffr_data <- fetch_ffr_data(api_key) %>% rename(ffr_rate = value)

# Merge All Data into the Main Dataset
time_series <- time_series %>%
  left_join(eur_usd, by = "date") %>%
  left_join(usd_cny, by = "date") %>%
  left_join(usd_jpy, by = "date") %>%
  left_join(wti_data, by = "date") %>%
  left_join(copper_data, by = "date") %>%
  left_join(treasury_yield_data, by = "date") %>%
  left_join(ffr_data, by = "date")

# Handle Missing Values
time_series <- time_series %>%
  arrange(date) %>% # Ordering data by date 
  fill(everything(), .direction = "down") %>% # Filling missing values with last observed values for time series 
  drop_na() # Dropping any rows with NA values

# Adding Lagged Variables
time_series <- time_series %>%
  arrange(date) %>%
  mutate(
    open_lag1 = lag(open, n = 1),
    close_lag1 = lag(close, n = 1)
  ) %>%
  drop_na() # Dropping NA values due to lags 

# Include SMA (10-Day Moving Average)
sma_function <- "SMA" # Moving avg function 
interval <- "daily" # Data interval
time_period <- 10 # No. of periods for SMA
series_type <- "close" # SMA applied to closing prices

sma_url <- paste0(
  "https://www.alphavantage.co/query?function=", sma_function,
  "&symbol=", symbol, "&interval=", interval,
  "&time_period=", time_period, "&series_type=", series_type,
  "&apikey=", api_key
)

sma_response <- GET(sma_url)
sma_data <- fromJSON(content(sma_response, "text"), simplifyDataFrame = TRUE)

# Parse SMA data
sma_series <- sma_data$`Technical Analysis: SMA` %>%
  map_dfr(~ as.data.frame(.x), .id = "date") %>%
  transmute(
    date = as.Date(date),  # Converting date column to Date type    
    SMA_10 = as.numeric(SMA) # Converting SMA values to numeric
  )

# Merge SMA_10 with the time_series data
time_series <- time_series %>%
  left_join(sma_series, by = "date") %>%
  filter(!is.na(SMA_10)) # Removing rows with NA values in NA columns 

# ----------------------------------------
# Exploratory Data Analysis
# ----------------------------------------

# Summary Statistics
cat("Summary Statistics:\n")
summary(time_series)

# Distribution Analysis: Closing Prices
ggplot(time_series, aes(x = close)) +
  geom_histogram(binwidth = 2.0, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Closing Prices", x = "Closing Price", y = "Frequency") +
  theme_minimal()

# Calculate Correlation Matrix
numeric_data <- time_series %>% select_if(is.numeric) # Selecting only numeric coloumns 
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot Correlation Matrix using ggcorrplot
ggcorrplot(cor_matrix, method = "circle", lab = TRUE, # Circle method with labels for correlations 
           title = "Correlation Matrix of Variables", lab_size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# Scatter Plot Analysis: High v.s. Close
ggplot(time_series, aes(x = high, y = close)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Scatterplot: High Price vs Close Price", x = "High Price", y = "Close Price") +
  theme_minimal()

# Scatter Plot Analysis: WTI v.s. Close
ggplot(time_series, aes(x = wti_price, y = close)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Scatterplot: WTI Price vs Close Price", x = "WTI Price", y = "Close Price") +
  theme_minimal()

# Scatter Plot Analysis: Treasury Yield v.s. Close
ggplot(time_series, aes(x = treasury_yield, y = close)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Scatterplot: Treasury Yield vs Close Price", x = "Treasury Yield", y = "Close Price") +
  theme_minimal()

# Time Series Analysis: Closing Prices Over Time
ggplot(time_series, aes(x = date, y = close)) +
  geom_line(color = "darkblue") +
  labs(title = "Time Series: Daily Closing Prices", x = "Date", y = "Close Price") +
  theme_minimal()

# Time Series Analysis: WTI Crude Oil Prices Over Time
ggplot(time_series, aes(x = date, y = wti_price)) +
  geom_line(color = "darkblue") +
  labs(title = "Time Series: Daily WTI Crude Oil Prices", x = "Date", y = "WTI Crude Oil Price") +
  theme_minimal()

# Time Series Analysis: 10-Year Treasury Yield Over Time
ggplot(time_series, aes(x = date, y = treasury_yield)) +
  geom_line(color = "darkblue") +
  labs(title = "Time Series: 10-Year Treasury Yield Over Time", x = "Date", y = "10-Year Treasury Yield") +
  theme_minimal()

# ----------------------------------------
# Multiple Linear Regression
# ----------------------------------------

# Splitting the Data
set.seed(123) 
train_indices <- sample(1:nrow(time_series), size = 0.8 * nrow(time_series)) # Adjust training split
train_data <- time_series[train_indices, ]
test_data <- time_series[-train_indices, ]

# Fitting the MLR Model
lm_model <- lm(close ~ open + high + low + log_volume + EUR_to_USD + USD_to_CNY +
                 USD_to_JPY + wti_price + copper_price + treasury_yield + ffr_rate +
                 open_lag1 + close_lag1 + SMA_10, data = train_data)

# Model Summary
summary(lm_model)

# Predictions and MSE Calculation
mlr_predictions <- predict(lm_model, newdata = test_data)
mlr_mse <- mean((test_data$close - mlr_predictions)^2) # Computing avg squared difference between actual and predicted 
cat("Multiple Linear Regression MSE:", mlr_mse, "\n")

# ----------------------------------------
# Residual Diagnostics
# ----------------------------------------

# Extract diagnostic data
diagnostics <- augment(lm_model) # df of fitted values, residuals, and leverage stats

# Residuals vs Fitted Values
ggplot(diagnostics, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values (MLR)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Residuals vs Time
ggplot(diagnostics, aes(x = train_data$date, y = .resid)) +
  geom_line(color = "darkblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Time (MLR)", x = "Time", y = "Residuals") +
  theme_minimal()

# Histogram of Residuals
ggplot(diagnostics, aes(x = .resid)) +
  geom_histogram(binwidth = 0.3, fill = "steelblue", alpha = 0.7) +
  labs(title = "Histogram of Residuals (MLR)", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Q-Q Plot of Residuals
ggplot(diagnostics, aes(sample = .resid)) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot of Residuals (MLR)", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Outlier Detection: Studentized Residuals > 3
diagnostics <- diagnostics %>%
  mutate(outlier = ifelse(abs(.std.resid) > 3, "Yes", "No"))
cat("Outliers (Studentized Residuals > 3):\n")
print(diagnostics %>% filter(outlier == "Yes"))

# Leverage and High-Leverage Points Diagnostics 
p <- length(coef(lm_model)) - 1  
n <- nrow(train_data)           
average_leverage <- (p + 1) / n

diagnostics <- diagnostics %>%
  mutate(HighLeverage = ifelse(.hat > 2 * average_leverage, "Yes", "No"))
cat("Average Leverage:", round(average_leverage, 4), "\n")
cat("High Leverage Points:\n")
print(diagnostics %>% filter(HighLeverage == "Yes")) # Note these points to highlight in plot below

# Residuals vs Leverage Plot with Highlighted and Labeled Points
ggplot(diagnostics, aes(x = .fitted, y = .std.resid)) +  
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_point(
    data = diagnostics %>% filter(.rownames %in% c("3", "34")), # Highlight points based on diagnostics
    color = "red") +
  geom_text(
    data = diagnostics %>% filter(.rownames %in% c("3", "34")), # Label highlighted points 
    aes(label = .rownames), color = "red", vjust = -1, hjust = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Studentized Residuals vs Fitted Values (MLR)", 
       x = "Fitted Values", y = "Studentized Residuals") +
  theme_minimal()

# ACF Plot of Residuals
acf_values <- acf(diagnostics$.resid, plot = FALSE)  # ACF without base R plot
acf_df <- data.frame(Lag = acf_values$lag[-1], ACF = acf_values$acf[-1]) # Excluding lag 0

ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", position = "dodge", fill = "darkblue", width = 0.2) +
  geom_hline(yintercept = c(-1.96 / sqrt(n), 1.96 / sqrt(n)), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "ACF of Residuals (MLR)", x = "Lag", y = "ACF") +
  theme_minimal()

# VIF to check for multicollinearity
vif_values <- car::vif(lm_model)
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)

ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +  
  coord_flip() +
  labs(title = "Variance Inflation Factor (VIF)", x = "Variable", y = "VIF") +
  theme_minimal()

# Removing 'date' column from both datasets
train_data <- train_data %>% select(-date) 
test_data <- test_data %>% select(-date)

# ----------------------------------------
# Subset Selection
# ----------------------------------------

# Forward Stepwise Selection
set.seed(123)
forward_model <- train(close ~ ., data = train_data, method = "leapForward", 
                       trControl = trainControl(method = "cv", number = 10), # 10-fold CV 
                       tuneGrid = expand.grid(nvmax = 1:(ncol(train_data) - 1))) # No. of predictors to evaluate 

# Backward Stepwise Selection
set.seed(123)
backward_model <- train(close ~ ., data = train_data, method = "leapBackward", 
                        trControl = trainControl(method = "cv", number = 10), 
                        tuneGrid = expand.grid(nvmax = 1:(ncol(train_data) - 1)))

# Extract Cross-Validation Errors for Forward Selection
forward_cv_errors <- data.frame(
  Predictors = forward_model$results$nvmax, # No. of predictors in each subset 
  CV_Error = forward_model$results$RMSE # Corresponding CV error (RMSE)
)

# Extract Cross-Validation Errors for Backward Selection
backward_cv_errors <- data.frame(
  Predictors = backward_model$results$nvmax,
  CV_Error = backward_model$results$RMSE
)

# Plot Cross-Validation Error for Forward Selection
ggplot(forward_cv_errors, aes(x = Predictors, y = CV_Error)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Forward Stepwise Selection: Cross-Validation Error",
       x = "Number of Predictors",
       y = "Cross-Validation Error") +
  theme_minimal()

# Plot Cross-Validation Error for Backward Selection
ggplot(backward_cv_errors, aes(x = Predictors, y = CV_Error)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Backward Stepwise Selection: Cross-Validation Error",
       x = "Number of Predictors",
       y = "Cross-Validation Error") +
  theme_minimal()

# Best Subset Size
cat("Forward Stepwise Best Subset Size:", forward_model$bestTune$nvmax, "\n") # Optimal subset size for fwd
cat("Backward Stepwise Best Subset Size:", backward_model$bestTune$nvmax, "\n") # Optimal subset size for bck

# Extract Best Variables for Forward Selection
selected_variables_forward <- names(coef(forward_model$finalModel, id = forward_model$bestTune$nvmax))[-1] # Excluding intercept
forward_best_formula <- as.formula(paste("close ~", paste(selected_variables_forward, collapse = " + ")))

# Fit the Best Forward Model
forward_final_model <- lm(forward_best_formula, data = train_data) # Linear model with best subset from fwd selection 

# Extract Best Variables for Backward Selection
selected_variables_backward <- names(coef(backward_model$finalModel, id = backward_model$bestTune$nvmax))[-1] 
backward_best_formula <- as.formula(paste("close ~", paste(selected_variables_backward, collapse = " + ")))

# Fit the Best Backward Model
backward_final_model <- lm(backward_best_formula, data = train_data) # Linear model with best subset from bck selection 

# Predictions and MSE Calculation
forward_predictions <- predict(forward_final_model, newdata = test_data)
backward_predictions <- predict(backward_final_model, newdata = test_data)

forward_mse <- mean((test_data$close - forward_predictions)^2)
backward_mse <- mean((test_data$close - backward_predictions)^2)

# Output MSEs
cat("Forward Stepwise MSE:", forward_mse, "\n")
cat("Backward Stepwise MSE:", backward_mse, "\n")

# Summarize Models
summary(forward_final_model)
summary(backward_final_model)

# ----------------------------------------
# Ridge and Lasso Regression
# ----------------------------------------

# Define response variables
y_train <- train_data$close # Response variable for training data 
y_test <- test_data$close # Response variable for test data

# Define predictor matrices for train and test data
x_train <- model.matrix(close ~ ., data = train_data)[, -1]  # Matrix of predictors for training data
x_test <- model.matrix(close ~ ., data = test_data)[, -1]    # Matrix of predictors for test data 

# Ridge Regression
set.seed(123)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, lambda = 10^seq(4, -2, length = 100), standardize = TRUE) # Specifying ridge with log sequence of lambda values + standardizing 
ridge_lambda <- ridge_model$lambda.min # Optimal lamba that minimises CV error
ridge_predictions <- predict(ridge_model, s = ridge_lambda, newx = x_test) # Predicting on test data using optimal lambda
ridge_mse <- mean((y_test - ridge_predictions)^2) # Calculating MSE
cat("Ridge Regression MSE:", ridge_mse, "\n")

# Lasso Regression
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, lambda = 10^seq(4, -2, length = 100), standardize = TRUE)
lasso_lambda <- lasso_model$lambda.min
lasso_predictions <- predict(lasso_model, s = lasso_lambda, newx = x_test)
lasso_mse <- mean((y_test - lasso_predictions)^2)
cat("Lasso Regression MSE:", lasso_mse, "\n")

# ----------------------------------------
# Ridge and Lasso Regression: Coefficient Paths and Validation Errors
# ----------------------------------------

# Extract Ridge Coefficients Across All Lambdas
ridge_lambda <- ridge_model$lambda  # Vector of lambda values
ridge_coefs <- do.call(rbind, lapply(1:length(ridge_lambda), function(i) {
  coefs <- as.vector(coef(ridge_model$glmnet.fit, s = ridge_lambda[i])) # Extracting coeff for each lambda
  data.frame(Lambda = ridge_lambda[i], Predictor = rownames(coef(ridge_model$glmnet.fit)), Coefficient = coefs)
}))

# Remove Intercept
ridge_coefs <- ridge_coefs %>% filter(Predictor != "(Intercept)")

# Plot Ridge Coefficient Path
ggplot(ridge_coefs, aes(x = log10(Lambda), y = Coefficient, color = Predictor)) +
  geom_line(size = 1) +
  labs(title = "Ridge Regression: Coefficient Path",
       x = "Log(λ)", y = "Standardized Coefficient") +
  theme_minimal()

# Extract Lasso Coefficients Across All Lambdas
lasso_lambda <- lasso_model$lambda  # Vector of lambda values
lasso_coefs <- do.call(rbind, lapply(1:length(lasso_lambda), function(i) {
  coefs <- as.vector(coef(lasso_model$glmnet.fit, s = lasso_lambda[i]))
  data.frame(Lambda = lasso_lambda[i], Predictor = rownames(coef(lasso_model$glmnet.fit)), Coefficient = coefs)
}))

# Remove Intercept
lasso_coefs <- lasso_coefs %>% filter(Predictor != "(Intercept)")

# Plot Lasso Coefficient Path
ggplot(lasso_coefs, aes(x = log10(Lambda), y = Coefficient, color = Predictor)) +
  geom_line(size = 1) +
  labs(title = "Lasso Regression: Coefficient Path",
       x = "Log(λ)", y = "Standardized Coefficient") +
  theme_minimal()

# Extract lambda, cross-validation error, and standard errors
ridge_results <- data.frame(
  log_lambda = log10(ridge_model$lambda),
  cv_error = ridge_model$cvm,
  cv_se = ridge_model$cvsd
)

# Find the minimum MSE and corresponding lambda
min_index <- which.min(ridge_results$cv_error) # Index of the minimum MSE 
optimal_log_lambda <- ridge_results$log_lambda[min_index] # Log lambda for optimal lambda 

# Ridge Validation Plot
ggplot(ridge_results, aes(x = log_lambda, y = cv_error)) +
  geom_point(color = "red", size = 2) + # Red points for errors
  geom_errorbar(aes(ymin = cv_error - cv_se, ymax = cv_error + cv_se), 
                color = "grey", width = 0.1) + # Error bars
  geom_vline(xintercept = optimal_log_lambda, linetype = "dashed", color = "blue") + 
  annotate("text", x = optimal_log_lambda + 0.3, 
           y = max(ridge_results$cv_error) * 0.9, 
           label = paste0("Optimal λ = ", round(10^optimal_log_lambda, 4)),
           color = "blue", size = 4, hjust = 0) + # Annotation
  labs(title = "Ridge Regression: Validation Error vs Lambda",
       x = expression(Log(lambda)), 
       y = "Mean-Squared Error") +
  theme_minimal()

# Extract lambda, cross-validation error, and standard errors for Lasso
lasso_results <- data.frame(
  log_lambda = log10(lasso_model$lambda),
  cv_error = lasso_model$cvm,
  cv_se = lasso_model$cvsd
)

# Find the minimum MSE and corresponding lambda
min_index_lasso <- which.min(lasso_results$cv_error)
optimal_log_lambda_lasso <- lasso_results$log_lambda[min_index_lasso]

# Lasso Validation Plot
ggplot(lasso_results, aes(x = log_lambda, y = cv_error)) +
  geom_point(color = "red", size = 2) + # Red points for errors
  geom_errorbar(aes(ymin = cv_error - cv_se, ymax = cv_error + cv_se), 
                color = "grey", width = 0.1) + # Error bars
  geom_vline(xintercept = optimal_log_lambda_lasso, linetype = "dashed", color = "blue") +
  annotate("text", x = optimal_log_lambda_lasso + 0.3, 
           y = max(lasso_results$cv_error) * 0.9, 
           label = paste0("Optimal λ = ", round(10^optimal_log_lambda_lasso, 4)),
           color = "blue", size = 4, hjust = 0) + 
  labs(title = "Lasso Regression: Validation Error vs Lambda",
       x = expression(Log(lambda)), 
       y = "Mean-Squared Error") +
  theme_minimal()

# ----------------------------------------
# PCR and PLS Regression
# ----------------------------------------

# PCR
set.seed(123)
pcr_model <- pcr(close ~ ., data = train_data, scale = TRUE, validation = "CV") # Fitting PCR with CV to get optimal no. of comps + standardization 
optimal_pcr_components <- which.min(pcr_model$validation$PRESS) # Selecting optimal no. of comps with lowest PREES
pcr_predictions <- predict(pcr_model, test_data, ncomp = optimal_pcr_components) # Making predictions on test data using optimal no. of comps
pcr_mse <- mean((y_test - pcr_predictions)^2)
cat("PCR MSE:", pcr_mse, "\n")

# PLS
set.seed(123)
pls_model <- plsr(close ~ ., data = train_data, scale = TRUE, validation = "CV")
optimal_pls_components <- which.min(pls_model$validation$PRESS)
pls_predictions <- predict(pls_model, test_data, ncomp = optimal_pls_components)
pls_mse <- mean((y_test - pls_predictions)^2)
cat("PLS MSE:", pls_mse, "\n")

# ----------------------------------------
# PCR and PLS Regression: Validation Plots
# ----------------------------------------

# Extract validation results for PCR
pcr_msep <- data.frame(
  n_components = 1:length(pcr_model$validation$PRESS), # No. of comps evaluated
  MSEP = pcr_model$validation$PRESS, # PRESS for each comp 
  row.names = NULL  # Reset row names
)

# Reshape the data into long format
pcr_msep_long <- pcr_msep %>%
  pivot_longer(
    cols = starts_with("MSEP"),         # Including all columns that represent MSEP data  
    names_to = "components",            # Name for reshaped columns
    values_to = "MSEP"                  # New column for MSEP
  ) %>%
  mutate(components = as.numeric(gsub("MSEP\\.|\\.comps", "", components)))  # Extracting numeric component values

# Find the optimal number of components
optimal_pcr_components <- pcr_msep_long$components[which.min(pcr_msep_long$MSEP)] # Component with minimum MSEP 

# Create the PCR Validation Plot
ggplot(pcr_msep_long, aes(x = components, y = MSEP)) +
  geom_line(color = "red", size = 1) +                      
  geom_point(color = "blue", size = 2) +                    
  geom_vline(xintercept = optimal_pcr_components,            
             linetype = "dashed", color = "blue", size = 1) +
  annotate("text", x = optimal_pcr_components + 1,           
           y = max(pcr_msep_long$MSEP) * 0.9,
           label = paste("Optimal =", optimal_pcr_components),
           color = "blue", size = 4, hjust = 0) +
  labs(title = "PCR Validation Plot: Mean Squared Prediction Error",
       x = "Number of Components",
       y = "Mean Squared Prediction Error (MSEP)") +
  theme_minimal()

# Extract validation results for PLS
pls_msep <- data.frame(
  n_components = 1:length(pls_model$validation$PRESS),  
  MSEP = pls_model$validation$PRESS,
  row.names = NULL  # Reset row names
)

# Reshape the data into long format
pls_msep_long <- pls_msep %>%
  pivot_longer(
    cols = starts_with("MSEP"),          
    names_to = "components",            
    values_to = "MSEP"                   
  ) %>%
  mutate(components = as.numeric(gsub("MSEP\\.|\\.comps", "", components)))  

# Find the optimal number of components
optimal_pcr_components <- pls_msep_long$components[which.min(pls_msep_long$MSEP)]

# PLS Validation Plot
ggplot(pls_msep_long, aes(x = components, y = MSEP)) +
  geom_line(color = "red", size = 1) +                      
  geom_point(color = "blue", size = 2) +                    
  geom_vline(xintercept = optimal_pls_components,            
             linetype = "dashed", color = "blue", size = 1) +
  annotate("text", x = optimal_pls_components + 1,          
           y = max(pls_msep_long$MSEP) * 0.9,
           label = paste("Optimal =", optimal_pls_components),
           color = "blue", size = 4, hjust = 0) +
  labs(title = "PLS Validation Plot: Mean Squared Prediction Error",
       x = "Number of Components",
       y = "Mean Squared Prediction Error (MSEP)") +
  theme_minimal()

# ----------------------------------------
# Tree-Based Models: Regression Tree, Bagging, Random Forest and Boosting
# ----------------------------------------

# Regression Tree
set.seed(123)
regression_tree_model <- rpart(close ~ ., data = train_data, method = "anova", control = rpart.control(cp = 0.01)) # Fitting regression tree with cp to control overfitting 

# Cross-Validation Results for Regression Tree
printcp(regression_tree_model)

# Extract the CP table from the regression tree model
cp_table <- as.data.frame(regression_tree_model$cptable) # Converting cptable to df 

# Create a ggplot version of the CP plot
ggplot(cp_table, aes(x = nsplit + 1, y = xerror)) +
  geom_line(color = "red", size = 1) +            
  geom_point(color = "blue", size = 2) +         
  geom_vline(xintercept = which.min(cp_table$xerror), 
             linetype = "dashed", color = "blue", size = 1) + 
  labs(title = "Regression Tree: Cross-Validation Results",
       x = "Number of Splits",
       y = "Cross-Validation Error (xerror)") +
  theme_minimal()

# Prune the Tree Based on Optimal CP
optimal_cp <- regression_tree_model$cptable[which.min(regression_tree_model$cptable[,"xerror"]), "CP"]
pruned_tree_model <- prune(regression_tree_model, cp = optimal_cp) # Prune tree at optimal cp 

# Predictions and MSE for Pruned Regression Tree
tree_predictions <- predict(pruned_tree_model, newdata = test_data)
pruned_tree_mse <- mean((y_test - tree_predictions)^2)
cat("Pruned Regression Tree MSE:", pruned_tree_mse, "\n")

# Pruned Regression Tree Visualiation 
rpart.plot(pruned_tree_model, main = "Pruned Regression Tree: Visualization")

# Bagging
set.seed(123)
bagging_model <- randomForest(close ~ ., data = train_data, mtry = ncol(train_data) - 1, ntree = 500) # Bagging with mtry = no. of predictors, ntrees = 500
bagging_predictions <- predict(bagging_model, newdata = test_data)
bagging_mse <- mean((y_test - bagging_predictions)^2)

# OOB Error
cat("OOB Error for Bagging:", mean(bagging_model$mse), "\n")
cat("Bagging MSE:", bagging_mse, "\n")

# Error Rates (Tree-wise MSE)
error_rates <- data.frame(
  Trees = 1:length(bagging_model$mse),
  OOB = bagging_model$mse
)

# Plotting OOB Error vs. Number of Trees
ggplot(error_rates, aes(x = Trees, y = OOB)) +
  geom_line(color = "darkgreen") +
  labs(title = "Error Reduction with Number of Trees: Bagging", x = "Number of Trees", y = "OOB Error") +
  theme_minimal()

# Bagging: Relative Influence of Variables
bagging_importance <- data.frame(
  Variable = rownames(bagging_model$importance),
  Importance = bagging_model$importance[, "IncNodePurity"]
)

# Plotting the Relative Influence
ggplot(bagging_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Relative Influence of Variables: Bagging", x = "Variables", y = "Relative Influence") +
  theme_minimal()

# Random Forest
set.seed(123)
rf_model <- randomForest(close ~ ., data = train_data, mtry = sqrt(ncol(train_data) - 1), ntree = 500) # Random forest with mtry = sqrt(predictors), ntree = 500 
rf_predictions <- predict(rf_model, newdata = test_data)
rf_mse <- mean((y_test - rf_predictions)^2)
cat("Random Forest MSE:", rf_mse, "\n")

# OOB Error
cat("OOB Error for Random Forest:", mean(rf_model$mse), "\n")

# OOB Error Rates
oob_error_rates <- data.frame(
  Trees = 1:length(rf_model$mse),
  OOB = rf_model$mse
)

# Plotting OOB Error
ggplot(oob_error_rates, aes(x = Trees, y = OOB)) +
  geom_line(color = "darkgreen") +
  labs(title = "Error Reduction with Number of Trees: Random Forest",
       x = "Number of Trees", y = "OOB Error") +
  theme_minimal()

# Random Forest Variable Importance
rf_importance <- data.frame(
  Variable = rownames(rf_model$importance),
  Importance = rf_model$importance[, "IncNodePurity"]
)

ggplot(rf_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Relative Influence of Variable: Random Forest",
       x = "Variables", y = "Relative Influence") +
  theme_minimal()

# Boosting
set.seed(123)
boosting_model <- gbm(close ~ ., data = train_data, distribution = "gaussian", 
                      n.trees = 5000, interaction.depth = 4, shrinkage = 0.01, cv.folds = 10) # Boosting with Gaussian distribution,
                                                                                              # 5000 trees, depth = 4, learning rate = 0.01, and 10-fold CV


# Optimal no. of trees
optimal_trees <- gbm.perf(boosting_model, method = "cv", plot.it = FALSE)
cat("Optimal number of trees:", optimal_trees, "\n")

# Predictions and MSE
boosting_predictions <- predict(boosting_model, newdata = test_data, n.trees = optimal_trees)
boosting_mse <- mean((y_test - boosting_predictions)^2)
cat("Boosting MSE:", boosting_mse, "\n")

# CV Error Rates (Tree-wise Error)
cv_error_rates <- data.frame(
  Trees = 1:boosting_model$n.trees,
  CV_Error = boosting_model$cv.error
)

# Plotting CV Error vs. Number of Trees
ggplot(cv_error_rates, aes(x = Trees, y = CV_Error)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Error Reduction with Number of Trees: Boosting",
    x = "Number of Trees", 
    y = "Cross-Validation Error"
  ) +
  theme_minimal()

# Boosting: Relative Influence of Variables
relative_influence <- summary(boosting_model, n.trees = optimal_trees, plot = FALSE)
ggplot(relative_influence, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Relative Influence of Variables: Boosting", x = "Variables", y = "Relative Influence") +
  theme_minimal()
