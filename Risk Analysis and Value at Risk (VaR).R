library(quantmod)        # For financial data
library(PerformanceAnalytics) # For VaR calculation
library(ggplot2)         # For visualization
library(dplyr)           # For data manipulation
library(scales)

# Step 1: Data Collection
# Set the stock symbol and date range
symbol <- "BAC"
start_date <- "2023-01-01"
end_date <- Sys.Date()

getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)
stock_data <- get(symbol)

# Calculate daily returns
returns <- dailyReturn(Cl(stock_data), type = "log")  # Log returns
returns <- na.omit(returns)

# Step 2: VaR Calculation
# Historical VaR
historical_VaR <- VaR(returns, p = 0.95, method = "historical")
historical_VaR_value <- as.numeric(historical_VaR)

# Parametric VaR
parametric_VaR <- VaR(returns, p = 0.95, method = "gaussian")
parametric_VaR_value <- as.numeric(parametric_VaR)

# Monte Carlo Simulation (simulating 10,000 returns)
set.seed(123)
simulated_returns <- replicate(10000, {
  mean_return <- mean(returns)
  sd_return <- sd(returns)
  rnorm(length(returns), mean = mean_return, sd = sd_return)
})
monte_carlo_VaR <- apply(simulated_returns, 2, function(x) quantile(x, 0.05))

# Step 3: Backtesting VaR
# Count exceptions for backtesting
exceptions_historical <- sum(returns < historical_VaR_value)
exceptions_parametric <- sum(returns < parametric_VaR_value)

# Step 4: Visualization
# 4.1 Distribution of Returns
ggplot(returns, aes(x = daily.returns)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = historical_VaR_value, color = "red", linetype = "dashed") +
  geom_vline(xintercept = parametric_VaR_value, color = "green", linetype = "dashed") +
  labs(title = "Distribution of PLTR Returns",
       x = "Log Returns",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 4.2 VaR Estimates
vaR_data <- data.frame(
  Method = c("Historical VaR", "Parametric VaR", "Monte Carlo VaR"),
  VaR = c(historical_VaR, parametric_VaR, quantile(monte_carlo_VaR, 0.05))
)

ggplot(vaR_data, aes(x = Method, y = VaR, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(title = "Value at Risk (VaR) Estimates for PLTR",
       y = "VaR (at 95% Confidence Level)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 4.3 Backtesting Results
# Backtesting done on 100 trading days backtesting should not exceed 5
backtest_data <- data.frame(
  Method = c("Historical VaR", "Parametric VaR"),
  Exceptions = c(exceptions_historical, exceptions_parametric)
)

ggplot(backtest_data, aes(x = Method, y = Exceptions, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(title = "Backtesting Results for PLTR VaR Models",
       y = "Number of Exceptions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Print results
cat("Historical VaR at 95% confidence level:", percent(historical_VaR_value, accuracy = 0.001), "\n")
cat("Parametric VaR at 95% confidence level:", percent(parametric_VaR_value, accuracy = 0.001), "\n")
cat("Monte Carlo VaR at 95% confidence level:", percent(quantile(monte_carlo_VaR, 0.05), accuracy = 0.001), "\n")
cat("Number of exceptions (Historical VaR):", exceptions_historical, "\n")
cat("Number of exceptions (Parametric VaR):", exceptions_parametric, "\n")