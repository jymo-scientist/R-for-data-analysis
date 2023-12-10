ibrary(forecast)
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Load data from the RDS file
toronto_weather <- readRDS("data.rds")
# Convert Date.Time column to POSIXct
toronto_weather$Date.Time <- as.POSIXct(toronto_weather$Date.Time, format="%Y-%m-
%d %H:%M")
# Subset data for a specific date range
start_date <- as.POSIXct("2003-05-01 00:00")
end_date <- as.POSIXct("2003-05-07 23:00")
subset_data <- subset(toronto_weather, Date.Time >= start_date & Date.Time <= end_date)
# Create a time series object
ts_data <- ts(subset_data$Temp, frequency = 24)
data_no_outliers <- ifelse(ts_data %in% outliers, median(ts_data, na.rm = TRUE), ts_data)

# Generate a quadratic trend
fit_trend <- (1:length(data_no_outliers))^2
# Fit ARIMA(0,1,1) model with trend
fit <- Arima(ts_data, order = c(1, 1,0), xreg = fit_trend)
# Generate future time points for prediction
full_length <- length(ts_data) + 24
next_time <- seq(from = as.POSIXct("2003-05-08 00:00"), to = as.POSIXct("2003-05-08 23:00"), by = "hour")
future_trend <- (1:length(next_time))^1.5
forecasted_time <- seq(from = max(next_time), to = max(next_time) + 23 * 3600, by = "hour")

# Forecast future 24 time points
forecasted_values <- forecast(fit, xreg = future_trend, h = 24)

# Use the forecasted time points
forecasted_time <- seq(from = max(next_time) + 1, to = max(next_time) + 24 * 3600, by = "hour")
# Combine the actual data and predicted values
plot_data <- data.frame(
  Date = c(next_time,forecasted_time),
  Actual = coredata(ts_data),
  Predicted = c(rep(NA, length(ts_data)), as.numeric(forecasted_values$mean[1:length(ts_data)])),
  Lower = c(rep(NA, length(ts_data)), as.numeric(forecasted_values$lower[1:length(ts_data)])),
  Upper = c(rep(NA, length(ts_data)), as.numeric(forecasted_values$upper[1:length(ts_data)]))
)

data_no_na <- na.omit(plot_data)
# Actual data for May 8, 2003
actual_data <- subset(toronto_weather, Date.Time >= as.POSIXct("2003-05-08 00:00") & Date.Time
                      <= as.POSIXct("2003-05-08 23:00"))

#filling Na values
#plot_data$Predicted[is.na(plot_data$Predicted)] <- runif(sum(is.na(plot_data$Predicted)), min = min_temp, max = max_temp)
#plot_data$Lower[is.na(plot_data$Lower)] <- mean(plot_data$Lower, na.rm = TRUE)
#plot_data$Upper[is.na(plot_data$Upper)] <- mean(plot_data$Upper, na.rm = TRUE)


# Find the end date of the blue line
end_date_blue_line <- tail(actual_data$Date.Time, 1)

# Convert Date column
plot_data$Date[!is.na(plot_data$Predicted)] <- plot_data$Date[!is.na(Predicted)] + (end_date_blue_line - min(plot_data$Date[!is.na(Predicted)]))




ggplot(
  geom_line(data = actual_data, aes(x = Date.Time, y = Temp), color = "blue",size=1) +
  geom_line(data = plot_data[!is.na(Predicted), ], aes(x = Date, y = Predicted), color = "red") +
  geom_ribbon(data = plot_data[!is.na(plot_data$Lower), ], aes(x = Date, ymin = Lower, ymax = Upper), linetype = "dashed", alpha = 0.0003, color = "green", show.legend = FALSE) +
  labs(title = "Temperature Prediction", y = "Temperature", x = "Time") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("blue", "red", "black"), name = "Temperature") +
  guides(color = guide_legend(override.aes = list(fill = NA)))


