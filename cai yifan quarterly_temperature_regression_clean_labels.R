# Quarterly Temperature Regression Analysis
# Adelaide Airport M.O. Station 023034
# Output labels use Jan-Mar, Apr-Jun, Jul-Sep, and Oct-Dec.

# 1. Read data
temp <- read.csv("adelaide_temperature_clean.csv")

# 2. Use complete analysis period
temp <- subset(temp, Year >= 1956 & Year <= 2025)

# 3. Create three-month period averages
quarter_data <- data.frame(
  Year = rep(temp$Year, times = 4),
  Period = rep(c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"), each = nrow(temp)),
  QuarterlyTemp = c(
    rowMeans(temp[, c("Jan", "Feb", "Mar")], na.rm = TRUE),
    rowMeans(temp[, c("Apr", "May", "Jun")], na.rm = TRUE),
    rowMeans(temp[, c("Jul", "Aug", "Sep")], na.rm = TRUE),
    rowMeans(temp[, c("Oct", "Nov", "Dec")], na.rm = TRUE)
  )
)

quarter_data$Period <- factor(
  quarter_data$Period,
  levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
)

# 4. Regression result for each period
periods <- levels(quarter_data$Period)
results <- data.frame(
  Period = character(),
  Slope_C_per_year = numeric(),
  Slope_C_per_decade = numeric(),
  P_value = numeric(),
  R_squared = numeric()
)

for (p in periods) {
  sub_data <- quarter_data[quarter_data$Period == p, ]
  fit <- lm(QuarterlyTemp ~ Year, data = sub_data)
  fit_sum <- summary(fit)
  slope <- coef(fit)["Year"]
  p_value <- fit_sum$coefficients["Year", "Pr(>|t|)"]
  r_squared <- fit_sum$r.squared
  results <- rbind(results, data.frame(
    Period = p,
    Slope_C_per_year = slope,
    Slope_C_per_decade = slope * 10,
    P_value = p_value,
    R_squared = r_squared
  ))
}

print(results)
write.csv(results, "temperature_quarterly_regression_results_clean_labels.csv", row.names = FALSE)
write.csv(quarter_data, "temperature_quarterly_data_clean_labels.csv", row.names = FALSE)

# 5. Create a clean labelled figure
png("quarterly_temperature_trends_clean_labels.png", width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (p in periods) {
  sub_data <- quarter_data[quarter_data$Period == p, ]
  fit <- lm(QuarterlyTemp ~ Year, data = sub_data)
  fit_sum <- summary(fit)
  slope_decade <- coef(fit)["Year"] * 10
  p_value <- fit_sum$coefficients["Year", "Pr(>|t|)"]
  plot(
    sub_data$Year,
    sub_data$QuarterlyTemp,
    main = p,
    xlab = "Year",
    ylab = "Quarterly mean maximum temperature (°C)",
    pch = 16
  )
  abline(fit, lwd = 2)
  legend(
    "topleft",
    legend = c(
      paste0("Slope = ", round(slope_decade, 3), " °C/decade"),
      paste0("p = ", signif(p_value, 4))
    ),
    bty = "n"
  )
}

dev.off()

cat("Analysis finished. Check the folder for clean labelled output files.\n")
