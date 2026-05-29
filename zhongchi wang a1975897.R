library(readxl)
library(dplyr)
library(ggplot2)

file_path <- file.choose()

df <- read_excel(file_path, sheet = "Table001 (Page 1-2)")

month_cols <- c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec")

df2 <- df %>%
  mutate(
    month_count = rowSums(!is.na(across(all_of(month_cols)))),
    annual_mean_calc = rowMeans(across(all_of(month_cols)), na.rm = TRUE)
  )

df_full <- df2 %>%
  filter(month_count == 12)

fit <- lm(annual_mean_calc ~ Year, data = df_full)

summary(fit)

df_full <- df_full %>%
  mutate(fitted_temp = predict(fit))

fit_table <- df_full %>%
  select(Year, annual_mean_calc, fitted_temp)

print(fit_table)

write.csv(df2, "annual_mean_temperature_all_years.csv", row.names = FALSE)
write.csv(fit_table, "linear_fit_table.csv", row.names = FALSE)

ggplot(df_full, aes(x = Year, y = annual_mean_calc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Adelaide Annual Mean Temperature Trend",
    x = "Year",
    y = "Annual Mean Temperature (°C)"
  ) +
  theme_minimal()