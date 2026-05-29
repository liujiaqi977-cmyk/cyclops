data <- read_excel("/Users/yuchunli/Desktop/EDA/group/monthly rainfall.xlsx",
                       sheet = "Sheet1")
head(data)

# 删除Annual为空的行（1955）
data <- data %>% filter(!is.na(Annual))


summary(data$Annual)

# 最湿年份
data[which.max(data$Annual), ]

# 最干年份
data[which.min(data$Annual), ]

monthly_avg <- colMeans(data[, 2:13], na.rm = TRUE)
monthly_avg

ggplot(monthly_df, aes(x = Month, y = Rainfall, fill = Rainfall)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  ggtitle("Average Monthly Rainfall") +
  xlab("Month") +
  ylab("Rainfall (mm)") +
  theme_minimal()
theme_set(theme_minimal(base_size = 14))

# 找最湿年份
max_year_row <- data[which.max(data$Annual), ]

# 找最干年份
min_year_row <- data[which.min(data$Annual), ]

max_year_row
min_year_row

# 转成长格式方便画图
library(tidyr)

compare_years <- data %>%
  filter(Year %in% c(max_year_row$Year, min_year_row$Year)) %>%
  pivot_longer(cols = Jan:Dec,
               names_to = "Month",
               values_to = "Rainfall")

ggplot(compare_years, aes(x = Month, y = Rainfall, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Monthly Rainfall Comparison (Wettest vs Driest Year)") +
  labs(fill = "Year") +
  theme_minimal()

ggplot(compare_years, aes(x = Month, y = Rainfall, group = Year, color = factor(Year))) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Rainfall Pattern Comparison") +
  labs(color = "Year") +
  theme_minimal()

ggplot(compare_years, aes(x = factor(Year), y = Rainfall, fill = factor(Year))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  ggtitle("Distribution Comparison (Wettest vs Driest Year)") +
  xlab("Year") +
  theme_minimal()

# 最湿年份统计
summary(max_year_row[2:13])

# 最干年份统计
summary(min_year_row[2:13])

library(readxl)

rain <- read_excel("monthly rainfall.xlsx")

monthly_data <- unlist(rain[,2:13])

hist(monthly_data,
     breaks = 20,
     main = "Histogram of Monthly Rainfall in Adelaide",
     xlab = "Monthly Rainfall (mm)",
     col = "lightblue",
     border = "black")