library(tidyverse)
library(readxl)

final_folder <- "C:/Users/liuji/Desktop/data/outputs/final_plots"
dir.create(final_folder, recursive = TRUE, showWarnings = FALSE)

rainfall <- read_excel(
  "C:/Users/liuji/Desktop/data/adelaide_airport_rainfall_023034.xlsx",
  sheet = "Monthly Rainfall"
)

rainfall_30 <- rainfall %>%
  filter(Year >= 1995, Year <= 2025)

average_rainfall <- mean(rainfall_30$Annual, na.rm = TRUE)

annual_plot <- ggplot(rainfall_30, aes(x = Year, y = Annual)) +
  geom_point(color = "royalblue3", size = 3, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange2", linewidth = 1.2, na.rm = TRUE) +
  geom_hline(yintercept = average_rainfall, color = "firebrick2", linetype = "dashed", linewidth = 1) +
  annotate(
    "text",
    x = 1996,
    y = average_rainfall - 25,
    label = paste("30-year average =", round(average_rainfall, 1), "mm"),
    color = "firebrick2",
    hjust = 0,
    size = 4
  ) +
  scale_x_continuous(breaks = 1995:2025, limits = c(1995, 2025)) +
  labs(
    title = "Annual Total Rainfall at Adelaide Airport (1995-2025)",
    x = "Year",
    y = "Annual Rainfall (mm)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

annual_plot

ggsave(
  file.path(final_folder, "annual_rainfall_plot.png"),
  annual_plot,
  width = 10,
  height = 5,
  dpi = 300
)
