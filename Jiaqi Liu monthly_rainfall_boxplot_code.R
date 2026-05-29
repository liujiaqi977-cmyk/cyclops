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

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

rainfall_long <- rainfall_30 %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "Month",
    values_to = "Rainfall"
  ) %>%
  mutate(Month = factor(Month, levels = month_levels))

monthly_outliers <- rainfall_long %>%
  filter(!is.na(Rainfall)) %>%
  group_by(Month) %>%
  filter(Rainfall %in% boxplot.stats(Rainfall)$out) %>%
  arrange(Rainfall, .by_group = TRUE) %>%
  mutate(label_y = Rainfall + 4 * row_number()) %>%
  mutate(
    label_y = case_when(
      Month == "Mar" & Year == 1999 ~ Rainfall - 5,
      Month == "Mar" & Year == 2012 ~ Rainfall + 7,
      TRUE ~ label_y
    )
  ) %>%
  ungroup()

box_plot <- ggplot(rainfall_long, aes(x = Month, y = Rainfall)) +
  geom_boxplot(
    fill = "deepskyblue2",
    color = "black",
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_point(data = monthly_outliers, color = "firebrick2", size = 2.5) +
  geom_text(
    data = monthly_outliers,
    aes(y = label_y, label = Year),
    color = "firebrick2",
    size = 3.2
  ) +
  labs(
    title = "Monthly Rainfall Distribution at Adelaide Airport (1995-2025)",
    x = "Month",
    y = "Monthly Rainfall (mm)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )

box_plot

ggsave(
  file.path(final_folder, "monthly_rainfall_boxplot.png"),
  box_plot,
  width = 9,
  height = 5,
  dpi = 300
)
