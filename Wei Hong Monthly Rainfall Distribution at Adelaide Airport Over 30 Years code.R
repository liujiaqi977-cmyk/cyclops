
# Install packages (only run once)
install.packages("readxl")
install.packages("gt")
install.packages("dplyr")

# Load libraries
library(readxl)
library(gt)
library(dplyr)

# Read Excel file
rainfall <- read_excel("Monthly Rainfall Distribution at Adelaide Airport Over 30 Years.xlsx")

# Create formatted table
rainfall %>%

  gt() %>%

  fmt_number(
    columns = Jan:Dec,
    decimals = 1
  ) %>%

  data_color(
    columns = Jan:Dec,
    colors = scales::col_numeric(
      palette = c("#4F8BC9", "#F5F5F5"),
      domain = NULL
    )
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "#ff6666"),
      cell_text(
        color = "black",
        weight = "bold"
      )
    ),
    locations = cells_body(
      columns = Year
    )
  ) %>%

  tab_style(
    style = list(
      cell_fill(color = "#0B4F7D"),
      cell_text(
        color = "white",
        weight = "bold",
        size = px(18)
      )
    ),
    locations = cells_column_labels(
      everything()
    )
  ) %>%

  tab_options(
    table.border.top.color = "grey",
    table.border.bottom.color = "grey",
    heading.background.color = "white",
    table.font.size = 14,
    data_row.padding = px(5)
  )
