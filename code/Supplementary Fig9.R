# ==============================================================================
# Historical and SSP food-production line plot
# Data: production prediction.xlsx / Sheet1
# Typography: Arial, 8 pt throughout
# Output aspect ratio: 4:3
# ==============================================================================

library(readxl)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Paths and output settings
# ------------------------------------------------------------------------------
file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/production prediction.xlsx"
sheet_name <- "Sheet1"
out_dir <- dirname(file_path)

r_path <- file.path(out_dir, "production_prediction_lineplot.R")
png_path <- file.path(out_dir, "production_prediction_lineplot.png")
pdf_path <- file.path(out_dir, "production_prediction_lineplot.pdf")

# Exact 4:3 ratio
fig_width_cm <- 8
fig_height_cm <- 6

# Arial font registration on macOS
font_family <- "Arial"
if (identical(Sys.info()[["sysname"]], "Darwin")) {
  quartzFonts(
    Arial = quartzFont(c("Arial", "Arial Bold", "Arial Italic", "Arial Bold Italic"))
  )
}

if (!file.exists(file_path)) {
  stop("Cannot find the input workbook: ", file_path)
}

# ------------------------------------------------------------------------------
# 2. Read and validate data
# ------------------------------------------------------------------------------
production_data <- read_excel(file_path, sheet = sheet_name)

if (ncol(production_data) < 3) {
  stop("Sheet1 must contain at least three columns: Year, Scenarios, and production.")
}

# Use the first three columns so the code is not affected by full-width brackets
# or minor changes in the production-column title.
production_data <- production_data[, 1:3]
names(production_data) <- c("Year", "Scenario", "Production_Mt")

production_data <- production_data %>%
  mutate(
    Year = as.numeric(Year),
    Scenario = trimws(as.character(Scenario)),
    Production_Mt = as.numeric(Production_Mt)
  ) %>%
  arrange(Year, Scenario)

if (anyNA(production_data)) {
  stop("The input data contain missing or non-numeric Year/Production values.")
}

if (anyDuplicated(production_data[c("Year", "Scenario")])) {
  stop("Duplicate Year-Scenario combinations were found in Sheet1.")
}

required_scenarios <- c("Historical", "SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
missing_scenarios <- setdiff(required_scenarios, unique(production_data$Scenario))
if (length(missing_scenarios) > 0) {
  stop("Missing scenarios: ", paste(missing_scenarios, collapse = ", "))
}

production_data <- production_data %>%
  mutate(Scenario = factor(Scenario, levels = required_scenarios))

# ------------------------------------------------------------------------------
# 3. Join the historical endpoint to every SSP line
# ------------------------------------------------------------------------------
historical_data <- production_data %>%
  filter(Scenario == "Historical") %>%
  arrange(Year)

future_data <- production_data %>%
  filter(Scenario != "Historical") %>%
  arrange(Scenario, Year)

anchor_year <- max(historical_data$Year)
anchor_value <- historical_data %>%
  filter(Year == anchor_year) %>%
  pull(Production_Mt)

scenario_anchor <- data.frame(
  Year = anchor_year,
  Scenario = factor(required_scenarios[-1], levels = required_scenarios),
  Production_Mt = rep(anchor_value, length(required_scenarios) - 1)
)

future_plot_data <- bind_rows(scenario_anchor, future_data) %>%
  arrange(Scenario, Year)

# ------------------------------------------------------------------------------
# 4. Plot
# ------------------------------------------------------------------------------
line_colors <- c(
  "Historical" = "#333333",
  "SSP1" = "#1B9E77",
  "SSP2" = "#377EB8",
  "SSP3" = "#D95F02",
  "SSP4" = "#7570B3",
  "SSP5" = "#E7298A"
)

p <- ggplot() +
  # Historical observations
  geom_line(
    data = historical_data,
    aes(x = Year, y = Production_Mt, colour = Scenario, group = Scenario),
    linewidth = 0.6,
    lineend = "round"
  ) +
  geom_point(
    data = historical_data,
    aes(x = Year, y = Production_Mt, colour = Scenario),
    size = 1.5,
    stroke = 0,
    show.legend = FALSE
  ) +
  # Future SSP trajectories, each anchored to the 2024 historical value
  geom_line(
    data = future_plot_data,
    aes(x = Year, y = Production_Mt, colour = Scenario, group = Scenario),
    linewidth = 0.5,
    lineend = "round"
  ) +
  geom_vline(
    xintercept = anchor_year + 0.5,
    colour = "#8C8C8C",
    linewidth = 0.3,
    linetype = "dashed"
  ) +
  scale_colour_manual(
    values = line_colors,
    breaks = required_scenarios,
    drop = FALSE,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050, 2060),
    limits = c(2020, 2060),
    # Keep the 8 cm figure and 8 pt font unchanged; add only a little extra
    # internal space on the right so the centred "2060" label is not clipped.
    expand = expansion(add = c(0.2, 1.2))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Year",
    y = "Crop production (Mt)"
  ) +
  theme_classic(base_family = font_family, base_size = 8) +
  theme(
    text = element_text(family = font_family, size = 8, colour = "black"),
    axis.title = element_text(family = font_family, size = 8, colour = "black"),
    axis.text = element_text(family = font_family, size = 8, colour = "black"),
    legend.text = element_text(family = font_family, size = 8, colour = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.56, 0.23),
    legend.justification = c(0.5, 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(0.45, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.spacing.x = unit(0.08, "cm"),
    legend.spacing.y = unit(0.02, "cm"),
    legend.margin = margin(2, 3, 2, 3, unit = "pt"),
    legend.background = element_rect(fill = "white", colour = NA),
    axis.ticks = element_line(linewidth = 0.3, colour = "black"),
    axis.ticks.length = unit(0.08, "cm"),
    axis.line = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.3
    ),
    plot.margin = margin(5, 5, 5, 5, unit = "pt")
  ) +
  guides(
    colour = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(linewidth = 0.8)
    )
  )

# ------------------------------------------------------------------------------
# 5. Save at an exact 4:3 ratio
# ------------------------------------------------------------------------------
ggsave(
  filename = png_path,
  plot = p,
  width = fig_width_cm,
  height = fig_height_cm,
  units = "cm",
  dpi = 600,
  bg = "white"
)

if (identical(Sys.info()[["sysname"]], "Darwin")) {
  # Quartz uses the Arial family registered above and avoids cairo/pdf font
  # substitution problems on macOS.
  quartz(
    file = pdf_path,
    type = "pdf",
    width = fig_width_cm / 2.54,
    height = fig_height_cm / 2.54,
    family = font_family,
    bg = "white"
  )
  print(p)
  dev.off()
} else {
  ggsave(
    filename = pdf_path,
    plot = p,
    width = fig_width_cm,
    height = fig_height_cm,
    units = "cm",
    device = cairo_pdf,
    bg = "white"
  )
}

cat("R code:", r_path, "\n")
cat("PNG saved to:", png_path, "\n")
cat("PDF saved to:", pdf_path, "\n")
