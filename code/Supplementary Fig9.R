
library(readxl)
library(tidyverse)
library(RColorBrewer)
output_dir <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG17"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
setwd(output_dir) 
excel_file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG17/FIG17.xlsx"
TARGET_REGION <- ""
TARGET_SHEET_INDEX <- 1
SSP_SCENARIOS <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
START_YEAR <- 2020
END_YEAR <- 2060 
process_single_sheet <- function(sheet_data) {
  names(sheet_data) <- gsub("[[:space:]]+", "", names(sheet_data))
  names(sheet_data) <- trimws(names(sheet_data))
  
  full_data_cleaned <- sheet_data %>%
    mutate(
      Year = as.numeric(Year),
      Mean_Value = as.numeric(Mean_Value),
      Scenario = str_trim(Scenario)
    ) %>%
    filter(Scenario %in% SSP_SCENARIOS,
           Year >= START_YEAR,
           Year <= END_YEAR) %>%
    dplyr::select(Year, Mean_Value, Scenario)
  
  return(full_data_cleaned)
}
all_colors <- c(
  "SSP1" = "#5B9A89",
  "SSP2" = "#7B9DA8",
  "SSP3" = "#B07156",
  "SSP4" = "#A5A58D",
  "SSP5" = "#D1603D"
)
GHG_Y_TITLE <- expression(paste("GHG (Mt CO"[2]*"-eq/yr)"))

create_mytheme_single <- function() {
  
  theme_bw() +
    theme(
     
      text = element_text(family = "ArialMT", size = 8, color = "black"),
      axis.text = element_text(size = 8, family = "ArialMT", color = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8, family = "ArialMT", face = "bold", margin = margin(r = 5), color = "black"),
      plot.title = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(size = 8, family = "ArialMT", color = "black"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", linewidth = 0.5),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.text.x = element_text(size = 8, family = "ArialMT", color = "black"),
      axis.ticks.x = element_line(linewidth = 0.5)
    )
}
y_limits_china <- c(36000, 52000, 4000) # [ymin, ymax, ystep]
plot_scenario_single <- function(forecast_data, region_name, y_limits) {
  LINE_WIDTH <- 0.8
  POINT_SIZE <- 2
  POINT_STROKE <- 0.7
  current_theme <- create_mytheme_single()
  scenario_levels <- SSP_SCENARIOS
  point_data_forecast <- forecast_data %>%
    mutate(Scenario = factor(Scenario, levels = SSP_SCENARIOS))
  plot_data <- point_data_forecast
  p <- ggplot() +
    geom_line(data = plot_data,
              aes(x = Year, y = Mean_Value, color = Scenario),
              linewidth = LINE_WIDTH) +
    geom_point(data = point_data_forecast,
               aes(x = Year, y = Mean_Value, color = Scenario),
               shape = 21,
               fill = "white",
               size = POINT_SIZE,
               stroke = POINT_STROKE) +
    scale_color_manual(values = all_colors,
                       breaks = scenario_levels,
                       labels = SSP_SCENARIOS,
                       name = "") +
    labs(x = NULL,
         y = GHG_Y_TITLE,
         title = NULL) +
    scale_x_continuous(breaks = seq(START_YEAR, END_YEAR, by = 10),
                       limits = c(START_YEAR, END_YEAR)) +
    scale_y_continuous(limits = c(y_limits[1], y_limits[2]),
                       breaks = seq(y_limits[1], y_limits[2], by = y_limits[3]),
                       expand = c(0, 0)) +
   current_theme +
    coord_cartesian(xlim = c(START_YEAR, END_YEAR), expand = FALSE) +
 guides(color = guide_legend(
      override.aes = list(
        linewidth = LINE_WIDTH,
        colour = unname(all_colors[scenario_levels]),
        shape = rep(21, length(SSP_SCENARIOS)),
        size = rep(POINT_SIZE, length(SSP_SCENARIOS)),
        stroke = rep(POINT_STROKE, length(SSP_SCENARIOS)),
        fill = rep("white", length(SSP_SCENARIOS))
      ),
      nrow = 1
    ))
  
  return(p)
}

sheet_raw_data <- read_excel(
  path = excel_file_path,
  sheet = TARGET_SHEET_INDEX,
  col_names = TRUE
)
current_forecast_data <- process_single_sheet(sheet_raw_data)
p_single <- plot_scenario_single(current_forecast_data, TARGET_REGION, y_limits_china)
filename <- "SI_croprequest.png" 
ggsave(filename, plot = p_single, width = 15, height = 8, units = "cm", dpi = 600)
message(paste("：", filename, "", output_dir))
print(p_single)