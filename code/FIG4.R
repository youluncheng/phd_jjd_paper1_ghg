####A

library(sf)        
library(ggplot2)    
library(jsonlite)  
library(gridExtra)  

china_map <- st_read(
  "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_A/shengfenbianjie.json", 
  quiet = TRUE,
  check_ring_dir = FALSE
)
china_map <- st_make_valid(china_map)  
zones <- list(
  "ZoneI" = c("内蒙古自治区", "新疆维吾尔自治区", "甘肃省", "青海省", "西藏自治区", 
              "陕西省", "山西省", "宁夏回族自治区"),
  "ZoneII" = c("黑龙江省", "吉林省", "辽宁省"),
  "ZoneIII" = c("北京市", "天津市", "河北省", "河南省", "山东省"),
  "ZoneIV" = c("浙江省", "上海市", "安徽省", "江西省", "湖南省", "湖北省", 
               "四川省", "重庆市", "江苏省"),
  "ZoneV" = c("广东省", "广西壮族自治区", "海南省", "福建省", "云南省", "贵州省"),
  "ZoneVI" = setdiff(unique(china_map$name), c("台湾省", "香港特别行政区", "澳门特别行政区"))
)

label_names <- c("ZoneI", "ZoneII", "ZoneIII", "ZoneIV", "ZoneV", "Mainland China")

plots <- list()

for (i in seq_along(zones)) {
  target_provinces <- zones[[i]]
  china_map$fill <- ifelse(china_map$name %in% target_provinces, "yes", "no")
  
  current_label <- label_names[[i]]
  
  if (i == 1) {
    current_margin <- unit(c(40, 1, 0, 1), "pt") 
  } else {
    current_margin <- unit(c(0, 1, 0, 1), "pt")
  }
  
  p <- ggplot() +
    geom_sf(data = subset(china_map, fill == "no"), 
            fill ="#F0F0F0", color = NA, linewidth = 0) +
    geom_sf(data = subset(china_map, fill == "yes"), 
            fill = "#B3CDE3", color = NA, linewidth = 0) +
    geom_sf(data = china_map, 
            fill = NA, color = "black", linewidth = 0.3) + 
    annotate("text", x = 73, y = 58, label = current_label, family = "Arial", color = "black", size = 8 / .pt, fontface = "plain", hjust = 0) +             
    
    labs(title = NULL) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = current_margin, 
      
    
      panel.background = element_blank(), # 绘图区背景透明
      plot.background = element_blank(),  # 整个图表区背景透明 (包括边距)
    
      rect = element_rect(fill = "transparent", colour = NA)
    )
  
  plots[[i]] <- p
}

layout_heights <- c(1.5, 1, 1, 1, 1, 1)

output_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_A/FIG4_A.png"

ggsave(
  output_path,
  do.call(grid.arrange, c(plots, ncol = 1, heights = list(layout_heights))),
  width = 3.5,    
  height = 24,  
  units = "cm",
  dpi = 600,
  bg = "transparent" 
)
message(paste("Saved transparent image:", output_path))




##########B

# ----------------------------------------------------
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(patchwork) 

output_dir <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_B"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
setwd(output_dir) 

excel_file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_B/yuceshuju2.xlsx"

region_map <- c("ZoneI", "ZoneII", "ZoneIII", "ZoneIV", "ZoneV", "China")
num_sheets <- length(region_map)

HISTORY_END_YEAR <- 2023
FORECAST_START_YEAR <- 2023 
SSP_SCENARIOS <- c("SSP1-RCP26", "SSP2-RCP45", "SSP3-RCP70", "SSP4-RCP34", "SSP5-RCP85")

process_single_sheet <- function(sheet_data) {
  names(sheet_data) <- gsub("[[:space:]]+", "", names(sheet_data))
  names(sheet_data) <- trimws(names(sheet_data))
  
  data_cleaned <- sheet_data %>% 
    mutate(
      Year = as.numeric(Year),
      Mean_Value = as.numeric(Mean_Value),
      Lower_Bound = as.numeric(Lower_Bound),
      Upper_Bound = as.numeric(Upper_Bound),
      Scenario = str_trim(Scenario)
    ) %>% 
    filter(Scenario %in% SSP_SCENARIOS,
           Year >= 1980,
           Year <= 2060)
  
  shared_history_data <- data_cleaned %>% 
    filter(Year <= HISTORY_END_YEAR, Scenario == "SSP2-RCP45") %>% 
    mutate(Scenario = "History") 
  
  full_forecast_data <- data_cleaned %>% 
    filter(Year >= (HISTORY_END_YEAR + 1)) 
  
  return(list(history_data = shared_history_data, forecast_data = full_forecast_data))
}


all_colors <- c(
  "History"      = "#7C7979",
  "SSP1-RCP26" = "#C3E6C8", 
  "SSP2-RCP45" = "#C7E7F8", 
  "SSP3-RCP70" = "#FFE3CC", 
  "SSP4-RCP34" = "#D8C4E6", 
  "SSP5-RCP85" = "#F08080"  
)

GHG_Y_TITLE <- "GHG Emissions\n(Mt CO\u2082-eq/yr)"

create_mytheme <- function(is_bottom_plot = FALSE, is_first_plot = FALSE) {
  
  legend_pos <- if (is_first_plot) "top" else "none" 
  
  theme_bw() + 
    theme(
      text = element_text(family = "Arial", size = 8, color = "black"), 
      axis.text = element_text(size = 8, family = "Arial", color = "black"), 
      
      axis.title.x = element_blank(), 
  
      axis.title.y = element_text(
        size = 8, 
        family = "Arial", 
        face = "plain", 
        color = "black",
        lineheight = 0.8, 
        margin = margin(t = 0, r = 0.5, b = 0, l = 0.5) 
      ),
      plot.title = element_blank(), 
      plot.margin = margin(t = 7, r = 5, b = 7, l = 2, unit = "pt"), 
      legend.position = legend_pos, 
      legend.title = element_blank(),
      legend.justification = "left",
      legend.box.just = "left", 
      legend.key.size = unit(0.5, "lines"),
      legend.text = element_text(size = 8, family = "Arial", color = "black"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(t = 15, r = 0, b = 0, l = -33, unit = "pt"), 
      
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      
      panel.border = element_rect(colour = "black", linewidth = 0.3), 
      
      axis.line.x = element_blank(), 
      axis.line.y = element_blank(), 
      
      axis.text.x = if (is_bottom_plot) element_text(size = 8, family = "Arial", color = "black") else element_blank(),
      axis.ticks.x = if (is_bottom_plot) element_line(linewidth = 0.3) else element_blank()
    )
}

y_limits_map <- list(
  "ZoneI" = c(20, 120, 20),
  "ZoneII" = c(50, 230, 60),
  "ZoneIII" = c(80, 240, 30),
  "ZoneIV" = c(200, 500, 100),
  "ZoneV" = c(40, 200, 40),
  "China" = c(400, 1200, 200)
)


interpolate_ribbon_data <- function(data, smooth_mean = FALSE) {
  if (nrow(data) == 0) return(data)
  
  scenarios <- unique(data$Scenario)
  interpolated_dfs <- list()
  
  for (s in scenarios) {
    df_scenario <- data %>% filter(Scenario == s)
    
    if (nrow(df_scenario) > 1) {
      n_points <- max(500, nrow(df_scenario))
      full_years <- seq(min(df_scenario$Year), max(df_scenario$Year), length.out = n_points)
      
      spline_lower <- spline(df_scenario$Year, df_scenario$Lower_Bound, xout = full_years)
      spline_upper <- spline(df_scenario$Year, df_scenario$Upper_Bound, xout = full_years)
      
      if (smooth_mean) {
        spline_mean <- spline(df_scenario$Year, df_scenario$Mean_Value, xout = full_years)
        mean_values <- spline_mean$y
      } else {
        mean_values <- approx(df_scenario$Year, df_scenario$Mean_Value, xout = full_years, method = "linear")$y
      }
      
      interpolated_df <- data.frame(
        Year = full_years,
        Mean_Value = mean_values, 
        Lower_Bound = spline_lower$y,
        Upper_Bound = spline_upper$y,
        Scenario = s
      )
      
      interpolated_dfs[[length(interpolated_dfs) + 1]] <- interpolated_df
    } else {
      interpolated_dfs[[length(interpolated_dfs) + 1]] <- df_scenario
    }
  }
  
  return(bind_rows(interpolated_dfs))
}

plot_scenario_by_region <- function(history_data, forecast_data, region_name, is_bottom, is_first, y_limits_map) {
  LINE_WIDTH <- 0.5  
  POINT_SIZE <- 1    
  POINT_STROKE <- 0.5 
  ALPHA_VALUE <- 0.3 
  VLINE_WIDTH <- 0.5 
  
  y_limits <- y_limits_map[[region_name]]
  
  current_theme <- create_mytheme(is_bottom, is_first)
  scenario_levels <- c("History", SSP_SCENARIOS)
  
  plot_data <- forecast_data %>% 
    mutate(Scenario = factor(Scenario, levels = SSP_SCENARIOS))
  
  history_data_smooth <- interpolate_ribbon_data(history_data, smooth_mean = TRUE)
  forecast_ribbon_data <- interpolate_ribbon_data(plot_data, smooth_mean = FALSE)
  
  history_end_points <- history_data %>% 
    filter(Year == HISTORY_END_YEAR) %>% 
    dplyr::select(History_Value = Mean_Value)
  
  history_end_points_data <- history_data %>%
    filter(Year == HISTORY_END_YEAR)
  
  segment_data <- plot_data %>% 
    group_by(Scenario) %>% 
    slice(1) %>% 
    ungroup() %>%
    mutate(History_Value = history_end_points$History_Value[1])
  
  point_data_forecast <- plot_data %>%
    filter(Year %% 10 == 0) 
  
  legend_fills <- c(unname(all_colors["History"]), unname(all_colors[SSP_SCENARIOS])) 
  legend_alphas <- rep(ALPHA_VALUE, length(scenario_levels)) 
  
  p <- ggplot() +
    geom_ribbon(data = history_data_smooth,
                aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound, fill = Scenario),
                alpha = ALPHA_VALUE) + 
    geom_line(data = history_data_smooth,
              aes(x = Year, y = Mean_Value, color = "History"), 
              linewidth = LINE_WIDTH,
              linetype = "solid") +
    geom_vline(xintercept = HISTORY_END_YEAR, 
               linetype = "dashed", 
               color = "gray50", 
               linewidth = VLINE_WIDTH) +
    geom_ribbon(data = forecast_ribbon_data,
                aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound, fill = Scenario),
                alpha = ALPHA_VALUE) + 
    geom_line(data = plot_data,
              aes(x = Year, y = Mean_Value, color = Scenario),
              linewidth = LINE_WIDTH) +
    geom_segment(data = segment_data,
                 aes(x = HISTORY_END_YEAR, xend = (HISTORY_END_YEAR + 1), 
                     y = History_Value, yend = Mean_Value, color = Scenario),
                 linewidth = LINE_WIDTH) +
    geom_point(data = history_end_points_data,
               aes(x = Year, y = Mean_Value, color = Scenario),
               shape = 21, 
               fill = "white",
               size = POINT_SIZE, 
               stroke = POINT_STROKE) + 
    geom_point(data = point_data_forecast, 
               aes(x = Year, y = Mean_Value, color = Scenario), 
               shape = 21, 
               fill = "white", 
               size = POINT_SIZE, 
               stroke = POINT_STROKE) + 
    scale_color_manual(values = all_colors, 
                       breaks = scenario_levels,
                       labels = c("Historical Line", SSP_SCENARIOS),
                       name = "") + 
    scale_fill_manual(values = all_colors,
                      breaks = scenario_levels,
                      guide = "none") + 
    labs(x = NULL,
         y = GHG_Y_TITLE,
         title = region_name) +
    scale_x_continuous(breaks = seq(1980, 2060, by = 20),
                       limits = c(1980, 2060)) +
    scale_y_continuous(limits = c(y_limits[1], y_limits[2]),
                       breaks = seq(y_limits[1], y_limits[2], by = y_limits[3]),
                       expand = c(0, 0)) + 
    current_theme +
    coord_cartesian(xlim = c(1980, 2060), expand = FALSE) + 
    guides(color = guide_legend(
      override.aes = list(
        fill = legend_fills, 
        alpha = legend_alphas, 
        linewidth = LINE_WIDTH,
        colour = unname(all_colors[scenario_levels]), 
        shape = c(NA, rep(21, length(SSP_SCENARIOS))), 
        size = c(NA, rep(POINT_SIZE, length(SSP_SCENARIOS))), 
        stroke = c(NA, rep(POINT_STROKE, length(SSP_SCENARIOS))), 
        fill = c(NA, rep("white", length(SSP_SCENARIOS))) 
      ),
      nrow = 2 
    ))
  
  return(p)
}


all_plots <- list()

for (i in 1:num_sheets) {
  current_region <- region_map[i]
  is_first <- (i == 1) 
  
  sheet_raw_data <- read_excel(
    path = excel_file_path,
    sheet = i, 
    col_names = TRUE
  )
  
  data_components <- process_single_sheet(sheet_raw_data)
  current_history_data <- data_components$history_data
  current_forecast_data <- data_components$forecast_data
  is_bottom <- (i == num_sheets) 
  
  p <- plot_scenario_by_region(current_history_data, current_forecast_data, current_region, is_bottom, is_first, y_limits_map)
  all_plots[[i]] <- p
}

p_combined <- wrap_plots(all_plots, ncol = 1) 

p_combined <- p_combined + 
  plot_annotation(
    title = NULL,
    theme = theme(plot.title = element_blank(),
                  plot.margin = margin(t = 0.5, r = 5, b = 0.5, l =1, unit = "pt"))
  )

filename <- "FIG4_B.png" 
ggsave(filename, plot = p_combined, width = 7.5, height = 24, units = "cm", dpi = 300)

message(paste("成功生成并保存了图表：", filename, "到输出目录：", output_dir))

print(p_combined)








# FIG4_C

library(tidyverse)
library(patchwork)
library(cowplot) 
library(readxl)
library(grid)        
library(scales)    
file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_C/FIG4_C_with_SD.xlsx" 
data_long_raw <- read_xlsx(file_path)
PATTERN_LEVELS <- c("SSP1-RCP26", "SSP2-RCP45")
YEAR_LEVELS <- c(2030, 2060)
REGION_LEVELS_ORDERED <- c("ZoneI", "ZoneII", "ZoneIII", "ZoneIV", "ZoneV", "Mainland China")
PLOT_TYPES <- c("Costs", "Benefits")
if(!"SD" %in% names(data_long_raw) && "sd" %in% names(data_long_raw)) {
  names(data_long_raw)[names(data_long_raw) == "sd"] <- "SD"
}

data_plot <- data_long_raw %>%
  filter(Year %in% YEAR_LEVELS, Scenario %in% PATTERN_LEVELS) %>%
  mutate(Unified_Type = ifelse(Benefit_Type == "Costs", "Costs", "Benefits")) %>%
  group_by(Region, Scenario, Year, Unified_Type) %>%
  summarise(Value = sum(Value, na.rm = TRUE), Raw_SD = sqrt(sum(SD^2, na.rm = TRUE)), .groups = 'drop') %>%
  complete(Region, Scenario, Year, Unified_Type, fill = list(Value = 0, Raw_SD = 0)) %>%
  mutate(Unified_Type = factor(Unified_Type, levels = PLOT_TYPES), 
         Year = factor(Year, levels = YEAR_LEVELS),
         Region = factor(Region, levels = REGION_LEVELS_ORDERED), 
         Scenario = factor(Scenario, levels = PATTERN_LEVELS))

plot_region_grouped_pattern <- function(data, region_name) {
  data_subset <- data %>% filter(Region == region_name)
  is_bottom_plot <- (region_name == REGION_LEVELS_ORDERED[length(REGION_LEVELS_ORDERED)])
  
  bar_width <- 0.35; dodge_distance <- 0.25 
  data_subset_modified <- data_subset %>%
    mutate(Year_Numeric = as.numeric(Year), 
           X_Pos = case_when(Unified_Type == "Costs" ~ Year_Numeric - dodge_distance, 
                             Unified_Type == "Benefits" ~ Year_Numeric + dodge_distance))
  
  max_val_data <- max(data_subset_modified$Value + data_subset_modified$Raw_SD, na.rm = TRUE)
  min_val_data <- min(data_subset_modified$Value - data_subset_modified$Raw_SD, na.rm = TRUE)
  
  if (is.infinite(max_val_data)) max_val_data <- 0
  if (is.infinite(min_val_data)) min_val_data <- 0
  if (max_val_data == min_val_data) { max_val_data <- max_val_data + 1; min_val_data <- min_val_data - 1 }
  
  
  pretty_breaks <- base::pretty(c(min_val_data, max_val_data), n = 3)
  
  y_limit_min <- min(pretty_breaks)
  y_limit_max <- max(pretty_breaks)
  
  p <- ggplot(data_subset_modified, aes(x = X_Pos, y = Value, fill = Unified_Type)) +
    geom_col(width = bar_width, color = "#555555", linewidth = 0.3) +
    geom_errorbar(aes(ymin = Value - Raw_SD, ymax = Value + Raw_SD), width = 0.15, linewidth = 0.2, color = "black") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) + 
    
    scale_y_continuous(breaks = pretty_breaks, 
                       expand = c(0, 0), 
                       name = "Value (billion USD)") +
    coord_cartesian(ylim = c(y_limit_min, y_limit_max)) + 
    
    scale_x_continuous(breaks = as.numeric(factor(YEAR_LEVELS)), labels = YEAR_LEVELS, expand = expansion(mult = 0.05)) +
    facet_grid(. ~ Scenario, space = "free_x") +
    scale_fill_manual(values = c("Costs"="#FF9896", "Benefits"="#AEC7E8")) + guides(fill = "none") + 
    
    
    labs(title = NULL, x = "") +
    theme_minimal(base_size = 8, base_family = "Arial") +
    theme(
      plot.title = element_blank(),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.title.y = element_text(size = 8, color = "black"),
      legend.position = "none", 
      
      panel.border = element_rect(colour = "#333333", fill = NA, linewidth = 0.3),
      
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      
      axis.line = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.ticks.y = element_line(color = "black", linewidth = 0.3), 
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.3, linetype = "solid"),
      strip.text = element_text(size = 8, color = "black"),
      
      plot.margin = margin(t = 6, r = 2, b = 6, l = 2, unit = "pt"),
      panel.spacing.x = unit(0, "lines"),
      
      axis.title.x = if (is_bottom_plot) element_text(size = 8, color = "black", margin = margin(t = 5, b = 0)) else element_blank(),
      axis.text.x = if (is_bottom_plot) element_text(size = 8, angle = 0, hjust = 0.5, vjust = -1, color = "black", margin = margin(t = 0, b = 0)) else element_blank()
    )
  if (is_bottom_plot) p <- p + theme(axis.text.x.bottom = element_text(margin = margin(t = 0, b = 0)), axis.title.x.bottom = element_text(margin = margin(t = 0, b = 0)))
  return(p)
}

regions_to_plot <- REGION_LEVELS_ORDERED
plot_list <- lapply(regions_to_plot, function(r) plot_region_grouped_pattern(data_plot, r))

legend_data_new <- data.frame(Unified_Type = factor(PLOT_TYPES, levels=PLOT_TYPES), Value = 1)
p_legend_full <- ggplot(legend_data_new, aes(x=Unified_Type, y=Value, fill=Unified_Type)) +
  geom_col() + scale_fill_manual(values = c("Costs"="#FF9896", "Benefits"="#AEC7E8"), labels = PLOT_TYPES, name = NULL, guide = guide_legend(order = 1, nrow = 1)) +
  theme_void(base_size = 8, base_family = "Arial") +
  theme(legend.position = "bottom", legend.text = element_text(size = 8, color = "black", face="plain"), legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.3, "cm"), plot.margin = unit(c(0, 0, 0, 0), "pt")) 
legend_grob <- cowplot::get_legend(p_legend_full)

legend_wrapper <- ggplot() + 
  cowplot::draw_grob(legend_grob) + 
  theme_void() + 
  theme(plot.margin = margin(t = 0, r = 0, b = -15, l = 0, unit = "pt"))

final_plot_combined <- wrap_plots(
  legend_wrapper, 
  plot_list[[1]], plot_list[[2]], plot_list[[3]], 
  plot_list[[4]], plot_list[[5]], plot_list[[6]],
  ncol = 1, 
  heights = c(0.55, rep(1, 6)) 
) + 
  plot_annotation(
    theme = theme(
      plot.margin = unit(c(5, 5, 0, 5), "pt")
    )
  )

output_file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_C/FIG4_C.png"
ggsave(output_file_path, final_plot_combined, width = 7, height = 24, units = "cm", dpi = 300)
message(paste("Saved:", output_file_path))



#######combine
library(magick)
library(cowplot)
library(ggplot2)
library(grid)

path_A <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_A/FIG4_A.png"
path_B <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_B/FIG4_B.png"
path_C <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_C/FIG4_C.png"

img_A <- image_read(path_A)
img_B <- image_read(path_B)
img_C <- image_read(path_C)

info_A <- image_info(img_A)
info_B <- image_info(img_B)
info_C <- image_info(img_C)

max_height <- max(info_A$height, info_B$height, info_C$height)

resize_to_max <- function(img, target_h) {
  image_resize(img, geometry = paste0("x", target_h))
}

img_A_final <- resize_to_max(img_A, max_height)
img_B_final <- resize_to_max(img_B, max_height)
img_C_final <- resize_to_max(img_C, max_height)

w_A <- image_info(img_A_final)$width
w_B <- image_info(img_B_final)$width
w_C <- image_info(img_C_final)$width

rel_widths_calc <- c(w_A, w_B, w_C)

p1 <- ggdraw() + draw_image(img_A_final)
p2 <- ggdraw() + draw_image(img_B_final)
p3 <- ggdraw() + draw_image(img_C_final)

final_plot <- plot_grid(
  p1, p2, p3,
  ncol = 3,
  rel_widths = rel_widths_calc, 
  labels = c("a", "b", "c"),    
  label_size = 12,              
  label_fontfamily = "Arial",  
  label_x = 0.02, label_y = 0.98, 
  hjust = 0, vjust = 1
)

total_pixel_width <- w_A + w_B + w_C
total_cm_width <- 24 * (total_pixel_width / max_height)

output_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG4_Combined_Fixed.png"

ggsave(
  output_path,
  final_plot,
  width = total_cm_width, 
  height = 24, 
  units = "cm",
  dpi = 300, 
  bg = "white" 
)

message(paste("拼接完成:", output_path))
message(paste("计算出的总宽度:", round(total_cm_width, 2), "cm"))