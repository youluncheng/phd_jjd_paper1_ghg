
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork) 



years <- 1980:2023
population_vals <- c(98705, 100072, 101654, 103008, 104357, 105851, 107507, 109300, 111026, 112704, 114333, 115823, 
                     117171, 118517, 119850, 121121, 122389, 123626, 124761, 125786, 126743, 127627, 128453, 129227, 
                     129988, 130756, 131448, 132129, 132802, 133450, 134091, 134916, 135922, 136726, 137646, 138326, 
                     139232, 140011, 140541, 141008, 141212, 141260, 141175, 140967) / 10000 


df_history <- data.frame(year = years, population = population_vals)
pop_2023 <- population_vals[length(population_vals)]
HISTORY_END_YEAR <- 2023


FORECAST_END_YEAR <- 2060
forecast_years <- (HISTORY_END_YEAR + 1):FORECAST_END_YEAR
n_years_2060 <- length(forecast_years)


high_final_2100 <- 5.9
medium_final_2100 <- 4.6
low_final_2100 <- 3.2
n_years_2100 <- length(2024:2100) 

predict_accelerating <- function(start, end, n) {
  t <- seq(0, 1, length.out = n)
  accelerating_values <- start + (end - start) * t^3
  return(accelerating_values)
}


high_full_forecast <- predict_accelerating(pop_2023, high_final_2100, n_years_2100)
medium_full_forecast <- predict_accelerating(pop_2023, medium_final_2100, n_years_2100)
low_full_forecast <- predict_accelerating(pop_2023, low_final_2100, n_years_2100)

high_forecast <- high_full_forecast[1:n_years_2060]
medium_forecast <- medium_full_forecast[1:n_years_2060]
low_forecast <- low_full_forecast[1:n_years_2060]


df_forecast <- bind_rows(
  data.frame(year = forecast_years, population = high_forecast, scenario = "High fertility scenario"),
  data.frame(year = forecast_years, population = medium_forecast, scenario = "Medium fertility scenario"),
  data.frame(year = forecast_years, population = low_forecast, scenario = "Low fertility scenario")
)


df_history$scenario <- "Historical population"
df_plot_history <- df_history %>% 
  rename(mean_value = population) %>%
  dplyr::select(year, mean_value, scenario)


df_plot_forecast <- df_forecast %>% 
  rename(mean_value = population) %>%
  dplyr::select(year, mean_value, scenario)


segment_data <- df_plot_forecast %>% 
  group_by(scenario) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate(
    x = HISTORY_END_YEAR,
    y = pop_2023,
    xend = year,
    yend = mean_value
  )


df_lines <- bind_rows(df_plot_history, df_plot_forecast)


scenario_levels <- c(
  "Historical population",
  "High fertility scenario",
  "Medium fertility scenario",
  "Low fertility scenario"
)
df_lines$scenario <- factor(df_lines$scenario, levels = scenario_levels)

# ----------------------------------------------------
# 3. 定义颜色方案和绘图主题 (保持不变)
# ----------------------------------------------------

all_colors_pop <- c(
  "Historical population"= "#7C7979", # 灰色
  "High fertility scenario" = "#387877", # 蓝绿色
  "Medium fertility scenario" = "#447294", # 蓝色
  "Low fertility scenario" = "#E16A4D"# 红色/橙色
)


create_pop_theme <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "ArialMT", size = 8, color = "black"),
      axis.text = element_text(size = 8, family = "ArialMT", color = "black"),
      
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8, family = "ArialMT", margin = margin(r = 2), color = "black"), # 统一为 8pt
      
      legend.position = "top",
      legend.title = element_blank(),
      legend.key.size = unit(0.6, "lines"),
      legend.text = element_text(size = 8, family = "ArialMT", color = "black"),
      legend.background = element_blank(),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      panel.border = element_rect(colour = "black", linewidth = 0.5),
      
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      
      axis.ticks = element_line(linewidth = 0.5),
      plot.margin = margin(t = 0, r = 10, b = 0, l = 3, unit = "pt")
    )
}


plot_population_scenario <- function(data_lines, data_segments, data_history, data_forecast_points, all_colors, scenario_levels) {
  LINE_WIDTH <- 0.6
  POINT_SIZE <- 1.5
  POINT_STROKE <- 0.6
  VLINE_WIDTH <- 0.5
  
  
  X_START <- min(data_lines$year)
  X_END <- max(data_lines$year)
  
  p <- ggplot() +
    
  
    geom_vline(xintercept = HISTORY_END_YEAR,
               linetype = "dashed",
               color = "gray50",
               linewidth = VLINE_WIDTH) +
    
    
    geom_line(data = data_lines %>% filter(scenario == "Historical population"),
              aes(x = year, y = mean_value, color = scenario),
              linewidth = LINE_WIDTH,
              linetype = "solid") +
    
   
    geom_line(data = data_lines %>% filter(scenario != "Historical population"),
              aes(x = year, y = mean_value, color = scenario),
              linewidth = LINE_WIDTH,
              linetype = "dashed") +
    
    
    geom_segment(data = data_segments,
                 aes(x = x, xend = xend, y = y, yend = yend, color = scenario),
                 linewidth = LINE_WIDTH,
                 linetype = "dashed") +
    
  
    geom_point(data = data_history %>% filter(year %% 10 == 0),
               aes(x = year, y = mean_value, color = scenario),
               shape = 21, fill = "white",
               size = POINT_SIZE, stroke = POINT_STROKE) +
    
   
    geom_point(data = data_forecast_points %>% filter(year %% 10 == 0),
               aes(x = year, y = mean_value, color = scenario),
               shape = 21, fill = "white",
               size = POINT_SIZE, stroke = POINT_STROKE) +
    
   
    scale_color_manual(values = all_colors,
                       breaks = scenario_levels) +
    
    
    labs(x = NULL, 
         y = "Population (billions)",
         title = NULL,
         color = NULL) +
    
   
    scale_x_continuous(breaks = seq(1980, 2060, by = 20),
                       limits = c(1980, 2060), 
                       expand = c(0, 0)) +
    
    
    scale_y_continuous(limits = c(10, 15), 
                       breaks = seq(10, 15, by = 1),
                       expand = c(0, 0)) +
    
   
    create_pop_theme() +
    
   
    guides(color = guide_legend(
      override.aes = list(
        linetype = c("solid", rep("dashed", 3)),
        shape = c(21, rep(21, 3)),
        fill = "white",
        size = POINT_SIZE,
        stroke = POINT_STROKE
      ),
      nrow = 1
    ))
  
  return(p)
}



p_pop <- plot_population_scenario(
  df_lines, segment_data, df_plot_history, df_plot_forecast, all_colors_pop, scenario_levels
)


output_path_final <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG15/Population_2060.png" 

ggsave(output_path_final, plot = p_pop, width = 15, height = 6, units = "cm", dpi = 600) 

message(paste("✅ 成功生成人口预测图，截止年份为 2060 年，保存到：", output_path_final))


print(p_pop)