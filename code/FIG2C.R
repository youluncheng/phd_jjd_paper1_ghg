library(readxl)
library(ggplot2)
library(tidyverse)
library(cowplot) 
library(grid)
file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG2/C/FIG2C.xlsx"
if(!file.exists(file_path)) stop("找不到文件，请检查路径！")

data_1980_1989 <- read_excel(file_path, sheet = "1980-1989")
data_1990_1999 <- read_excel(file_path, sheet = "1990-1999")
data_2000_2009 <- read_excel(file_path, sheet = "2000-2009")
data_2010_2023 <- read_excel(file_path, sheet = "2010-2023")

create_plot_with_shared_legend <- function(data, title) {
  name_col <- colnames(data)[1] 
  
  bar_data <- data %>% select(all_of(c(name_col, colnames(data)[2:9]))) %>% rowwise() %>% mutate(Total = sum(c_across(2:9), na.rm = TRUE)) %>% arrange(desc(Total)) %>% select(-Total)
  original_order <- bar_data[[name_col]]
  bar_data_long <- bar_data %>% pivot_longer(cols = -all_of(name_col), names_to = "Category", values_to = "Value") %>% mutate(!!sym(name_col) := factor(!!sym(name_col), levels = original_order))
  line_data <- data %>% select(all_of(c(name_col, colnames(data)[10:11]))) %>% rename(Line1 = 2, Line2 = 3) %>% mutate(!!sym(name_col) := factor(!!sym(name_col), levels = original_order))
  scale_factor <- 4.5
  
  color_mapping <- c("Fertilizer Application" = "#BD7795", "Manure Application" = "#F39865", "Straw Returning" = "#EC6E66", "Straw Burning" = "#91CCC0","Leaching/Runoff" = "#7C7979", "Paddy Rice Cultivation" = "#7FABD1", "Irrigation Energy" = "#EEB6D4", "Agricultural Machinery Energy" = "#2D8875")
  
  p <- ggplot() +
    geom_col(data = bar_data_long, aes(x = !!sym(name_col), y = Value, fill = Category), position = "stack") +
    geom_line(data = line_data, aes(x = !!sym(name_col), y = Line1 * scale_factor, group = 1, color = "Emission/Area (t/ha)"), size = 0.2) +
    geom_point(data = line_data, aes(x = !!sym(name_col), y = Line1 * scale_factor, color = "Emission/Area (t/ha)"), size = 0.4) +
    geom_line(data = line_data, aes(x = !!sym(name_col), y = Line2 * scale_factor, group = 1, color = "Emission/Yield (t/t)"), size = 0.2) +
    geom_point(data = line_data, aes(x = !!sym(name_col), y = Line2 * scale_factor, color = "Emission/Yield (t/t)"), size = 0.4) +
    
    scale_y_continuous(
      name = expression("GHG (Mt CO"[2]*"-eq/yr)"), 
      limits = c(0, 120),
      breaks = seq(0, 120, by = 30),
      sec.axis = sec_axis(~ . / scale_factor, name = "Intensity", breaks = seq(0, 20, by = 5))
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_manual(values = color_mapping) +
    scale_color_manual(values = c("Emission/Area (t/ha)" = "red", "Emission/Yield (t/t)" = "blue")) +
    
    labs(x = name_col, fill = "", color = "", title = title) +
    theme_minimal() + 
    theme(
      text = element_text(color = "black", family = "Arial", size = 8),
      axis.text.x = element_text(color = "black", angle = 90, size = 8, hjust = 1, vjust = 0.5, margin = margin(t = -2)), 
      axis.ticks.length.x = unit(0.05, "cm"),
      axis.text.y = element_text(color = "black", size = 8),
      axis.title.x = element_text(color = "black", size = 0, vjust = 2),
      axis.title.y = element_text(color = "black", size = 8, margin = margin(r = 2, unit = "pt")),
      plot.title = element_text(color = "black", hjust = 0.5, size = 8, vjust = -1),
      plot.title.position = "plot",
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
      axis.line = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      legend.text = element_text(color = "black", size = 8),
      legend.title = element_blank(), legend.key.size = unit(0.3, "lines"),
      legend.position = "bottom", legend.justification = "center",
      legend.box = "horizontal", legend.spacing.y = unit(-0.2, "cm"), 
      legend.margin = margin(t = -5, 0, 0, 0)
    ) +
    guides(fill = guide_legend(ncol = 4, byrow = TRUE), color = guide_legend(ncol = 1))
  return(p)
}

plot_1980_1989 <- create_plot_with_shared_legend(data_1980_1989, "1980-1989")
plot_1990_1999 <- create_plot_with_shared_legend(data_1990_1999, "1990-1999")
plot_2000_2009 <- create_plot_with_shared_legend(data_2000_2009, "2000-2009")
plot_2010_2023 <- create_plot_with_shared_legend(data_2010_2023, "2010-2023")

u <- "cm"
common_margin <- margin(t = 0, r = 0.2, b = 0.4, l = 0, unit = u) # b=0.4 给X轴标签留位
common_margin_right <- margin(t = 0, r = 0.2, b = 0.4, l = 0.2, unit = u)

plot_top_left <- plot_1980_1989 + theme(legend.position = "none", plot.margin = common_margin)
plot_top_right <- plot_1990_1999 + theme(legend.position = "none", plot.margin = common_margin_right)

plot_bottom_left <- plot_2000_2009 + theme(legend.position = "none", plot.margin = common_margin)
plot_bottom_right <- plot_2010_2023 + theme(legend.position = "none", plot.margin = common_margin_right)

combined_plot <- plot_grid(
  plot_top_left, plot_top_right, 
  plot_bottom_left, plot_bottom_right, 
  ncol = 2, nrow = 2, 
  rel_heights = c(1, 1), 
  rel_widths = c(1, 1),
  align = 'v', axis = 'lr'
)

legend <- get_legend(plot_1980_1989)

final_plot <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(6.4, 0.6))

output_dir <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG2"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = paste0(output_dir, "/Figure2C_with_legend.png"),
  plot = final_plot,
  width = 18,
  height = 7.2, 
  units = "cm",
  dpi = 1000,
  bg = "white"
)

message("图片已保存：两行均保留标签，且高度严格一致。")