
library(readxl)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(grid)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG2/C/FIG2C.xlsx"
output_dir <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG2"

if(!file.exists(file_path)) stop("！")
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

data_1980_1989 <- read_excel(file_path, sheet = "1980-1989")
data_1990_1999 <- read_excel(file_path, sheet = "1990-1999")
data_2000_2009 <- read_excel(file_path, sheet = "2000-2009")
data_2010_2023 <- read_excel(file_path, sheet = "2010-2023")

create_plot_no_legend <- function(data, title_text) {
  name_col <- colnames(data)[1]
  n_cols <- ncol(data)
  col_bar_indices <- 2:(n_cols - 2) 
  col_line_indices <- (n_cols - 1):n_cols 
  
  bar_data <- data %>% 
    select(all_of(c(name_col, colnames(data)[col_bar_indices]))) %>% 
    rowwise() %>% 
    mutate(Total = sum(c_across(all_of(colnames(data)[col_bar_indices])), na.rm = TRUE)) %>% 
    arrange(desc(Total)) %>% 
    select(-Total)
  original_order <- bar_data[[name_col]]
  
  bar_data_long <- bar_data %>% 
    pivot_longer(cols = -all_of(name_col), names_to = "Category", values_to = "Value") %>% 
    mutate(!!sym(name_col) := factor(!!sym(name_col), levels = original_order))
  
  user_order_top_to_bottom <- c(
    "Carbon sequestration", "Paddy rice", "Machinery energy", "Fertilizer application", "Manure application", 
    "Straw burning", "Straw returning", "Microbial N fixation", "Leaching/runoff", "N deposition"
  )
  bar_data_long$Category <- factor(bar_data_long$Category, levels = rev(user_order_top_to_bottom))
  
  line_data <- data %>% 
    select(all_of(c(name_col, colnames(data)[col_line_indices]))) %>% 
    rename(Line1 = 2, Line2 = 3) %>% 
    mutate(!!sym(name_col) := factor(!!sym(name_col), levels = original_order))
  
  scale_factor <- 2.625
  dark_red_color <- "black"
  light_black_color <- "black"
  color_mapping <- c(
    "Fertilizer application" = "#9E4C6E", "Manure application" = "#F16C27", "Straw returning" = "#D74C4F", 
    "Paddy rice" = "#7FABD1", "Straw burning"= "#91CCC0", "Machinery energy" = "#EEB6D4", 
    "N leaching/runoff"= "#585858", "N deposition emissions"= "#D1A1E2", "Biological N fixation" = "#2D8875", 
    "Carbon sequestration"="#C2A12B"
  )
  mid_x <- length(original_order) / 2 + 0.5
  
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "black", size = 0.4) +
    geom_col(data = bar_data_long, aes(x = !!sym(name_col), y = Value, fill = Category), position = "stack") +
    geom_line(data = line_data, aes(x = !!sym(name_col), y = Line1 * scale_factor, group = 1, color = "Emission/Area (t/ha)", linetype = "Emission/Area (t/ha)"), size = 0.2) +
    geom_point(data = line_data, aes(x = !!sym(name_col), y = Line1 * scale_factor, color = "Emission/Area (t/ha)"), size = 0.3) +
    geom_line(data = line_data, aes(x = !!sym(name_col), y = Line2 * scale_factor, group = 1, color = "Emission/Yield (t/t)", linetype = "Emission/Yield (t/t)"), size = 0.2) +
    geom_point(data = line_data, aes(x = !!sym(name_col), y = Line2 * scale_factor, color = "Emission/Yield (t/t)"), size = 0.3) +
    annotate("text", x = mid_x, y = 100, label = title_text, color = "black", size = 8 / .pt, family = "Arial", fontface = "plain", vjust = 1.5) +
    scale_y_continuous(
      name = expression("GHG (Mt CO"[2]*"-eq/yr)"), limits = c(-15, 105), breaks = sort(unique(c(0, seq(-15, 105, by = 30)))), 
      expand = c(0, 0), sec.axis = sec_axis(~ . / scale_factor, name = "Intensity", breaks = seq(0, 40, by = 8))
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_manual(values = color_mapping) +
    scale_color_manual(values = c("Emission/Area (t/ha)" = light_black_color, "Emission/Yield (t/t)" = light_black_color)) +
    scale_linetype_manual(values = c("Emission/Area (t/ha)" = "solid", "Emission/Yield (t/t)" = "dashed")) +
    labs(x = name_col, fill = NULL, color = NULL, linetype = NULL) + 
    theme_minimal() + 
    theme(
      text = element_text(color = "black", family = "Arial", size = 8),
      axis.text.x = element_text(color = "black", angle = 90, size = 8, hjust = 1, vjust = 0.5, margin = margin(t = 2)), 
      axis.title.x = element_text(color = "black", size = 0, vjust = 2),
      axis.text.y = element_text(color = dark_red_color, size = 8),
      axis.title.y = element_text(color = dark_red_color, size = 8, angle = 90, margin = margin(r = 2, unit = "pt")),
      axis.ticks.y = element_line(color = dark_red_color),
      axis.text.y.right = element_text(color = light_black_color, size = 8), 
      axis.title.y.right = element_text(color = light_black_color, size = 8, angle = 90, margin = margin(l = 2, unit = "pt")),
      axis.ticks.y.right = element_line(color = light_black_color),
      plot.title = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
      axis.line = element_blank(),
      legend.position = "none" 
    )
  return(p)
}

create_perfect_legend <- function() {
  all_levels <- c(
    "Carbon sequestration", "Paddy rice", "Machinery energy", "Fertilizer application", "Manure application", 
    "Straw burning", "Straw returning", "Biological N fixation", "N leaching/runoff", "N deposition",
    "Emission/Area (t/ha)", "Emission/Yield (t/t)"
  )
  all_colors <- c(
    "#C2A12B", "#7FABD1", "#EEB6D4", "#9E4C6E", "#F16C27", 
    "#91CCC0", "#D74C4F", "#2D8875", "#585858", "#D1A1E2",
    "#404040", "#404040"
  )
  
  dummy_df <- data.frame(label = factor(all_levels, levels = all_levels), x = 1, y = 1)

  dummy_plot <- ggplot(dummy_df, aes(x = x, y = y, color = label)) +
    geom_point() +  
    geom_line() +   
    scale_color_manual(values = all_colors) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8, family = "Arial"),
      legend.key.size = unit(0.4, "lines"),
      legend.key.width = unit(0.8, "cm"),   
      legend.spacing.x = unit(0.2, "cm"),
      legend.margin = margin(t = 5, r = 5, b = 5, l = 5)
    ) +
    guides(color = guide_legend(
      ncol = 4, byrow = TRUE, title = NULL,
      override.aes = list(
        shape = c(rep(15, 10), 16, 16),
        linetype = c(rep(0, 10), 1, 2),
        size = c(rep(4, 10), 1.8, 1.8),
        linewidth = c(rep(0, 10), 0.6, 0.6)
      )
    ))
  
  return(get_legend(dummy_plot))
}

p1 <- create_plot_no_legend(data_1980_1989, "1980-1989")
p2 <- create_plot_no_legend(data_1990_1999, "1990-1999")
p3 <- create_plot_no_legend(data_2000_2009, "2000-2009")
p4 <- create_plot_no_legend(data_2010_2023, "2010-2023")

u <- "cm"
margin_tl <- margin(t = 0.3, r = 0.1, b = 0.4, l = 0.2, unit = u) 
margin_tr <- margin(t = 0.3, r = 0.2, b = 0.4, l = 0.1, unit = u)
margin_bl <- margin(t = 0, r = 0.1, b = 0.4, l = 0.2, unit = u) 
margin_br <- margin(t = 0, r = 0.2, b = 0.4, l = 0.1, unit = u)

combined_plot <- plot_grid(
  p1 + theme(plot.margin = margin_tl), 
  p2 + theme(plot.margin = margin_tr), 
  p3 + theme(plot.margin = margin_bl), 
  p4 + theme(plot.margin = margin_br), 
  ncol = 2, nrow = 2, align = 'v', axis = 'lr'
)

perfect_legend <- create_perfect_legend()

final_plot <- plot_grid(combined_plot, perfect_legend, ncol = 1, rel_heights = c(6, 1.3))

output_file <- paste0(output_dir, "/Figure2C.png")
ggsave(
  filename = output_file,
  plot = final_plot,
  width = 18, 
  height = 14, 
  units = "cm",
  dpi = 1000,
  bg = "white"
)

message("")