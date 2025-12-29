


#########Mainland China

library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(patchwork) 




FILE_PATH <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG11_12/FIG1112.xlsx"

SAVE_DIR <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG11_12" 


legend_colors <- c(
  "Fertilizer Application" = "#BD7795",
  "Manure Application"= "#F39865",
  "Straw Burning" = "#91CCC0",
  "Irrigation/Runoff" = "#7C7979",
  "Paddy Rice Cultivation" = "#7FABD1",
  "Irrigation Energy" = "#EEB6D4",
  "Agricultural Machinery Energy" = "#2D8875",
  "Straw Returning" = "#EC6E66" 
)
expected_order <- names(legend_colors)
font_family <- "ArialMT"
line_thickness <- 0.1
LEGEND_HEIGHT_CM <- 1.0 


NAT_CANVAS_SIZE_CM <- 7.5 
NAT_OUTER_D <- 7.4
NAT_INNER_D <- 3.8 
NAT_R_OUTER <- NAT_OUTER_D / 2 
NAT_R_INNER <- NAT_INNER_D / 2 
NAT_RING_WIDTH <- NAT_R_OUTER - NAT_R_INNER 
NAT_LABEL_R_POS <- (NAT_R_OUTER + NAT_R_INNER) / 2 


NAT_BASE_FONT_SIZE <- 8 
NAT_CENTER_FONT_SIZE <- 10 
NAT_LEGEND_FONT_SIZE <- 8

NAT_PIE_XLIM_MAX <- NAT_CANVAS_SIZE_CM / 2 


df_nat_raw <- read_excel(FILE_PATH, sheet = "Sheet1")
df_nat_long <- df_nat_raw %>%
  pivot_longer(
    cols = -c(area, year),
    names_to = "措施",
    values_to = "比例"
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(措施 = factor(措施, levels = expected_order))


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



plot_national_pie <- function(data, year_id, include_legend = FALSE) {
  
  data_all <- data %>%
    filter(比例 > 0) %>%
    arrange(desc(措施)) %>%
    mutate(Prop = 比例 / sum(比例)) %>%
    mutate(
      cum_prop = cumsum(Prop) - Prop / 2, 
      label_text = paste0(round(Prop * 100, 1), "%")
    )
  
  
  data_all_labels <- data_all %>% filter(Prop >= 0.0001)
  
  center_label_data <- data.frame(
    x = 0, y = 0,
    label = paste0("National\n", year_id)
  )
  
  p <- ggplot(data_all, aes(x = NAT_LABEL_R_POS, y = Prop, fill = 措施)) +
    geom_col(
      stat = "identity",
      width = NAT_RING_WIDTH, 
      color = "white",
      linewidth = line_thickness
    ) +
    
  
    geom_text(
      data = data_all_labels,
      aes(x = NAT_LABEL_R_POS, y = cum_prop, label = label_text), 
      color = "black",
      size = NAT_BASE_FONT_SIZE / .pt, 
      family = font_family,
      inherit.aes = FALSE
    ) +
    
    coord_polar(theta = "y", start = -pi/2) +
    xlim(0, NAT_PIE_XLIM_MAX) + 
    
    geom_text(data = center_label_data,
              aes(x = 0, y = 0, label = label),
              color = "black",
              size = NAT_CENTER_FONT_SIZE / .pt, 
              family = font_family,
              fontface = "bold", 
              inherit.aes = FALSE) +
    
    scale_fill_manual(
      values = legend_colors, 
      name = NULL, 
      breaks = expected_order
    ) +
    
    theme_void() +
    theme(
      text = element_text(family = font_family, size = NAT_BASE_FONT_SIZE),
      plot.margin = unit(c(0, 0, 0, 0), "cm"), 
      
      
      legend.position = ifelse(include_legend, "bottom", "none"),
      legend.text = element_text(size = NAT_LEGEND_FONT_SIZE), 
      legend.spacing.x = unit(0.2, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"), 
      legend.spacing.y = unit(0, "cm") 
    )
  
  return(p)
}




unique_years <- unique(df_nat_long$year) %>% sort()


years_to_plot <- unique_years[1:min(4, length(unique_years))]

national_plots_list <- list()
dir.create(SAVE_DIR, showWarnings = FALSE, recursive = TRUE)


legend_obj <- NULL
if (length(unique_years) > 0) {
  first_year <- unique_years[1]
  df_nat_first <- df_nat_long %>% filter(year == first_year)
  if (nrow(df_nat_first) > 0) {
    p_temp_legend <- plot_national_pie(df_nat_first, first_year, include_legend = TRUE)
    legend_obj <- g_legend(p_temp_legend)
  }
}



for (current_year in years_to_plot) { 
  
  cat(paste("Processing National Pie for Year:", current_year, "\n"))
  
  df_nat_year <- df_nat_long %>% filter(year == current_year)
  if (nrow(df_nat_year) > 0) {
    p_national <- plot_national_pie(df_nat_year, current_year, include_legend = FALSE)
    national_plots_list[[as.character(current_year)]] <- p_national
  } else {
    
    national_plots_list[[as.character(current_year)]] <- ggplot() + 
      labs(title = paste0(current_year, "\nData Missing")) + 
      theme_void()
  }
}


if (length(national_plots_list) > 0) {
  
 
  p_grid_national <- wrap_plots(national_plots_list, ncol = 2, nrow = 2)
  
  
  FINAL_WIDTH_CM <- NAT_CANVAS_SIZE_CM * 2 # 7.5 * 2 = 15 cm
  FINAL_HEIGHT_CM <- NAT_CANVAS_SIZE_CM * 2 # 7.5 * 2 = 15 cm
  
  p_final_combined <- p_grid_national 
  
  if (!is.null(legend_obj)) {
    
    p_legend_wrapped <- wrap_elements(legend_obj)
    
   
    p_final_combined <- p_legend_wrapped / p_grid_national + 
      
      plot_layout(heights = c(LEGEND_HEIGHT_CM, FINAL_HEIGHT_CM)) 
    
   
    FINAL_HEIGHT_CM <- FINAL_HEIGHT_CM + LEGEND_HEIGHT_CM 
  } 
  
  final_file_name <- paste0(SAVE_DIR, "SI_bingtu_national.png")
  
  ggsave(
    filename = final_file_name,
    plot = p_final_combined,
    width = FINAL_WIDTH_CM, 
    height = FINAL_HEIGHT_CM, 
    units = "cm",
    dpi = 1000,
    create.dir = TRUE
  )
  
  cat("\n=================================================================\n")
  cat("Finished generating final combined chart (V50 - Bold Center & No Offset).\n")
  cat(paste("中心标签已设置加粗，并移除了 'Straw Returning' 的标签偏移。\n"))
  cat(paste("FINAL PLOT DIMENSIONS: ", FINAL_WIDTH_CM, "cm (Width) x ", FINAL_HEIGHT_CM, "cm (Height).\n"))
  cat("Charts saved to: ", SAVE_DIR, "\n")
  cat("=================================================================\n")
} else {
  cat("\n=================================================================\n")
  cat("Error: No national data found for combination.\n")
  cat("=================================================================\n")
}










###################12##############
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(patchwork) 
library(grid) 
library(ggrepel)


FILE_PATH <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG11_12/FIG1112.xlsx"
SAVE_DIR  <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG11_12"

if (!dir.exists(SAVE_DIR)) dir.create(SAVE_DIR, recursive = TRUE)


legend_colors <- c(
  "Fertilizer Application" = "#BD7795",
  "Manure Application"= "#F39865",
  "Straw Burning" = "#91CCC0",
  "Leaching/Runoff" = "#7C7979",
  "Paddy Rice Cultivation" = "#7FABD1",
  "Irrigation Energy" = "#EEB6D4",
  "Agricultural Machinery Energy" = "#2D8875",
  "Straw Returning" = "#EC6E66"
)
expected_order <- names(legend_colors)


TAG_FONT_SIZE     <- 10   
UNIFIED_FONT_SIZE <- 8    
FONT_COLOR        <- "black"
FONT_FAMILY       <- "Arial"
LINE_THICKNESS    <- 0.1


OUTER_DIAMETER_CM <- 4.2      
R_OUTER           <- OUTER_DIAMETER_CM / 2 
R_INNER           <- 2.0 / 2  
RING_WIDTH        <- R_OUTER - R_INNER 
LABEL_R_POS       <- (R_OUTER + R_INNER) / 2 
PIE_XLIM_MAX      <- R_OUTER   


plot_pie_chart <- function(data, area_id, year_id, include_legend = FALSE) {
  data_all <- data %>%
    filter(比例 > 0) %>%
    arrange(desc(措施)) %>%
    mutate(Prop = 比例 / sum(比例)) %>%
    mutate(cum_prop = cumsum(Prop) - Prop / 2,
           label_text = paste0(round(Prop * 100, 1), "%"))
  
  center_label_data <- data.frame(x = 0, y = 0, label = paste0("Area ", area_id, "\n", year_id))
  
  p <- ggplot(data_all, aes(x = LABEL_R_POS, y = Prop, fill = 措施)) +
    geom_col(width = RING_WIDTH, color = "white", linewidth = LINE_THICKNESS) +
    
   
    geom_text_repel(
      aes(x = LABEL_R_POS, y = cum_prop, label = label_text),
      color = FONT_COLOR,
      size = UNIFIED_FONT_SIZE / .pt,
      family = FONT_FAMILY,
      inherit.aes = FALSE,
      nudge_x = 0.3,           
      direction = "y",         
      segment.size = 0.2,      
      segment.color = "grey50",
      min.segment.length = 0,  
      box.padding = 0.05,       
      max.overlaps = Inf       
    ) +
    
    coord_polar(theta = "y", start = -pi/2, clip = "off") + 
    xlim(0, PIE_XLIM_MAX) +
    
    # 中心文字 (8pt)
    geom_text(data = center_label_data, aes(x = 0, y = 0, label = label),
              color = FONT_COLOR, size = UNIFIED_FONT_SIZE / .pt, family = FONT_FAMILY,
              fontface = "plain", inherit.aes = FALSE) +
    
    scale_fill_manual(values = legend_colors, name = NULL, breaks = expected_order) +
    theme_void() +
    theme(
      legend.position = ifelse(include_legend, "bottom", "none"),
      legend.text = element_text(size = UNIFIED_FONT_SIZE, color = FONT_COLOR, family = FONT_FAMILY),
      plot.margin = margin(0, 0, 0, 0, "pt")
    )
  return(p)
}


df_raw <- read_excel(FILE_PATH, sheet = "Sheet2")
df_long <- df_raw %>%
  pivot_longer(cols = -c(area, year), names_to = "措施", values_to = "比例") %>%
  filter(complete.cases(.)) %>%
  mutate(措施 = factor(措施, levels = expected_order))

unique_areas <- sort(unique(df_long$area))
unique_years <- sort(unique(df_long$year))

temp_p <- plot_pie_chart(df_long %>% filter(area == unique_areas[1], year == unique_years[1]), 
                         unique_areas[1], unique_years[1], include_legend = TRUE) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  return(tmp$grobs[[leg]])
}
legend_obj <- g_legend(temp_p)

area_rows <- list()
for (a_id in unique_areas) {
  row_plots <- list()
  for (y_id in unique_years) {
    df_sub <- df_long %>% filter(area == a_id, year == y_id)
    row_plots[[as.character(y_id)]] <- plot_pie_chart(df_sub, a_id, y_id)
  }
  area_rows[[as.character(a_id)]] <- wrap_plots(row_plots, nrow = 1, ncol = 4)
}


tags <- letters[1:6] 
for(i in seq_along(area_rows)){
  area_rows[[i]][[1]] <- area_rows[[i]][[1]] + 
    labs(tag = tags[i]) +
    theme(
      
      plot.tag = element_text(size = TAG_FONT_SIZE, face = "bold", color = FONT_COLOR, family = FONT_FAMILY),
      plot.tag.position = c(0.01, 0.99)
    )
}


p_grid_all <- wrap_plots(area_rows, ncol = 1) & 
  theme(plot.margin = margin(-2, -2, -2, -2, "pt")) 

p_final <- wrap_elements(legend_obj) / p_grid_all + 
  plot_layout(heights = c(1, 20)) &
  theme(plot.margin = margin(0, 0, 0, 0, "pt")) 


out_file <- file.path(SAVE_DIR, "area_final_10pt_tags.png")

ggsave(out_file, p_final, width = 18, height = 24, units = "cm", dpi = 600)

cat("绘图已完成。标签为 10pt，其他字号为 8pt，保存至：", out_file, "\n")