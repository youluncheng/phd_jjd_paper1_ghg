library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(patchwork)

FILE_PATH <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG1213/FIG1112.xlsx"
SAVE_DIR <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG11_12/" 

legend_colors <- c(
  "Fertilizer application" = "#9E4C6E", "Manure application" = "#F16C27", "Straw returning" = "#D74C4F", 
  "Paddy rice" = "#7FABD1", "Straw burning"= "#91CCC0", "Machinery energy" = "#EEB6D4", 
  "N leaching/runoff"= "#585858", "N deposition emissions"= "#D1A1E2", "Microbial N fixation" = "#2D8875"
)
expected_order <- names(legend_colors)

font_family <- "Arial" 
line_thickness <- 0.1
LEGEND_HEIGHT_CM <- 1.5 

NAT_CANVAS_SIZE_CM <- 7.5 
NAT_OUTER_D <- 7.4
NAT_INNER_D <- 3.8 
NAT_R_OUTER <- NAT_OUTER_D / 2 
NAT_R_INNER <- NAT_INNER_D / 2 
NAT_RING_WIDTH <- NAT_R_OUTER - NAT_R_INNER 
NAT_LABEL_R_POS <- (NAT_R_OUTER + NAT_R_INNER) / 2 

NAT_BASE_FONT_SIZE <- 8 
NAT_CENTER_FONT_SIZE <- 8 
NAT_LEGEND_FONT_SIZE <- 8
NAT_PIE_XLIM_MAX <- NAT_CANVAS_SIZE_CM / 2 

df_nat_raw <- read_excel(FILE_PATH, sheet = "Sheet1")
df_nat_long <- df_nat_raw %>%
  pivot_longer(cols = -c(area, year), names_to = "", values_to = "") %>%
  filter(complete.cases(.)) %>%
  mutate(措施 = factor(措施, levels = expected_order))

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

plot_national_pie <- function(data, year_id, include_legend = FALSE, label_char = NULL) {
  
  data_all <- data %>%
    filter(比例 > 0) %>%
    arrange(desc(措施)) %>%
    mutate(Prop = 比例 / sum(比例)) %>%
    mutate(cum_prop = cumsum(Prop) - Prop / 2)
  
  center_label_data <- data.frame(
    x = 0, y = 0,
    label = paste0("Mainland China\n", year_id)
  )
  
  p <- ggplot(data_all, aes(x = NAT_LABEL_R_POS, y = Prop, fill = 措施)) +
    geom_col(stat = "identity", width = NAT_RING_WIDTH, color = "white", linewidth = line_thickness) +

    
    coord_polar(theta = "y", start = -pi/2) +
    xlim(0, NAT_PIE_XLIM_MAX) + 

    geom_text(data = center_label_data, aes(x = 0, y = 0, label = label),
              color = "black", 
              size = NAT_CENTER_FONT_SIZE / .pt, 
              family = font_family, 
              fontface = "plain", 
              inherit.aes = FALSE) +
    
    scale_fill_manual(
      values = legend_colors, 
      name = NULL, 
      breaks = expected_order,
      guide = guide_legend(nrow = 3) 
    ) +
    
    labs(tag = label_char) +
    
    theme_void() +
    theme(
      text = element_text(family = font_family, size = NAT_BASE_FONT_SIZE, color = "black"),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"), 
      
      legend.position = ifelse(include_legend, "bottom", "none"),
      legend.text = element_text(size = NAT_LEGEND_FONT_SIZE, family = font_family, color = "black"), 
      legend.key.size = unit(0.4, "cm"),
      
      plot.tag = element_text(
        family = font_family, 
        size = 8,               
        face = "plain", 
        color = "black"        
      ),
      plot.tag.position = c(0.05, 0.98) 
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
  p_temp_legend <- plot_national_pie(df_nat_first, first_year, include_legend = TRUE, label_char = NULL)
  legend_obj <- g_legend(p_temp_legend)
}

my_letters <- c("a", "b", "c", "d")

for (i in seq_along(years_to_plot)) { 
  current_year <- years_to_plot[i]
  current_letter <- my_letters[i] 
  
  df_nat_year <- df_nat_long %>% filter(year == current_year)
  
  if (nrow(df_nat_year) > 0) {
    national_plots_list[[as.character(current_year)]] <- plot_national_pie(
      df_nat_year, 
      current_year, 
      include_legend = FALSE, 
      label_char = current_letter 
    )
  }
}

p_grid_national <- wrap_plots(national_plots_list, ncol = 2, nrow = 2)

if (!is.null(legend_obj)) {
  p_final_combined <- wrap_elements(legend_obj) / p_grid_national + 
    plot_layout(heights = c(LEGEND_HEIGHT_CM, NAT_CANVAS_SIZE_CM * 2)) 
} else {
  p_final_combined <- p_grid_national
}

ggsave(filename = paste0(SAVE_DIR, "SI_bingtu_national_no_percent.png"), 
       plot = p_final_combined,
       width = NAT_CANVAS_SIZE_CM * 2, 
       height = (NAT_CANVAS_SIZE_CM * 2) + LEGEND_HEIGHT_CM, 
       units = "cm", 
       dpi = 1000)












library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(patchwork) 
library(grid) 

FILE_PATH <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG1213/FIG1112.xlsx"
SAVE_DIR  <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG11_12"

if (!dir.exists(SAVE_DIR)) dir.create(SAVE_DIR, recursive = TRUE)

legend_colors <- c(
  "Fertilizer application" = "#9E4C6E", "Manure application" = "#F16C27", "Straw returning" = "#D74C4F", 
  "Paddy rice" = "#7FABD1", "Straw burning"= "#91CCC0", "Machinery energy" = "#EEB6D4", 
  "N leaching/runoff"= "#585858", "N deposition emissions"= "#D1A1E2", "Microbial N fixation" = "#2D8875"
)
expected_order <- names(legend_colors)

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

plot_pie_chart <- function(data, area_id, year_id) {
  
  data_all <- data %>%
    filter(比例 > 0) %>%
    arrange(desc(措施)) %>%
    mutate(Prop = 比例 / sum(比例)) %>%
    mutate(cum_prop = cumsum(Prop) - Prop / 2)
  
  center_label_data <- data.frame(x = 0, y = 0, label = paste0("Area ", area_id, "\n", year_id))
  
  p <- ggplot(data_all, aes(x = LABEL_R_POS, y = Prop, fill = 措施)) +
    geom_col(width = RING_WIDTH, color = "white", linewidth = LINE_THICKNESS) +
    
    coord_polar(theta = "y", start = -pi/2, clip = "off") + 
    xlim(0, PIE_XLIM_MAX) +

    geom_text(data = center_label_data, aes(x = 0, y = 0, label = label),
              color = FONT_COLOR, size = UNIFIED_FONT_SIZE / .pt, family = FONT_FAMILY,
              fontface = "plain", inherit.aes = FALSE) +
    
    scale_fill_manual(values = legend_colors, name = NULL, breaks = expected_order) +
    
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0, "pt"),
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = UNIFIED_FONT_SIZE, color = FONT_COLOR, family = FONT_FAMILY)
    )
  return(p)
}

df_raw <- read_excel(FILE_PATH, sheet = "Sheet2")
df_long <- df_raw %>%
  pivot_longer(cols = -c(area, year), names_to = "", values_to = "") %>%
  filter(complete.cases(.)) %>%
  mutate(措施 = factor(措施, levels = expected_order))

unique_areas <- sort(unique(df_long$area))
unique_years <- sort(unique(df_long$year))

area_rows <- list()

for (i in seq_along(unique_areas)) {
  a_id <- unique_areas[i]
  row_plots <- list()
  for (y_id in unique_years) {
    df_sub <- df_long %>% filter(area == a_id, year == y_id)
    row_plots[[as.character(y_id)]] <- plot_pie_chart(df_sub, a_id, y_id)
  }
  area_rows[[as.character(a_id)]] <- wrap_plots(row_plots, nrow = 1, ncol = 4) &
    theme(plot.margin = margin(t = 0, r = -5, b = 0, l = -5, unit = "pt"))
}

p_grid_all <- wrap_plots(area_rows, ncol = 1) & 
  theme(plot.margin = margin(t = -5, r = 0, b = -5, l = 0, "pt"))

p_final <- p_grid_all + 
  plot_layout(guides = "collect") +
  plot_layout(heights = unit(c(rep(1, length(area_rows))), "null")) & 
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(t = 0, b = -10, r = 0, l = 0), 
    legend.text = element_text(size = UNIFIED_FONT_SIZE, family = FONT_FAMILY),
    plot.margin = margin(t = 10, r = 2, b = 2, l = 2, "pt") 
  ) &
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

out_file <- file.path(SAVE_DIR, "area_final_no_percent.png")
ggsave(out_file, p_final, width = 18, height = 25, units = "cm", dpi = 600)
