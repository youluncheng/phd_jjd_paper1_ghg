
#####################A#####################A
#####################A#####################A
#####################A#####################A

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG1/FIG1_v3.xlsx"

yield_raw <- read_excel(file_path, sheet = "Sheet1", range = cell_cols("A:B"))
colnames(yield_raw) <- c("Year", "Yield")

emissions_raw <- read_excel(file_path, sheet = "Sheet1", range = cell_cols("D:F"))
colnames(emissions_raw) <- c("Year_E", "Net_emission", "Emission")

combined_data <- yield_raw %>%
  mutate(Year = as.numeric(Year)) %>%
  full_join(mutate(emissions_raw, Year_E = as.numeric(Year_E)), by = c("Year" = "Year_E")) %>%
  filter(Year >= 1980 & Year <= 2023)

y_left_min <- 250; y_left_max <- 650
y_right_min <- 600; y_right_max <- 950
scale_factor <- (y_left_max - y_left_min) / (y_right_max - y_right_min)

p_final <- ggplot(combined_data, aes(x = Year)) +

  geom_line(aes(y = Yield, color = "Crop Yield"), linewidth = 0.5) +
  geom_point(aes(y = Yield, color = "Crop Yield"), size = 0.6) +

  geom_line(aes(y = (Emission - y_right_min) * scale_factor + y_left_min, color = "Total Emission"), 
            linewidth = 0.5) +
  geom_point(aes(y = (Emission - y_right_min) * scale_factor + y_left_min, color = "Total Emission"), 
             size = 0.6) +
  
  geom_line(aes(y = (Net_emission - y_right_min) * scale_factor + y_left_min, color = "Net Emission"), 
            linetype = "dotted", linewidth = 0.5) +
  geom_point(aes(y = (Net_emission - y_right_min) * scale_factor + y_left_min, color = "Net Emission"), 
             size = 0.6) +

  scale_color_manual(
    values = c("Crop Yield" = "#90A4C4", "Total Emission" = "#F5B9AB", "Net Emission" = "#F5B9AB"),
    breaks = c("Crop Yield", "Total Emission", "Net Emission")
  ) +

  scale_y_continuous(
    name = "Crop Yield (Mt)",
    breaks = seq(250, 650, 100),
    limits = c(y_left_min, y_left_max + 20),
    expand = c(0, 0),
    sec.axis = sec_axis(
      trans = ~ (. - y_left_min) / scale_factor + y_right_min,
      name = expression("GHG emissions (Mt CO" [2] * "-eq)"),
      breaks = seq(600, 950, 50)
    )
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = c(1980, 1990, 2000, 2010, 2023),
    limits = c(1980, 2023),
    expand = c(0.01, 0.01),
    sec.axis = dup_axis(name = NULL, labels = NULL)
  ) +

  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    text = element_text(size = 8, family = "ArialMT"),
    
    axis.title.y.left = element_text(color = "black", size = 8),
    axis.text.y.left = element_text(color = "black", size = 8),
    axis.ticks.y.left = element_line(color = "black", linewidth = 0.5),
    
    axis.title.y.right = element_text(color = "black", size = 8, angle = 90, vjust = 0.5),
    axis.text.y.right = element_text(color = "black", size = 8),
    axis.ticks.y.right = element_line(color ="black", linewidth = 0.5),
    
    axis.text.x = element_text(color = "black", size = 8),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x.top = element_blank(), 

    legend.position = c(0, 1),
    legend.justification = c("left", "top"),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.2, "cm"),   
    legend.spacing.y = unit(-0.05, "cm"),   
    
    axis.line = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm")
  )

# 保存
ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1A.png",
       p_final, width = 6, height = 5, units = "cm", dpi = 300)

print(p_final)








#####################B#####################B
#####################B#####################B
#####################B#####################B
library(ggplot2)
library(readxl)
library(dplyr)
library(grid)


file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG1/FIG1_v3.xlsx"
data <- read_excel(file_path, sheet = "Sheet2")
required_columns <- c("name", "Value", "begin", "end")
if (!all(required_columns %in% colnames(data))) {
  missing <- setdiff(required_columns, colnames(data))
  stop(paste("缺少必要的列：", paste(missing, collapse = ", ")))
}

data <- data %>%
  mutate(
    original_order = 1:n(),
    is_baseline = grepl("1980|1990|2000|2010|2023", name)
  )

y_min <- 0
y_max <- 120  
x_min <- 0.5
x_max <- max(data$original_order) + 0.5

baseline_info <- data %>%
  filter(is_baseline) %>%
  select(original_order, name) %>%
  arrange(original_order)

p <- ggplot(data, aes(x = original_order)) +
  geom_rect(data = filter(data, is_baseline),
            aes(xmin = original_order - 0.4, xmax = original_order + 0.4,
                ymin = begin, ymax = end),
            fill = "#C0A12B", color = NA, alpha = 0.8) +
  geom_text(data = filter(data, is_baseline),
            aes(y = end + 3, label = sprintf("%.0f", round(end, 0))),
            size = 8 / (72 / 25.4), # 严格 8pt
            color = "black", family = "ArialMT",
            angle = 0, hjust = 0.5, vjust = 0) +
  
  geom_segment(data = filter(data, !is_baseline),
               aes(x = original_order - 1 + 0.4, 
                   xend = original_order + 1 - 0.4,
                   y = begin, yend = begin),
               linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_segment(data = filter(data, !is_baseline),
               aes(x = original_order - 1 + 0.4, 
                   xend = original_order + 1 - 0.4,
                   y = end, yend = end),
               linetype = "dashed", color = "black", linewidth = 0.4) +

  geom_text(data = filter(data, !is_baseline),
            aes(x = original_order, 
                y = (begin + end) / 2, 
                label = sprintf("%.0f", round(Value, 0))),
            size = 8 / (72 / 25.4), color = "black", family = "ArialMT",
            angle = 0, vjust = 0.5) +
  geom_text(data = filter(data, !is_baseline),
            aes(x = original_order, 
                y = pmin(begin, end) - 2, 
                label = ifelse(Value >= 0, "↑", "↓")),
            size = 8 / (72 / 25.4), color = "black", family = "ArialMT",
            angle = 0, vjust = 1) +
  scale_y_continuous(
    name = expression("Carbon sequestration (Mt CO" [2] * "-eq)"),
    breaks = seq(0, 120, 30), # 0, 30, 60, 90, 120
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "",
    breaks = baseline_info$original_order,
    labels = baseline_info$name
  ) +
  coord_cartesian(
    ylim = c(y_min, y_max),
    xlim = c(x_min, x_max),
    expand = FALSE
  ) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text = element_text(size = 8, color = "black", family = "ArialMT"),
    axis.title.y = element_text(size = 8, color = "black", family = "ArialMT"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y.right = element_blank(),
    axis.title.y.left = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(hjust = 0.5, vjust = 1),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10, unit = "pt")
  )
print(p)

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1B.png",
       p, width = 6, height = 5, units = "cm", dpi = 300)


#############################CCCCCCCC
#############################CCCCCCCC
#############################CCCCCCCC
library(ggplot2)
library(readxl)
library(dplyr)
library(grid)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG1/FIG1_v3.xlsx"
data <- read_excel(file_path, sheet = "Sheet3")

required_columns <- c("name", "Value", "begin", "end", "type")
if (!all(required_columns %in% colnames(data))) {
  missing <- setdiff(required_columns, colnames(data))
  stop(paste("：", paste(missing, collapse = ", ")))
}

data <- data %>%
  mutate(
    original_order = 1:n(),
    fill_group = case_when(
      grepl("1980|1990|2000|2010|2023", name) ~ "1980-2023 Baseline",
      grepl("Fertilizer application", name, ignore.case = TRUE) ~ "Fertilizer application",
      grepl("Manure application", name, ignore.case = TRUE) ~ "Manure application",
      grepl("Straw burning", name, ignore.case = TRUE) ~ "Straw burning",
      grepl("Straw returning", name, ignore.case = TRUE) ~ "Straw returning",
      grepl("Paddy rice", name, ignore.case = TRUE) ~ "Paddy rice",
      grepl("Machinery energy", name, ignore.case = TRUE) ~ "Machinery energy",
      grepl("Biological N fixation", name, ignore.case = TRUE) ~ "Biological N fixation",
      grepl("N leaching/runoff", name, ignore.case = TRUE) ~ "N leaching/runoff",
      grepl("N deposition", name, ignore.case = TRUE) ~ "N deposition",
      TRUE ~ "Other"
    )
  )

legend_colors <- c(
  "Fertilizer application" = "#9E4C6E",
  "Manure application" = "#F16C27",
  "Straw returning" = "#D74C4F",
  "Paddy rice" = "#7FABD1",
  "Straw burning"= "#91CCC0",
  "Machinery energy" = "#EEB6D4",
  "Biological N fixation" = "#2D8875",
  "N leaching/runoff"= "#585858",
  "N deposition"= "#D1A1E2",
  "1980-2023 Baseline" = "#9CA3AF"
)

valid_entries <- names(legend_colors)[names(legend_colors) != "1980-2023 Baseline"]
full_breaks <- c(
  valid_entries[1:3],           
  valid_entries[4:6],          
  valid_entries[7:8], "b1",    
  valid_entries[9], "b2", "b3" 
full_colors <- c(legend_colors, "b1" = "transparent", "b2" = "transparent", "b3" = "transparent")

y_min <- 600
y_max <- 1050
x_max <- max(data$original_order) + 0.2
x_min <- 0

label_data <- data %>% 
  filter(fill_group %in% names(legend_colors)) %>%
  mutate(label_y = ifelse(type == 1, end + 25, ifelse(type == 2, end - 25, NA)))

baseline_info <- data %>%
  filter(name %in% c("1980", "1990", "2000", "2010", "2023")) %>%
  select(original_order, name) %>%
  arrange(original_order)
p <- ggplot(data, aes(x = factor(original_order, levels = original_order))) +
  geom_rect(aes(xmin = original_order - 0.4, xmax = original_order + 0.4,
                ymin = begin, ymax = end, fill = fill_group),
            color = NA, alpha = 0.8) +
  geom_point(data = data %>% filter(type == 1),
             aes(x = original_order, y = end + 5, shape = factor(type)),
             size = 0.5, color = "black", fill = "black") +
  geom_point(data = data %>% filter(type == 2),
             aes(x = original_order, y = end - 5, shape = factor(type)),
             size = 0.5, color = "black", fill = "black") +
  geom_text(
    data = label_data,
    aes(x = original_order, y = label_y, label = sprintf("%.1f", Value)),
    size = 8/2.85, color = "black", family = "ArialMT",
    angle = 90, hjust = 0.5, vjust = 0.5
  ) +
  scale_shape_manual(values = c("1" = 24, "2" = 25), guide = "none") +
  scale_fill_manual(
    values = full_colors,
    breaks = full_breaks,
    labels = function(x) ifelse(grepl("^b[0-9]", x), "", x),
    guide = guide_legend(ncol = 3, byrow = TRUE, title = NULL)
  ) +
  scale_y_continuous(
    name = expression("GHG emissions (Mt CO" [2] * "-eq)"),
    breaks = seq(400, y_max, 50),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    name = "",
    breaks = baseline_info$original_order,
    labels = baseline_info$name
  ) +
  coord_cartesian(
    ylim = c(y_min, y_max),
    xlim = c(x_min, x_max)
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.01, 0.99),
    legend.justification = c("left", "top"),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, family = "ArialMT"),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.x = unit(0.1, "lines"),
    
    axis.text.y = element_text(size = 8, color = "black", family = "ArialMT"),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.title.y = element_text(size = 8, family = "ArialMT"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y.right = element_blank(),
    
    axis.text.x = element_text(size = 8, color = "black", family = "ArialMT", hjust = 0.5, vjust = 1),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_blank(),
    
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")
  )

print(p)

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1C.png",
       p, width = 12, height = 10, units = "cm", dpi = 300)















library(magick)


path_a <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1A.png' 
path_b <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1C.png' 
path_c <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1B.png'
path_d <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1D.png' 
output_path <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/FIG1_Combined_Final.png'

dpi <- 300
target_width_cm <- 18
target_width_px <- round((target_width_cm / 2.54) * dpi)

img_a <- image_read(path_a)
img_b <- image_read(path_b)
img_c <- image_read(path_c)
img_d <- image_read(path_d)
add_label <- function(img, label_text) {
  image_annotate(img, label_text, 
                 size = 50,          
                 font = "Arial", 
                 weight = 700,       
                 location = "+10+10", 
                 color = "black")
}

img_a_labeled <- add_label(img_a, "a")
img_b_labeled <- add_label(img_b, "b")
img_c_labeled <- add_label(img_c, "c")
img_d_labeled <- add_label(img_d, "d")

left_width <- round(target_width_px * 0.5)
img_a_res <- image_scale(img_a_labeled, as.character(left_width))
img_c_res <- image_scale(img_c_labeled, as.character(left_width))
left_column <- image_append(c(img_a_res, img_c_res), stack = TRUE)

info_left <- image_info(left_column)
img_b_res <- image_scale(img_b_labeled, paste0("x", info_left$height))
top_combined <- image_append(c(left_column, img_b_res), stack = FALSE)

top_res <- image_scale(top_combined, as.character(target_width_px))
img_d_res <- image_scale(img_d_labeled, as.character(target_width_px))
final_combined <- image_append(c(top_res, img_d_res), stack = TRUE)


final_combined <- image_background(final_combined, "white")

image_write(final_combined, path = output_path, density = dpi)

cat("", output_path)