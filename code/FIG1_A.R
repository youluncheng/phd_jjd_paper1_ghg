library(ggplot2)
library(readxl)
library(dplyr)
library(grid)


file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG1/FIG1_v3.xlsx"
data <- read_excel(file_path, sheet = "Sheet3")

required_columns <- c("name", "Value", "begin", "end", "type")
if (!all(required_columns %in% colnames(data))) {
  missing <- setdiff(required_columns, colnames(data))
  stop(paste("缺少必要的列：", paste(missing, collapse = ", ")))
}


data <- data %>%
  mutate(
    original_order = 1:n(),
    fill_group = case_when(
      grepl("1980|1990|2000|2010|2023", name) ~ "1980-2023 Baseline",
      grepl("Fertilizer Application", name, ignore.case = TRUE) ~ "Fertilizer Application",
      grepl("Manure Application", name, ignore.case = TRUE) ~ "Manure Application",
      grepl("Straw Returning", name, ignore.case = TRUE) ~ "Straw Returning",
      grepl("Straw Burning", name, ignore.case = TRUE) ~ "Straw Burning",
      grepl("Irrigation Energy", name, ignore.case = TRUE) ~ "Irrigation Energy",
      grepl("Leaching/Runoff", name, ignore.case = TRUE) ~ "Leaching/Runoff",
      grepl("Paddy Rice Cultivation", name, ignore.case = TRUE) ~ "Paddy Rice Cultivation",
      grepl("Agricultural Machinery Energy", name, ignore.case = TRUE) ~ "Agricultural Machinery Energy",
      TRUE ~ "Other"
    )
  )


legend_colors <- c(
  "1980-2023 Baseline" = "#9CA3AF",
  "Fertilizer Application" = "#BD7795",
  "Manure Application" = "#F39865",
  "Straw Returning" = "#EC6E66",
  "Straw Burning"= "#91CCC0",
  "Leaching/Runoff"= "#7C7979",
  "Paddy Rice Cultivation" = "#7FABD1",
  "Irrigation Energy" = "#EEB6D4",
  "Agricultural Machinery Energy" = "#2D8875"
)

target_groups <- names(legend_colors)
label_data <- data %>% filter(fill_group %in% target_groups)


y_min <- 600
y_max <- 1100
x_max <- max(data$original_order) + 0.2
x_min <- -0

#  8pt


label_data <- label_data %>%
  mutate(
    label_y = ifelse(type == 1, end + 30, end - 30)
  )


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
    aes(
      x = original_order,
      y = label_y,
      label = sprintf("%.1f", Value)
    ),
    size = 8/2.85, 
    color = "black",
    family = "ArialMT",
    angle = 90,          
    hjust = 0.5,         
    vjust = 0.5          
  ) +
  scale_shape_manual(values = c("1" = 24, "2" = 25), guide = "none") +
  scale_fill_manual(
    values = legend_colors,
    breaks = c(
      "Agricultural Machinery Energy",
      "Manure Application",
      "Fertilizer Application",
      "Paddy Rice Cultivation",
      "Irrigation Energy",
      "Straw Burning",
      "Leaching/Runoff",
      "Straw Returning"
    ),
    labels = c(
      "Agricultural Machinery Energy",
      "Manure Application",
      "Fertilizer Application",
      "Paddy Rice Cultivation",
      "Irrigation Energy",
      "Straw Burning",
      "Leaching/Runoff",
      "Straw Returning"
    ),
    guide = guide_legend(nrow = 2, title = NULL)
  )


baseline_info <- data %>%
  filter(name %in% c("1980", "1990", "2000", "2010", "2023")) %>%
  select(original_order, name) %>%
  arrange(original_order)


p <- p +
  scale_y_continuous(
    name = expression("GHG emissions (Mt CO" [2] * "-eq)"),
    breaks = seq(400, y_max, 50)[seq(400, y_max, 50) != 1100],
    expand = c(0, 0),
    position = "left"
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
 
  annotate("segment", x = x_min, xend = x_max,
           y = y_min, yend = y_min,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = "black") +
  annotate("segment", x = x_min, xend = x_min,
           y = y_min, yend = y_max,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = "black") +
  theme(
    
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    
   
    legend.background = element_blank(),
    legend.key = element_blank(), 
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, family = "ArialMT"),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.x = unit(0.1, "lines"),
    legend.box.margin = margin(t = 0, r = 0, b = -5, l = 0),
    
    
    axis.text.y = element_text(size = 8, color = "black", family = "ArialMT"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y.right = element_blank(),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.title.y = element_text(size = 8, family = "ArialMT"),
    

    axis.text.x = element_text(
      size = 8,
      color = "black",
      family = "ArialMT",
      hjust = 0.5,
      vjust = 1
    ),
    axis.ticks.x = element_line(
      color = "black", 
      linewidth = 0.5
    ),
    axis.title.x = element_blank(),
    axis.line = element_blank(),
    
    
    plot.margin = margin(0.05, 0.05, 0.05, 0.05, unit = "cm")
  )


print(p)


ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1A.png",
       p, width = 18, height = 10, units = "cm", dpi = 300)







####combine####combine####combine####combine########combine####combine####combine####combine####
####combine####combine####combine####combine########combine####combine####combine####combine####
library(magick)
library(grDevices) 


image1_path <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/FIG1B.png'
image2_path <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/Figure1A.png'
output_path_png <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/FIG1combine.png'
temp_label_A_path <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/temp_label_A.png'
temp_label_B_path <- '/Users/dongjingjing/Desktop/GHG/FIG/FIG1/temp_label_B.png'


img1 <- image_read(image1_path)
img2 <- image_read(image2_path)


info1 <- image_info(img1)
info2 <- image_info(img2)

target_width_px <- max(info1$width, info2$width)

img1_resized <- image_scale(img1, paste0(target_width_px, "x"))
img2_resized <- image_scale(img2, paste0(target_width_px, "x"))

img2_height_px <- image_info(img2_resized)$height

dpi <- 300
space_height_cm <- 1 
space_height_px <- round((space_height_cm / 2.54) * dpi)

spacer_strip <- image_blank(width = target_width_px, height = space_height_px, color = "transparent")

combined_image_no_bg <- image_append(c(img2_resized, spacer_strip, img1_resized), stack = TRUE)

combined_info <- image_info(combined_image_no_bg)
total_width_px <- combined_info$width 
total_height_px <- combined_info$height 

start_color <- "#FFFFFF"
end_color   <- "#E8EDF5"
get_gradient_colors <- colorRampPalette(c(start_color, end_color))

gradient_colors <- get_gradient_colors(total_height_px)
gradient_raster <- as.raster(matrix(gradient_colors, ncol = 1))
gradient_strip <- image_read(gradient_raster)

gradient_canvas <- image_scale(gradient_strip, paste0(total_width_px, "x", total_height_px, "!"))

final_image <- image_composite(
  image = gradient_canvas,
  composite_image = combined_image_no_bg,
  operator = "Over",
  offset = "+0+0"
)

font_size_pt <- 10 
font_color <- "black" 
base_offset <- 10 

create_label_png <- function(text, filename, size_pt, color) {
  cex_factor <- 1.2 
  
  png(filename = filename, width = 50, height = 50, units = "px", res = 300, bg = "transparent")
  par(mar = c(0, 0, 0, 0), xpd = TRUE, family = "Arial") 
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1)) 
  text(
    x = 0.5, 
    y = 0.5, 
    labels = text, 
    col = color,       
    cex = cex_factor,   
    font = 2,           # 2=Bold
    family = "Arial"    
  )
  dev.off()
  
  img <- image_read(filename)
  img <- image_trim(img)
  return(img)
}

label_A_img <- create_label_png("a", temp_label_A_path, font_size_pt, font_color)
offset_A_px <- paste0("+", base_offset, "+", base_offset) 

final_image <- image_composite(
  image = final_image, 
  composite_image = label_A_img,
  offset = offset_A_px, 
  operator = "Over"
)

label_B_img <- create_label_png("b", temp_label_B_path, font_size_pt, font_color)
vertical_offset_B <- img2_height_px + space_height_px + base_offset
offset_B_px <- paste0("+", base_offset, "+", vertical_offset_B) 

final_image <- image_composite(
  image = final_image, 
  composite_image = label_B_img,
  offset = offset_B_px, 
  operator = "Over"
)

image_write(final_image, path = output_path_png, density = dpi)


if(file.exists(temp_label_A_path)) file.remove(temp_label_A_path)
if(file.exists(temp_label_B_path)) file.remove(temp_label_B_path)

cat("\n", output_path_png, "\n")