library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)
library(scales) 

files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d") 

china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)


rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste("文件不存在:", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1 
  return(r)
})

# 统一坐标系
r_crs <- crs(rasters[[1]])
china <- st_transform(china, crs = r_crs)

# 颜色定义
gradient_colors <- c("#4575B1", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FEE090",
                     "#FDAE61", "#F46D43", "#D73027", "#A50026")

# ================= 3. 绘图循环 =================
plot_list <- list()
breaks_seq <- seq(0, 2, by = 0.4)

legend_plot <- NULL 

for(i in seq_along(rasters)){
  
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", linewidth = 0.2) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",
      name = expression(paste("C sequestration (kt CO"[2]*"-eq/yr)")),
      limits = c(0, 2),
      breaks = breaks_seq,
      oob = scales::squish,
      guide = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(8, "cm"),
        barheight = unit(0.3, "cm")
      )
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, face = "plain"),
      legend.position = "none", 
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(2, 2, 2, 2, "mm") 
    )
  
  plot_list[[i]] <- p
  
  if (i == 1) {
    legend_plot <- p + theme(
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.margin = margin(t = 10)
    )
  }
}

shared_legend <- get_legend(legend_plot)


map_grid <- plot_grid(
  plotlist = plot_list,
  ncol = 4, 
  nrow = 1,
  align = "hv"
)

final_plot <- plot_grid(
  map_grid, 
  shared_legend, 
  ncol = 1, 
  rel_heights = c(1, 0.2) 
)

out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/Combined_Plot_Horizontal.png"

ggsave(
  filename = out_file,
  plot = final_plot,
  width = 18,  
  height = 6,   
  units = "cm",
  dpi = 300
)

cat("：", out_file, "\n")




library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(scales)
library(dplyr)


files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
json_path <- "/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json"

china_map_full <- st_read(json_path, quiet = TRUE)

r_list <- lapply(files, function(f) {
  r <- rast(f)
  r[r <= 0] <- NA 
  return(r)
})

gradient_colors <- c("#4575B1", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FEE090",
                     "#FDAE61", "#F46D43", "#D73027", "#A50026")
my_professional_colors <- colorRampPalette(gradient_colors)(100)

create_map <- function(r, title, show_legend = FALSE) {
  
  p <- ggplot() +
    geom_spatraster(data = r) +
    
    scale_fill_gradientn(
      colours = my_professional_colors,
      limits = c(0, 2),
      oob = scales::squish,
      breaks = seq(0, 2, by = 0.4),
      labels = label_number(accuracy = 0.1),
      name = expression("C sequestration (kt CO"[2]*"-eq/yr)"),
      na.value = "transparent"
    ) +
    
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(5, "cm"),
        barheight = unit(0.3, "cm"),
        frame.colour = NA,     
        ticks.colour = NA       
      )
    ) +
    
    geom_sf(data = china_map_full, fill = NA, color = "black", linewidth = 0.15) +
    
    labs(title = title) +
    theme_void() + 
    
    theme(
      text = element_text(size = 8, color = "black"),
      plot.title = element_text(hjust = 0.5, vjust = -1, size = 8, face = "plain"),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = 8, margin = margin(b = 5)),
      legend.text = element_text(size = 8),
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
  
  return(p)
}


p_list <- lapply(seq_along(r_list), function(i) {
  create_map(r_list[[i]], periods[i])
})

p_legend_source <- create_map(r_list[[1]], "", show_legend = TRUE) + 
  theme(legend.box.margin = margin(t = -5, b = 5))
shared_legend <- get_legend(p_legend_source)

map_row <- plot_grid(plotlist = p_list, nrow = 1, align = "h")

final_plot <- plot_grid(
  map_row,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.25) 
)

out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/Combined_Map_Final.tif"

ggsave(
  filename = out_file,
  plot = final_plot,
  width = 18,
  height = 6,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

cat("：", out_file, "\n")









library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(cowplot)
library(scales)
library(dplyr)

# ================= 1. 路径与文件 =================
files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/GHG_Fer_Man_Combined_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
json_path <- "/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json"


china_map_full <- st_read(json_path, quiet = TRUE)
target_crs <- st_crs(china_map_full)
cat("：", target_crs$wkt, "\n")

r_list <- lapply(files, function(f) {
  r <- rast(f)
  r[r <= 0] <- NA 

  r_proj <- project(r, target_crs$wkt, method = "bilinear")
  
  return(r_proj)
})

# 颜色定义
gradient_colors <- c("#4575B1", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FEE090",
                     "#FDAE61", "#F46D43", "#D73027", "#A50026")
my_professional_colors <- colorRampPalette(gradient_colors)(100)

create_map <- function(r, title, show_legend = FALSE) {
  
  p <- ggplot() +
    geom_spatraster(data = r) +
    
    scale_fill_gradientn(
      colours = my_professional_colors,
      limits = c(0, 2),
      oob = scales::squish,
      breaks = seq(0, 2, by = 0.4),
      labels = label_number(accuracy = 0.1),
      name = expression("C sequestration (kt CO"[2]*"-eq/yr)"),
      na.value = "transparent"
    ) +
    
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(5, "cm"),
        barheight = unit(0.3, "cm"),
        frame.colour = NA,       
        ticks.colour = NA       
      )
    ) +
    
    geom_sf(data = china_map_full, fill = NA, color = "black", linewidth = 0.15) +
    
    labs(title = title) +
    theme_void() + 
    
    theme(
      text = element_text(size = 8, color = "black"),
      plot.title = element_text(hjust = 0.5, vjust = -1, size = 8, face = "plain"),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = 8, margin = margin(b = 2)),
      legend.text = element_text(size = 8),
      plot.margin = margin(t = -5, b = 5, l = 0, r = 0)
    )
  
  return(p)
}

p_list <- lapply(seq_along(r_list), function(i) {
  create_map(r_list[[i]], periods[i])
})

p_legend_source <- create_map(r_list[[1]], "", show_legend = TRUE) + 
  theme(legend.box.margin = margin(t = -5, b = 5))
shared_legend <- get_legend(p_legend_source)

map_row <- plot_grid(plotlist = p_list, nrow = 2, align = "h")

final_plot <- plot_grid(
  map_row,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.2) 
)

out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 1/Combined_Map_Final.tif"

ggsave(
  filename = out_file,
  plot = final_plot,
  width = 9,
  height = 10,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

cat("：", out_file, "\n")

