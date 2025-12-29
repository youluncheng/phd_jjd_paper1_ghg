##################FER kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)

files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Fer_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Fer_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Fer_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Fer_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d") 


china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)

# ====== t ->  kt) ======
rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste("unfair", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e3   
  return(r)
})

# ====== CRS一 ======
r_crs <- crs(rasters[[1]])
china <- st_transform(china, crs = r_crs)


gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")


plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  

  breaks_seq <- seq(0, 10, length.out = 5)
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", linewidth = 0.2) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(0, 10),
      breaks = breaks_seq
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = 2)),
      legend.position = "right",
      legend.title = element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
  
  plot_list[[i]] <- p
}


combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,           
  label_size = 9,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.05,            
  label_y = 0.98,            
  align = "hv",            
  axis = "tblr"
)

out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/fer.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   
  height = 8,   
  units = "cm",
  dpi = 300
)

cat("：", out_file, "\n")






##################man kt############
##################man kt############
##################man kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)


files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Manure_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Manure_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Manure_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Manure_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d")


china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)

# ====== t -> kt ======
rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste("un", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e3   
  return(r)
})


r_crs <- crs(rasters[[1]]) 
china <- st_transform(china, crs = r_crs)
gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")
plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  
  min_value <- min(0, na.rm = TRUE)
  max_value <- max(2, na.rm = TRUE)
  
  
  breaks_seq <- seq(0, 2, length.out = 5)  
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", size = 0.3) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",   
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(min_value, max_value),  
      breaks = breaks_seq  
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      legend.position = "right",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
  
  plot_list[[i]] <- p
}

# ====== 
combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,   
  label_size = 8,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.02,
  label_y = 0.95,
  align = "hv",           
  axis = "tblr",           
  rel_heights = c(1,1),    
  rel_widths  = c(1,1)     
)


out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/man.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   
  height = 8,  
  units = "cm",
  dpi = 300
)

cat("绘图完成，已保存至：", out_file, "\n")







##################crop buring kt############
##################crop buring kt############
##################crop buring kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)


files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Burn_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Burn_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Burn_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Burn_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d")


china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)

#  t -> kt======
rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste("", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e3   
  return(r)
})


r_crs <- crs(rasters[[1]])  


china <- st_transform(china, crs = r_crs)


gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")


plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  
  min_value <- min(0, na.rm = TRUE)
  max_value <- max(5, na.rm = TRUE)
  
  
  breaks_seq <- seq(0, 5, length.out = 5)  
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", size = 0.3) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",  
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(min_value, max_value),  
      breaks = breaks_seq  
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      legend.position = "right",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
  
  plot_list[[i]] <- p
}


combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,
  label_size = 8,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.02,
  label_y = 0.95,
  align = "hv",           
  axis = "tblr",           
  rel_heights = c(1,1),    
  rel_widths  = c(1,1)     
)


out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/burning.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   
  height = 8,   
  units = "cm",
  dpi = 300
)

cat("compl", out_file, "\n")






##################crop reside kt############
##################crop reside kt############
##################crop reside kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)


files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/crop_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/crop_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/crop_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/crop_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d")


china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)


rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste("", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e3   
  return(r)
})


r_crs <- crs(rasters[[1]])  


china <- st_transform(china, crs = r_crs)


gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")


plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  
  min_value <- min(0, na.rm = TRUE)
  max_value <- max(3, na.rm = TRUE)
  
  
  breaks_seq <- seq(0, 3, length.out = 5)  
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", size = 0.3) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",   
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(min_value, max_value),  
      breaks = breaks_seq   
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      legend.position = "right",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
  
  plot_list[[i]] <- p
}


combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,
  label_size = 8,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.02,
  label_y = 0.95,
  align = "hv",            
  axis = "tblr",           
  rel_heights = c(1,1),    
  rel_widths  = c(1,1)     
)

# ====== 保存输出 ======
out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/resides.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   
  height = 8,   
  units = "cm",
  dpi = 300
)

cat("：", out_file, "\n")









##################irr kt############
##################irr kt############
##################irr kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)


files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Irr_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Irr_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Irr_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Irr_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d")


china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)


rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste(":", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e3   
  return(r)
})


r_crs <- crs(rasters[[1]])  


china <- st_transform(china, crs = r_crs)


gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")


plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  
  min_value <- min(0, na.rm = TRUE)
  max_value <- max(1, na.rm = TRUE)
  
  
  breaks_seq <- seq(0, 1, length.out = 5)  
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", size = 0.3) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",   
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(min_value, max_value),  
      breaks = breaks_seq   
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      legend.position = "right",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
  
  plot_list[[i]] <- p
}


combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,
  label_size = 8,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.02,
  label_y = 0.95,
  align = "hv",            
  axis = "tblr",           
  rel_heights = c(1,1),    
  rel_widths  = c(1,1)     
)


out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/irr.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   
  height = 8,   
  units = "cm",
  dpi = 300
)

cat("：", out_file, "\n")








##################irrE kt############
##################irrE kt############
##################irrE kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)


files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/IRRE_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/IRRE_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/IRRE_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/IRRE_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d")


china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)


rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste(":", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e4   
  return(r)
})


r_crs <- crs(rasters[[1]])  


china <- st_transform(china, crs = r_crs)


gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")


plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
 
  min_value <- min(0, na.rm = TRUE)
  max_value <- max(1.5, na.rm = TRUE)
  
 
  breaks_seq <- seq(0, 1.5, length.out = 5)  
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", size = 0.3) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",  
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(min_value, max_value),  
      breaks = breaks_seq   
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      legend.position = "right",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
  
  plot_list[[i]] <- p
}


combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,
  label_size = 8,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.02,
  label_y = 0.95,
  align = "hv",           
  axis = "tblr",          
  rel_heights = c(1,1),    
  rel_widths  = c(1,1)    
)


out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/irre.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   
  height = 8,   
  units = "cm",
  dpi = 300
)

cat("：", out_file, "\n")






##################MACHINA-ENERGY 单位由转化kt############
##################MACHINA-ENERGY 单位由转化kt############
##################MACHINA-ENERGY 单位由转化kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)

# ====== 文件路径 ======
files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/MAC_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/MAC_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/MAC_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/MAC_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d")

# ====== 读取中国边界 ======
china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)

# ====== 读取栅格并处理 NA (单位 t -> 转换为 kt) ======
rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste("文件不存在:", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e3   # g 转换为 kt
  return(r)
})

# ====== 获取栅格的 CRS ======
r_crs <- crs(rasters[[1]])  # 获取第一个栅格的 CRS

# ====== 将中国边界的 CRS 转换为栅格的 CRS ======
china <- st_transform(china, crs = r_crs)

# ====== 渐变色设置 ======
gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")

# ====== 生成 ggplot 图列表 ======
plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  # 获取栅格数据的最小值和最大值
  min_value <- min(0, na.rm = TRUE)
  max_value <- max(12, na.rm = TRUE)
  
  # 设置图例范围从0到数据中的最大最小值
  breaks_seq <- seq(0, 12, length.out = 6)  # 使用4个区间
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", size = 0.3) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",   # 去除 NA 值的颜色显示
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(min_value, max_value),  # 使用栅格数据中的最小值和最大值
      breaks = breaks_seq   # 使用4个区间的断点
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      legend.position = "right",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
  
  plot_list[[i]] <- p
}

# ====== 合并为 2x2 图，最小化间距 ======
combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,
  label_size = 8,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.02,
  label_y = 0.95,
  align = "hv",            # 水平垂直对齐
  axis = "tblr",           # 对齐所有坐标轴
  rel_heights = c(1,1),    # 两行高度相等
  rel_widths  = c(1,1)     # 两列宽度相等
)

# ====== 保存输出 ======
out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/MAC.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   # 15 cm 转 inch
  height = 8,   # 12 cm 转 inch
  units = "cm",
  dpi = 300
)

cat("绘图完成，已保存至：", out_file, "\n")






##################RICE 单位由转化kt############
##################RICE 单位由转化kt############
##################RICE 单位由转化kt############
library(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)

# ====== 文件路径 ======
files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Rice_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Rice_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Rice_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/Rice_2010-2023.tif"
)
periods <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
labels <- c("a", "b", "c", "d")

# ====== 读取中国边界 ======
china <- st_read("/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json", quiet = TRUE)

# ====== 读取栅格并处理 NA (单位 t -> 转换为 kt) ======
rasters <- lapply(files, function(f){
  if(!file.exists(f)) stop(paste("文件不存在:", f))
  r <- rast(f)
  r[r == 0] <- NA
  r[r == -9999] <- NA
  r <- r / 1e3   # g 转换为 kt
  return(r)
})

# ====== 获取栅格的 CRS ======
r_crs <- crs(rasters[[1]])  # 获取第一个栅格的 CRS

# ====== 将中国边界的 CRS 转换为栅格的 CRS ======
china <- st_transform(china, crs = r_crs)

# ====== 渐变色设置 ======
gradient_colors <- c("#80c2c2", "#008585", "#3a978c", "#74a892", "#b8cdab",
                     "#fbf2c4", "#f0daa5", "#e5c185", "#d68a58", "#c7522a")

# ====== 生成 ggplot 图列表 ======
plot_list <- list()
for(i in seq_along(rasters)){
  r_df <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  # 获取栅格数据的最小值和最大值
  min_value <- min(0, na.rm = TRUE)
  max_value <- max(45, na.rm = TRUE)
  
  # 设置图例范围从0到数据中的最大最小值
  breaks_seq <- seq(min_value, max_value, length.out = 5)  # 使用4个区间
  
  p <- ggplot() +
    geom_tile(data = r_df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = china, fill = NA, color = "black", size = 0.3) +
    scale_fill_gradientn(
      colours = gradient_colors,
      na.value = "transparent",   # 去除 NA 值的颜色显示
      name = expression(paste("GHG (kt CO"[2]*"-eq/yr)")),
      limits = c(min_value, max_value),  # 使用栅格数据中的最小值和最大值
      breaks = breaks_seq   # 使用4个区间的断点
    ) +
    labs(title = periods[i]) +
    theme_minimal(base_size = 6) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = -10)),
      legend.position = "right",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.15, "cm"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
  
  plot_list[[i]] <- p
}

# ====== 合并为 2x2 图，最小化间距 ======
combined_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 2, nrow = 2,
  labels = labels,
  label_size = 8,
  hjust = 0, vjust = 1,
  label_fontface = "bold",
  label_colour = "black",
  label_x = 0.02,
  label_y = 0.95,
  align = "hv",            # 水平垂直对齐
  axis = "tblr",           # 对齐所有坐标轴
  rel_heights = c(1,1),    # 两行高度相等
  rel_widths  = c(1,1)     # 两列宽度相等
)

# ====== 保存输出 ======
out_file <- "/Users/dongjingjing/Desktop/GHG/FIG/SI/FIGS1_8/RICEC.png"
ggsave(
  filename = out_file,
  plot = combined_plot,
  width = 12,   # 15 cm 转 inch
  height = 8,   # 12 cm 转 inch
  units = "cm",
  dpi = 300
)

cat("绘图完成，已保存至：", out_file, "\n")


