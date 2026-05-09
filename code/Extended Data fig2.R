
###########################PRINT#####
library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(cowplot)
library(scales)
library(dplyr)

files <- c(
  "1980-1989" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 2/Total_Sum_1980-1989.tif",
  "1990-1999" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 2/Total_Sum_1990-1999.tif",
  "2000-2009" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 2/Total_Sum_2000-2009.tif",
  "2010-2023" = "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 2/Total_Sum_2010-2023.tif"
)
json_path <- "/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json"

china_map_full <- st_read(json_path, quiet = TRUE)

is_taiwan <- apply(china_map_full, 1, function(row) any(grepl("台湾|Taiwan", row)))
china_map_mask_sf <- china_map_full[!is_taiwan, ]
china_mask_vect <- vect(china_map_mask_sf)

r_list_raw <- lapply(files, rast)
r_list <- lapply(r_list_raw, function(x) {
  x_masked <- mask(x, china_mask_vect) 
  x_masked[x_masked <= 0] <- NA       
  return(x_masked / 1000000)            
})

fixed_limits <- c(0, 0.05)

custom_5_colors <- c(
  "#4575B1", 
  "#74ADD1", 
  "#ABD9E9", 
  "#E0F3F8",  
  "#FEE090",  
  "#FDAE61", 
  "#F46D43", 
  "#D73027", 
  "#A50026"   
)

my_professional_colors <- colorRampPalette(custom_5_colors)(100)

create_map <- function(r, title, show_legend = FALSE) {
  
  p <- ggplot() +
    geom_spatraster(data = r) +
    
    scale_fill_gradientn(
      colours = my_professional_colors, 
      limits = fixed_limits, 
      oob = scales::squish,     
      breaks = seq(0, 0.05, by = 0.01), 
      labels = label_number(accuracy = 0.01),
      name = expression("GHG Emissions (Mt CO"[2]*"-eq/yr)"),
      na.value = "white" 
    ) +
    
    guides(
      fill = guide_colorbar(
        title.position = "top", 
        title.hjust = 0.5,
        barwidth = unit(4, "cm"), 
        barheight = unit(0.3, "cm"),
        frame.colour = NA,       
        ticks.colour = "NA", 
        ticks.linewidth = 0.5
      )
    ) +
    
    geom_sf(data = china_map_full, fill = NA, color = "black", size = 0.15) +
    
    labs(title = title) +
    theme_void() + 
    
    theme(
      text = element_text(family = "Arial", size = 8, color = "black"), 
      plot.title = element_text(hjust = 0.5, vjust = -2, size = 8, face = "plain"), 
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = 8, margin = margin(b = 5)), 
      legend.text = element_text(size = 8),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  return(p)
}

p1 <- create_map(r_list[[1]], "1980-1989")
p2 <- create_map(r_list[[2]], "1990-1999")
p3 <- create_map(r_list[[3]], "2000-2009")
p4 <- create_map(r_list[[4]], "2010-2023")

p_legend_source <- create_map(r_list[[1]], "", show_legend = TRUE) + 
  theme(legend.box.margin = margin(t = -5, b = 5, l = 0, r = 0))
shared_legend <- get_legend(p_legend_source)

map_row <- plot_grid(p1, p2, p3, p4, nrow = 2, align = "h")

final_plot <- plot_grid(
  map_row,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.2) 
)


output_file <- "/Users/dongjingjing/Desktop/GHG/FIG/AddS1.tif"

ggsave(
  filename = output_file,
  plot = final_plot,
  width = 9,
  height = 10,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

message(paste("", output_file))