
#############################################################calculate###################################################
#############################################################calculate####################################################
#############################################################calculate####################################################
library(raster)
library(sf)
library(dplyr)
library(tools)

tif_files <- c(
  "Burn-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/Burn-ghg/Burn_2023.tif",
  "crop-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/crop-ghg/Total_GHG_2023.tif",
  "Fer-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/Fer-ghg/Fer_2023.tif",
  "Irr-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/Irr-ghg/Irr_2023.tif",
  "Irre-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/Irre-ghg/IRRE_2023.tif",
  "Mac-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/Mac-ghg/Mac_2023.tif",
  "Manure-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/Manure-ghg/Manure_2023.tif",
  "Rice-ghg" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/Rice-ghg/Rice_2023.tif",
  "sum-ghg_2023" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/sum-ghg/sum_output_2023.tif",
  "sum-ghg_2010" = "/Users/dongjingjing/Desktop/GHG/Data/Data_result/统一分辨率/sum-ghg/sum_output_2010.tif"
)

province_boundary_path <- "/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json"


province_sf <- tryCatch({
  st_read(province_boundary_path, quiet = TRUE) %>%
    st_transform(4326)  
}, error = function(e) {
  stop("无法读取省份边界文件: ", e$message)
})

name_fields <- c("NAME", "name", "省份", "省名")
name_field <- name_fields[name_fields %in% colnames(province_sf)][1]
if (is.na(name_field)) {
  stop("省份边界数据中未找到识别的名称字段，请检查数据结构")
}

exclude_provinces <- c("香港", "澳门", "台湾", "香港特别行政区", "澳门特别行政区", "台湾省")
province_sf <- province_sf[!province_sf[[name_field]] %in% exclude_provinces, ]

overall_output <- file.path(dirname(province_boundary_path), "provincial_results")
if (!dir.exists(overall_output)) {
  dir.create(overall_output, recursive = TRUE)
}

for (tif_name in names(tif_files)) {
  tryCatch({
    r <- raster(tif_files[tif_name])
    
    if (st_crs(province_sf) != crs(r)) {
      message("正在转换", basename(tif_files[tif_name]), "的投影...")
      r <- projectRaster(r, crs = st_crs(province_sf)$proj4string)
    }
    
    output_dir <- file.path(overall_output, tif_name)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    sum_results <- data.frame(
      省份 = character(), 
      数值总和 = numeric(), 
      文件名 = character(),
      单元格数量 = integer(),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_len(nrow(province_sf))) {
      province_name <- as.character(province_sf[[name_field]][i])
      
      province_raster <- tryCatch({
        mask(crop(r, province_sf[i, ]), province_sf[i, ])
      }, error = function(e) {
        warning("处理", province_name, "时出错: ", e$message)
        return(NULL)
      })
      
      if (!is.null(province_raster) && !all(is.na(values(province_raster)))) {
        output_file <- file.path(output_dir, paste0(province_name, ".tif"))
        writeRaster(province_raster, output_file, format = "GTiff", overwrite = TRUE)
        
        sum_value <- cellStats(province_raster, sum, na.rm = TRUE)
        cell_count <- cellStats(!is.na(province_raster), sum)
        
        sum_results <- bind_rows(sum_results, tibble(
          省份 = province_name,
          数值总和 = sum_value,
          单元格数量 = cell_count,
          文件名 = basename(tif_files[tif_name])
        ))
      }
    }
    
    sum_output_file <- file.path(overall_output, paste0("sum_results_", tif_name, ".csv"))
    write.csv(sum_results, sum_output_file, row.names = FALSE, fileEncoding = "UTF-8")
    message("处理完成: ", basename(tif_files[tif_name]))
    
  }, error = function(e) {
    warning("处理文件", basename(tif_files[tif_name]), "时出错: ", e$message)
  })
}

all_sum_files <- list.files(overall_output, pattern = "^sum_results_.*\\.csv$", full.names = TRUE)
if (length(all_sum_files) > 0) {
  combined_results <- lapply(all_sum_files, read.csv, fileEncoding = "UTF-8") %>%
    bind_rows()
  
  write.csv(combined_results, file.path(overall_output, "all_provinces_combined_results.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  message("所有结果已合并至: all_provinces_combined_results.csv")
}

message("所有文件处理完毕！结果保存在: ", overall_output)








#####################################################aaaaaaaaaaaaaaa###########################################
#####################################################aaaaaaaaaaaaaaa###########################################
#####################################################aaaaaaaaaaaaaaa###########################################
library(ggplot2)
library(readxl)
library(scales)
library(stringr)
library(ggExtra)
library(cowplot)
library(ggrepel)
library(showtext)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3.xlsx"
data <- read_excel(file_path, sheet = 1) 

convert_to_numeric <- function(column) {
  if (is.character(column) || is.factor(column)) {
    column <- as.character(column) %>% trimws() %>% str_remove_all("[,￥$€]")
    column <- ifelse(column %in% c("", "NA"), NA, column)
  }
  as.numeric(column)
}

target_cols <- c(2, 3, 4, 5)
for (col in target_cols) {
  data[[col]] <- convert_to_numeric(data[[col]])
}

data <- data[complete.cases(data[[2]], data[[3]], data[[4]], data[[5]]), ]
if (nrow(data) == 0) stop("No valid data. Please check Excel format.")

data[[3]] <- log(data[[3]] + 1) %>% rescale(to = c(0, 1))
data[[4]] <- log(data[[4]] + 1) %>% rescale(to = c(0, 1))

data <- data.frame(
  FirstCol_Label = data[[1]], 
  Bubble_Size = data[[2]],
  X_Value = data[[4]],
  Y_Value = data[[3]],
  Color_Value = data[[5]]
)

size_breaks <- quantile(data$Bubble_Size, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
size_labels <- paste0(round(size_breaks[-length(size_breaks)], 1), 
                      "-", 
                      round(size_breaks[-1], 1))

data$Size_Category <- cut(
  data$Bubble_Size,
  breaks = size_breaks,
  labels = size_labels,
  include.lowest = TRUE
)

custom_colors <- c("#1E3A8A", "#3B82F6", "#34D399", "#10B981", "#b8cdab", 
                   "#FBBF24", "#FDBA74", "#F97316", "#FB923C", "#DC2626")

p <- ggplot(data, aes(x = X_Value, y = Y_Value)) +
  geom_point(aes(size = Size_Category, color = Color_Value), alpha = 0.7, fill = NA) +
  
  geom_text_repel(
    aes(label = FirstCol_Label),
    size = 2.5, color = "black", family = "Arial", 
    box.padding = 0.2,   
    point.padding = 0.2, 
    max.overlaps = 25,   
    force = 1,           
    segment.size = 0.2   
  ) +
  
  geom_vline(xintercept = 0.5, color = "black", linewidth = 0.4) + 
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.4) + 
  
  xlim(-0.05, 1.05) +
  ylim(-0.05, 1.05) +
  
  scale_size_manual(
    name = expression(paste("GHG emissions (Mt CO"[2]*"-eq)")), 
    values = c(1, 2, 3, 4),
    breaks = size_labels,
    labels = size_labels,
    guide = guide_legend(
      title.position = "left", 
      title.hjust = 0.5, 
      label.position = "bottom", 
      label.hjust = 0.5, 
      keyheight = unit(0.1, "cm"), 
      keywidth = unit(0.1, "cm") 
    )
  ) +
  
  scale_color_gradientn(
    name = "Emission intensity (t/ha)", 
    colours = custom_colors,
    limits = c(0.3, 1.15),
    breaks = seq(0.3, 1.15, by = 0.2),
    guide = guide_colorbar(
      barwidth = 0.2,
      barheight = 4,
      ticks = TRUE,
      ticks.colour = "black",
      title.position = "left",
      title.hjust = 0.7,
      title.vjust = 0.5, 
      frame.colour = "black",
      frame.linewidth = 0.2, 
      barcolour = "#D3D3D3", 
      title.theme = element_text(margin = margin(r = 12))
    )
  ) +
  
  labs(
    x = "Proportion of dryland area (%)", 
    y = "Food crop planting area (kha)"   
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none", 
    axis.text = element_text(size = 8, color = "black", family = "Arial"), 
    axis.title = element_text(size = 8, color = "black", family = "Arial"), 
    legend.text = element_text(size = 6, color = "black", family = "Arial"), 
    legend.title = element_text(size = 6, color = "black", family = "Arial"), 
    strip.text = element_text(size = 8, color = "black", family = "Arial"), 
    plot.title = element_text(size = 8, color = "black", family = "Arial"), 
    plot.subtitle = element_text(size = 8, color = "black", family = "Arial"), 
    plot.caption = element_text(size = 8, color = "black", family = "Arial") 
  ) +
  
  annotate("text", x = 0.2, y = 1, 
           label = "'GHG Emissions'",  
           parse = TRUE,                     
           size = 3, hjust = 0.5, vjust = 0, color = "black", family = "Arial")

p_with_marginal <- ggExtra::ggMarginal(p, type = "density", fill = "skyblue", size = 10)

legend <- get_legend(p + theme(legend.position = "right", 
                               legend.title = element_text(angle = -270),
                               legend.margin = margin(t = 20, r = 1, b = -25, l = 00)))

FIG3a <- plot_grid(p_with_marginal, legend, rel_widths = c(0.7, 0.15)) 

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3a.png", plot = FIG3a, width = 9, height = 8, unit = "cm", dpi = 300)

#####################################################bbbbbbbbbbbbbbbb###########################################
#####################################################bbbbbbbbbbbbbbbbb###########################################
#####################################################bbbbbbbbbbbbbbbbbbb###########################################
if (!require("ggrepel")) install.packages("ggrepel")
library(ggplot2)
library(readxl)
library(scales)
library(stringr)
library(ggExtra)
library(cowplot)
library(ggrepel)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3.xlsx"
data <- read_excel(file_path, sheet = 2) 

convert_to_numeric <- function(column) {
  if (is.character(column) || is.factor(column)) {
    column <- as.character(column) %>% trimws() %>% str_remove_all("[,￥$€]")
    column <- ifelse(column %in% c("", "NA"), NA, column)
  }
  as.numeric(column)
}

target_cols <- c(2, 3, 4, 5)
for (col in target_cols) {
  data[[col]] <- convert_to_numeric(data[[col]])
}

data <- data[complete.cases(data[[2]], data[[3]], data[[4]], data[[5]]), ]
if (nrow(data) == 0) stop("No valid data. Please check Excel format.")

data[[3]] <- log(data[[3]] + 1) %>% rescale(to = c(0, 1))
data[[4]] <- log(data[[4]] + 1) %>% rescale(to = c(0, 1))

data <- data.frame(
  FirstCol_Label = data[[1]], 
  Bubble_Size = data[[2]],
  X_Value = data[[4]],
  Y_Value = data[[3]],
  Color_Value = data[[5]]
)

size_breaks <- quantile(data$Bubble_Size, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
size_labels <- paste0(round(size_breaks[-length(size_breaks)], 1), 
                      "-", 
                      round(size_breaks[-1], 1))

data$Size_Category <- cut(
  data$Bubble_Size,
  breaks = size_breaks,
  labels = size_labels,
  include.lowest = TRUE
)

custom_colors <- c("#1E3A8A", "#3B82F6", "#34D399", "#10B981", "#b8cdab", 
                   "#FBBF24", "#FDBA74", "#F97316", "#FB923C", "#DC2626")

p <- ggplot(data, aes(x = X_Value, y = Y_Value)) +
  geom_point(aes(size = Size_Category, color = Color_Value), alpha = 0.7, fill = "gray") +
  
  geom_text_repel(
    aes(label = FirstCol_Label),
    size = 2, color = "black", family = "Arial", 
    box.padding = 0.2,   
    point.padding = 0.2, 
    max.overlaps = 25,   
    force = 1,           
    segment.size = 0.2   
  ) +
  
  geom_vline(xintercept = 0.5, color = "black", linewidth = 0.4) + 
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.4) + 
  
  xlim(-0.05, 1.05) +
  ylim(-0.05, 1.05) +
  
  scale_size_manual(
    name = expression(paste("N"[2], "O emissions (Mt CO"[2]*"-eq)")), 
    values = c(1, 2, 3, 4),
    breaks = size_labels,
    labels = size_labels,
    guide = guide_legend(
      title.position = "left", 
      title.hjust = 0.5, 
      label.position = "bottom", 
      label.hjust = 0.5, 
      keyheight = unit(0.1, "cm"), 
      keywidth = unit(0.1, "cm") 
    )
  ) +
  
  scale_color_gradientn(
    name = "Emission intensity (t/ha)", 
    colours = custom_colors,
    limits = c(0.03, 0.2),
    breaks = seq(0.03, 0.2, by = 0.02),
    guide = guide_colorbar(
      barwidth = 0.2,
      barheight = 4,
      ticks = TRUE,
      ticks.colour = "black",
      title.position = "left",
      title.hjust = 0.7,
      title.vjust = 0.5, 
      frame.colour = "black",
      frame.linewidth = 0.2, 
      barcolour = "#D3D3D3", 
      title.theme = element_text(margin = margin(r = 12))
    )
  ) +
  
  labs(
    x = "Proportion of dryland area (%)", 
    y = "Food crop planting area (kha)"   
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    legend.position = "none", 
    axis.text = element_text(size = 8, color = "black", family = "Arial"), 
    axis.title = element_text(size = 8, color = "black", family = "Arial"), 
    legend.text = element_text(size = 6, color = "black", family = "Arial"), 
    legend.title = element_text(size = 6, color = "black", family = "Arial"), 
    strip.text = element_text(size = 8, color = "black", family = "Arial"), 
    plot.title = element_text(size = 8, color = "black", family = "Arial"), 
    plot.subtitle = element_text(size = 8, color = "black", family = "Arial"), 
    plot.caption = element_text(size = 8, color = "black", family = "Arial") 
  ) +

  annotate("text", x = 0.2, y = 1, 
           label = "'N'[2] * 'O Emissions'", 
           parse = TRUE,                      
           size = 3, hjust = 0.5, vjust = 0, color = "black", family = "Arial")

p_with_marginal <- ggExtra::ggMarginal(p, type = "density", fill = "skyblue", size = 10)

legend <- get_legend(p + theme(legend.position = "right", 
                               legend.title = element_text(angle = -270),
                               legend.margin = margin(t = 20, r = 1, b = -25, l = 00)))

FIG3b <- plot_grid(p_with_marginal, legend, rel_widths = c(0.7, 0.15)) 

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3b.png", plot = FIG3b, width = 9, height = 8, unit = "cm", dpi = 300)

#####################################################cccccccccccccccccc###########################################
#####################################################ccccccccccccccccccccc###########################################
#####################################################cccccccccccccccccccccc###########################################
library(ggplot2)
library(readxl)
library(scales)
library(stringr)
library(ggExtra)
library(cowplot)
library(ggrepel)
file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3.xlsx"
data <- read_excel(file_path, sheet = 3)  

convert_to_numeric <- function(column) {
  if (is.character(column) || is.factor(column)) {
    column <- as.character(column) %>% trimws() %>% str_remove_all("[,￥$€]")
    column <- ifelse(column %in% c("", "NA", "无数据", "--"), NA, column)
  }
  as.numeric(column)
}

target_cols <- c(2, 3, 4, 5)
for (col in target_cols) {
  data[[col]] <- convert_to_numeric(data[[col]])
}

data <- data[complete.cases(data[[2]], data[[3]], data[[4]], data[[5]]), ]
if (nrow(data) == 0) stop("No valid data. Please check Excel format.")

data[[3]] <- log(data[[3]] + 1) %>% rescale(to = c(0, 1))
data[[4]] <- log(data[[4]] + 1) %>% rescale(to = c(0, 1))

data <- data.frame(
  FirstCol_Label = data[[1]], 
  Bubble_Size = data[[2]],
  X_Value = data[[4]],
  Y_Value = data[[3]],
  Color_Value = data[[5]]
)

size_breaks <- quantile(data$Bubble_Size, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
size_labels <- paste0(round(size_breaks[-length(size_breaks)], 1), 
                      "-", 
                      round(size_breaks[-1], 1))

data$Size_Category <- cut(
  data$Bubble_Size,
  breaks = size_breaks,
  labels = size_labels,
  include.lowest = TRUE
)

custom_colors <- c("#1E3A8A", "#3B82F6", "#34D399", "#10B981", "#b8cdab", 
                   "#FBBF24", "#FDBA74", "#F97316", "#FB923C", "#DC2626")


p <- ggplot(data, aes(x = X_Value, y = Y_Value)) +
  geom_point(aes(size = Size_Category, color = Color_Value), alpha = 0.7, fill = "gray") +
  
  geom_text_repel(
    aes(label = FirstCol_Label),
    size = 2.5, color = "black", family = "Arial", 
    box.padding = 0.2,   
    point.padding = 0.2, 
    max.overlaps = 25,   
    force = 1,           
    segment.size = 0.2   
  ) +
  
  geom_vline(xintercept = 0.5, color = "black", linewidth = 0.4) +  
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.4) +  
  
  xlim(-0.05, 1.05) +
  ylim(-0.05, 1.05) +
  
  scale_size_manual(
    name = expression(paste("CH"[4], " emissions (Mt CO"[2]*"-eq)")), 
    values = c(1, 2, 3, 4),
    breaks = size_labels,
    labels = size_labels,
    guide = guide_legend(
      title.position = "left",  
      title.hjust = 0.5,  
      label.position = "bottom",
      label.hjust = 0.5, 
      keyheight = unit(0.1, "cm"), 
      keywidth = unit(0.1, "cm") 
    )
  ) +
  
  scale_color_gradientn(
    name = "Emission intensity (t/ha)", 
    colours = custom_colors,
    limits = c(0, 0.03),
    breaks = seq(0, 0.03, by = 0.003),
    guide = guide_colorbar(
      barwidth = 0.2,
      barheight = 4,
      ticks = TRUE,
      ticks.colour = "black",
      title.position = "left",
      title.hjust = 0.7,
      title.vjust = 0.5,  
      frame.colour = "black",
      frame.linewidth = 0.2,  
      barcolour = "#D3D3D3", 
      title.theme = element_text(margin = margin(r = 12))
    )
  ) +
  
  labs(
    x = "Proportion of dryland area (%)", 
    y = "Food crop planting area (kha)" 
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    legend.position = "none",  
    axis.text = element_text(size = 8, color = "black", family = "Arial"), 
    axis.title = element_text(size = 8, color = "black", family = "Arial"),  
    legend.text = element_text(size = 6, color = "black", family = "Arial"), 
    legend.title = element_text(size = 6, color = "black", family = "Arial"),  
    strip.text = element_text(size = 8, color = "black", family = "Arial"), 
    plot.title = element_text(size = 8, color = "black", family = "Arial"),  
    plot.subtitle = element_text(size = 8, color = "black", family = "Arial"),  
    plot.caption = element_text(size = 8, color = "black", family = "Arial")  
  )+
  annotate("text", x = 0.2, y = 1, label = "CH₄ Emissions", size = 3, 
           hjust = 0.5, vjust = 0, color = "black", family = "Arial") 
p_with_marginal <- ggExtra::ggMarginal(p, type = "density", fill = "skyblue", size = 10)

legend <- get_legend(p + theme(legend.position = "right", 
                               legend.title = element_text(angle = -270),
                               legend.margin = margin(t = 20, r = 1, b = -25, l = 00)))

FIG3c <- plot_grid(p_with_marginal, legend, rel_widths = c(0.7, 0.15))  

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3c.png", plot = FIG3c, width = 9, height = 8, unit = "cm", dpi = 300)


#####################################################ddddddddddddddddddddd###########################################
#####################################################ddddddddddddddddddddddd###########################################
#####################################################dddddddddddddddddddddddd###########################################
if (!require("ggrepel")) install.packages("ggrepel")
library(ggplot2)
library(readxl)
library(scales)
library(stringr)
library(ggExtra)
library(cowplot)
library(ggrepel)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3.xlsx"
data <- read_excel(file_path, sheet = 4)  

convert_to_numeric <- function(column) {
  if (is.character(column) || is.factor(column)) {
    column <- as.character(column) %>% trimws() %>% str_remove_all("[,￥$€]")
    column <- ifelse(column %in% c("", "NA"), NA, column)
  }
  as.numeric(column)
}

target_cols <- c(2, 3, 4, 5)
for (col in target_cols) {
  data[[col]] <- convert_to_numeric(data[[col]])
}

data <- data[complete.cases(data[[2]], data[[3]], data[[4]], data[[5]]), ]
if (nrow(data) == 0) stop("No valid data. Please check Excel format.")

data[[3]] <- log(data[[3]] + 1) %>% rescale(to = c(0, 1))
data[[4]] <- log(data[[4]] + 1) %>% rescale(to = c(0, 1))

data <- data.frame(
  FirstCol_Label = data[[1]],
  Bubble_Size = data[[2]],
  X_Value = data[[4]],
  Y_Value = data[[3]],
  Color_Value = data[[5]]
)

size_breaks <- quantile(data$Bubble_Size, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
size_labels <- paste0(round(size_breaks[-length(size_breaks)], 1), 
                      "-", 
                      round(size_breaks[-1], 1))

data$Size_Category <- cut(
  data$Bubble_Size,
  breaks = size_breaks,
  labels = size_labels,
  include.lowest = TRUE
)

custom_colors <- c("#1E3A8A", "#3B82F6", "#34D399", "#10B981", "#b8cdab", 
                   "#FBBF24", "#FDBA74", "#F97316", "#FB923C", "#DC2626")

p <- ggplot(data, aes(x = X_Value, y = Y_Value)) +
  geom_point(aes(size = Size_Category, color = Color_Value), alpha = 0.7, fill = "gray") +
  
  geom_text_repel(
    aes(label = FirstCol_Label),
    size = 2.5, color = "black", family = "Arial",  
    box.padding = 0.2,   
    point.padding = 0.2, 
    max.overlaps = 25,   
    force = 1,           
    segment.size = 0.2   
  ) +
  
  geom_vline(xintercept = 0.5, color = "black", linewidth = 0.4) +  
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.4) +  
  xlim(-0.05, 1.05) +
  ylim(-0.05, 1.05) +
  
  scale_size_manual(
    name = expression(paste("CO"[2], "emissions (Mt CO"[2]*"-eq)")),  
    values = c(1, 2, 3, 4),
    breaks = size_labels,
    labels = size_labels,
    guide = guide_legend(
      title.position = "left",  
      title.hjust = 0.5,  
      label.position = "bottom",  
      label.hjust = 0.5,  
      keyheight = unit(0.1, "cm"),  
      keywidth = unit(0.1, "cm")  
    )
  ) +
  
 
  scale_color_gradientn(
    name = "Emission intensity (t/ha)",  
    colours = custom_colors,
    limits = c(0.1, 1.1),
    breaks = seq(0.1, 1.1, by = 0.1),
    guide = guide_colorbar(
      barwidth = 0.2,
      barheight = 4,
      ticks = TRUE,
      ticks.colour = "black",
      title.position = "left",
      title.hjust = 0.7,
      title.vjust = 0.5,  
      frame.colour = "black",
      frame.linewidth = 0.2,  
      barcolour = "#D3D3D3", 
      title.theme = element_text(margin = margin(r = 12))
    )
  ) +
  
  
  labs(
    x = "Proportion of dryland area (%)",  
    y = "Food crop planting area (kha)"   
  ) +
  
  theme_minimal() +
  theme(
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    legend.position = "none",  
    axis.text = element_text(size = 8, color = "black", family = "Arial"),  
    axis.title = element_text(size = 8, color = "black", family = "Arial"),  
    legend.text = element_text(size = 6, color = "black", family = "Arial"),  
    legend.title = element_text(size = 6, color = "black", family = "Arial"),  
    strip.text = element_text(size = 8, color = "black", family = "Arial"),  
    plot.title = element_text(size = 8, color = "black", family = "Arial"),  
    plot.subtitle = element_text(size = 8, color = "black", family = "Arial"),  
    plot.caption = element_text(size = 8, color = "black", family = "Arial")  
  )+
  
  annotate("text", x = 0.2, y = 1, label = "CO₂ Emissions", size = 3, 
           hjust = 0.5, vjust = 0, color = "black", family = "Arial") 

p_with_marginal <- ggExtra::ggMarginal(p, type = "density", fill = "skyblue", size = 10)


legend <- get_legend(p + theme(legend.position = "right", 
                               legend.title = element_text(angle = -270),
                               legend.margin = margin(t = 20, r = 1, b = -25, l = 00)))

FIG3d <- plot_grid(p_with_marginal, legend, rel_widths = c(0.7, 0.15))  

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3d.png", plot = FIG3d, width = 9, height = 8, unit = "cm", dpi = 300)







##############EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
##############EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
##############EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
##############EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
library(readxl)
library(ggplot2)
library(ggrepel)  


file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3.xlsx"
data <- read_excel(file_path, sheet = "Fig.2e")


colnames(data)
str(data)
colnames(data) <- c("Name", "Delta_GHG", "Delta_Yield")

data$Delta_GHG <- as.numeric(data$Delta_GHG)
data$Delta_Yield <- as.numeric(data$Delta_Yield)

data_clean <- data[, c("Name", "Delta_GHG", "Delta_Yield")]
data_clean <- na.omit(data_clean)

summary(data_clean)
decoupling_labels <- data.frame(
  x = c(30, 50, 50, 30, -45, -45, -20),  
  y = c(90, 50, 20, -100, 90, -5, -100), 
  label = c("Weak Decoupling", "Coupling", "Weak Decoupling", 
            "Absolute Decoupling", "Absolute Decoupling", 
            "Relative Decoupling", "Negative Decoupling") 
)

FIG3e <- ggplot(data_clean, aes(x = Delta_Yield, y = Delta_GHG, label = Name)) +
  geom_point(color = "skyblue", size = 1) +
 
  geom_abline(slope = 1, intercept = 0, color = "gray50", 
              size = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray40", 
             size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray40", 
             size = 0.5, linetype = "dashed") +
  geom_text_repel(
    size = 2.2,  
    color = "black", 
    family = "Arial",
    box.padding = 0.3,  
    point.padding = 0.4,
    segment.color = "gray20",
    segment.size = 0.3,
    force = 0.5,  
    max.overlaps = Inf,  
    segment.length = 0.2,
    min.segment.length = 0.1,
    max.time = 1,  
    max.iter = 10000  
  ) +
  geom_text(
    data = decoupling_labels,
    aes(x = x, y = y, label = label),
    color = "darkred",  
    size = 2,
    family = "Arial"
  ) +
  labs(
    x = "Delta Yield (%)", 
    y = "Delta GHG (%)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-60, 60, by = 20)) +
  scale_y_continuous(breaks = seq(-100, 100, by = 40)) +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-100, 100)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 8, color = "black", family = "Arial"),
    axis.text.x = element_text(size = 8, color = "black", family = "Arial"),
    axis.text.y = element_text(size = 8, color = "black", family = "Arial")
  )

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3e.png", plot = FIG3e, width = 9, height = 8, unit = "cm", dpi = 300)










##############F

library(readxl)
library(ggplot2)
library(ggrepel)  


file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3.xlsx"
data <- read_excel(file_path, sheet = "Fig.2f")

colnames(data)
str(data)

colnames(data) <- c("Name", "Delta_GHG", "Delta_Area")

data$Delta_GHG <- as.numeric(data$Delta_GHG)
data$Delta_Area <- as.numeric(data$Delta_Area)

data_clean <- data[, c("Name", "Delta_GHG", "Delta_Area")]
data_clean <- na.omit(data_clean)

summary(data_clean)

decoupling_labels <- data.frame(
  x = c(30, 50, 50, 30, -45, -45, -20),  
  y = c(90, 50, 20, -100, 90, -5, -100),  
  label = c("Weak Decoupling", "Coupling", "Weak Decoupling", 
            "Absolute Decoupling", "Absolute Decoupling", 
            "Relative Decoupling", "Negative Decoupling")  
)


FIG3f <- ggplot(data_clean, aes(x = Delta_Area, y = Delta_GHG, label = Name)) +
  geom_point(color = "skyblue", size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "gray50", 
              size = 0.5, linetype = "dashed") +
  
  geom_hline(yintercept = 0, color = "gray40", 
             size = 0.5, linetype = "dashed") +
 
  geom_vline(xintercept = 0, color = "gray40", 
             size = 0.5, linetype = "dashed") +
 
  geom_text_repel(
    size = 2.2,  
    color = "black", 
    family = "Arial",
    box.padding = 0.3, 
    point.padding = 0.4,
    segment.color = "gray20",
    segment.size = 0.3,
    force = 0.5,  
    max.overlaps = Inf, 
    segment.length = 0.2,
    min.segment.length = 0.1,
    max.time = 1,  
    max.iter = 10000  
  ) +
 
  geom_text(
    data = decoupling_labels,
    aes(x = x, y = y, label = label),
    color = "darkred",  
    size = 2,
    family = "Arial"
  ) +
  labs(
    x = "Delta Area (%)", 
    y = "Delta GHG (%)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-60, 60, by = 20)) +
  scale_y_continuous(breaks = seq(-100, 100, by = 40)) +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-100, 100)) +
  theme(
    
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    
    axis.line = element_line(color = "black", size = 0.5),
   
    axis.ticks = element_line(color = "black", size = 0.5),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 8, color = "black", family = "Arial"),
    axis.text.x = element_text(size = 8, color = "black", family = "Arial"),
    axis.text.y = element_text(size = 8, color = "black", family = "Arial")
  )


ggsave("/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3f.png", plot = FIG3f, width = 9, height = 8, unit = "cm", dpi = 300)












############################合并


if (!require("magick")) install.packages("magick")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("grid")) install.packages("grid")
if (!require("ggplot2")) install.packages("ggplot2") 

library(magick)
library(gridExtra)
library(grid)
library(ggplot2)


image_files <- c(
  "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3a.png",
  "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3b.png",
  "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3c.png",
  "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3d.png",
  "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3e.png",
  "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/FIG3f.png"
)


images_with_labels <- lapply(1:6, function(i) {
  
  img <- image_read(image_files[i])
  
  img <- image_background(img, "white")
  
  raster_img <- rasterGrob(img, interpolate = TRUE)
  
  annotated_grob <- grobTree(
    raster_img,
    textGrob(letters[i], x = 0.02, y = 0.98, 
             gp = gpar(fontsize = 12 , fontface = "bold", col = "black"),
             just = c("left", "top"))
  )
  
  return(annotated_grob)
})

combined_plot <- grid.arrange(
  grobs = images_with_labels,
  ncol = 2, nrow = 3
)

output_image_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG3/combined_image_with_labels.png"

ggsave(output_image_path, plot = combined_plot, width = 18, height = 24, units = "cm", dpi = 300, bg = "white")

message("图像合并成功！合并后的文件路径：", output_image_path)
