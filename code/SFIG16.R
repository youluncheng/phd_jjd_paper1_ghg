library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)
library(RColorBrewer)

# 1. 基础路径设置
base_dir <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG16"
excel_path <- file.path(base_dir, "FIG16.xlsx")
map_path <- file.path(base_dir, "shengfenbianjie.json")
output_path <- file.path(base_dir, "SSP_2030_2060_NoBold_8pt.png")

if(!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

# 2. 数据读取
sheet_2030 <- read_excel(excel_path, sheet = "sheet2030")
sheet_2060 <- read_excel(excel_path, sheet = "sheet2060")
province_map <- st_read(map_path, quiet = TRUE) 

province_map$name <- toupper(province_map$name)
sheet_2030$name <- toupper(sheet_2030$name)
sheet_2060$name <- toupper(sheet_2060$name)

province_map_2030 <- province_map %>% left_join(sheet_2030, by = "name")
province_map_2060 <- province_map %>% left_join(sheet_2060, by = "name")

# 3. 配色与主题
my_palette_func <- colorRampPalette(brewer.pal(11, "Spectral"))
my_palette <- rev(my_palette_func(50)) 

theme_map_contrast <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 8, color = "black"),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"), 
    panel.grid = element_blank(),
    legend.position = "right", 
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8),
    legend.box.spacing = unit(0.1, "cm"), 
    legend.margin = margin(l = 0) 
  )

# 4. 绘图函数
make_map_contrast <- function(data, column) {
  ggplot(data) +
    geom_sf(aes_string(fill = column), color = "grey50", size = 0.1) + 
    coord_sf(expand = FALSE) +
    scale_fill_gradientn(
      colors = my_palette, 
      breaks = seq(0, 5500, by = 1000),
      limits = c(0, 5500),
      na.value = "grey90",
      name = "Food demand\n(ten thousand tons)", 
      guide = guide_colorbar(
        barwidth = 0.4,       
        barheight = 3.5,      
        title.position = "top",
        title.hjust = 0,
        ticks.colour = "white",
        frame.colour = "white",
        ticks.linewidth = 0.4,
        frame.linewidth = 0.4 
      )
    ) +
    theme_map_contrast
}

# 5. 生成地图列表
plots_2030 <- lapply(paste0("SSP", 1:5), function(s) make_map_contrast(province_map_2030, s))
plots_2060 <- lapply(paste0("SSP", 1:5), function(s) make_map_contrast(province_map_2060, s))

# 6. 构建顶部标题 (核心修改处)
# x = 0.5 是中点，我们将 x 设为 0.4 强制其向左偏移
col_header <- plot_grid(
  NULL, 
  ggdraw() + draw_label("2030", size = 8, fontfamily = "Arial", x = 0.4, hjust = 0.5),
  ggdraw() + draw_label("2060", size = 8, fontfamily = "Arial", x = 0.4, hjust = 0.5),
  ncol = 3,
  rel_widths = c(0.06, 1, 1) 
)

# 7. 构建行标签 (SSP)
make_row_label <- function(label) {
  ggdraw() + draw_label(label, angle = 90, size = 8, fontfamily = "Arial")
}
row_lbls <- lapply(paste0("SSP", 1:5), make_row_label)

# 8. 组合行
rows <- mapply(function(lab, p2030, p2060) {
  plot_grid(
    lab, p2030, p2060, 
    ncol = 3, 
    rel_widths = c(0.06, 1, 1), 
    align = "h", axis = "tb"
  )
}, row_lbls, plots_2030, plots_2060, SIMPLIFY = FALSE)

# 9. 组合总图
final_plot <- plot_grid(
  col_header,
  rows[[1]], rows[[2]], rows[[3]], rows[[4]], rows[[5]],
  ncol = 1,
  rel_heights = c(0.05, 1, 1, 1, 1, 1) 
)

# 10. 保存
ggsave(output_path, final_plot, width = 18, height = 24, units = "cm", dpi = 300, bg = "white")

print(paste("文件已保存，x=0.4 偏移版:", output_path))