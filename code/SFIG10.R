
library(sf)
library(ggplot2)
library(ggforce)
library(dplyr)
library(readxl)
library(tidyr)
library(lwgeom)
library(patchwork)
library(stringr)

# 设置全局字体为 Arial
theme_set(theme_minimal(base_family = "Arial"))

custom_coords <- data.frame(
  yingwen = c(
    "XJ","GS", "QH","XZ",
    "NMG", "NX", "SC","YN",
    "SN","SX","CQ", "GZ",
    "HE","HEN", "HUB", "HAN", "GX","HUN",
    "BJ", "AH","JX", "GD",
    "TJ", "SD", "JS", "ZJ", "FJ",
    "HLJ","JL", "LN", "SH"
  ),
  custom_lon = c(
    79, 79,79,79,
    88, 88, 88, 88,
    97, 97, 97, 97,
    106, 106, 106, 106, 106, 106,
    115, 115, 115, 115,
    124, 124, 124, 124, 124,
    133, 133, 133,133
  ),
  custom_lat = c(
    51, 43, 35, 27,
    49, 41, 33, 25,
    51, 43, 35, 27,
    52, 44, 36, 28, 20,12,
    45, 38, 29, 22,
    50, 42, 34, 26, 18,
    52, 44, 36, 28
  ),
  stringsAsFactors = FALSE
)


geojson_path <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG10/绘图文件/shengfenbianjie.json"
if (!file.exists(geojson_path)) stop("GeoJSON文件不存在：", geojson_path)
china_geojson <- st_read(geojson_path, quiet = TRUE) %>% st_make_valid()


df <- read_excel("/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG10/绘图文件/FIG10.xlsx", sheet = "Sheet2")

names(df) <- str_trim(names(df))
names(df) <- str_replace_all(names(df), "-", " ")
df <- df %>% mutate(across(-c(name, year), as.numeric))


df_long <- df %>%
  pivot_longer(
    cols = -c(name, year),
    names_to = "处理类型",
    values_to = "数值"
  ) %>%
  mutate(
    年代分组 = case_when(
      year >= 1980 & year <= 1989 ~ "1980-1989",
      year >= 1990 & year <= 1999 ~ "1990-1999",
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2023 ~ "2010-2023",
      TRUE ~ "其他年份"
    )
  ) %>%
  filter(年代分组 != "其他年份") %>%
  group_by(name, 年代分组, 处理类型) %>%
  summarise(数值 = mean(数值, na.rm = TRUE), .groups = "drop") %>%
  group_by(name, 年代分组) %>%
  mutate(总数值 = sum(数值, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(yingwen = name)


pie_data <- df_long %>%
  left_join(custom_coords, by = "yingwen") %>%
  filter(!is.na(custom_lon) & !is.na(custom_lat)) %>%
  group_by(yingwen, 年代分组) %>%
  mutate(
    比例 = 数值 / sum(数值, na.rm = TRUE),
    累积比例 = cumsum(比例),
    start_angle = 2 * pi * lag(累积比例, default = 0),
    end_angle = 2 * pi * 累积比例
  ) %>%
  ungroup()


legend_colors <- c(
  "Fertilizer Application" = "#BD7795",
  "Manure Application" = "#F39865",
  "Straw Returning" = "#EC6E66",
  "Straw Burning" = "#91CCC0",
  "Leaching/Runoff" = "#7C7979",
  "Paddy Rice Cultivation" = "#7FABD1",
  "Irrigation Energy" = "#EEB6D4",
  "Agricultural Machinery Energy" = "#2D8875"
)
existing_types <- unique(pie_data$处理类型)
legend_colors <- legend_colors[names(legend_colors) %in% existing_types]


plot_by_decade <- function(decade_data, decade_name) {
  ggplot() +
    geom_sf(data = china_geojson, fill = "#f5f5f5", color = "gray50", linewidth = 0.1) +
   
    annotate("text", x = 105, y = 59, label = decade_name,
             size = 3, family = "Arial", fontface = "plain") +
    geom_arc_bar(
      data = decade_data,
      aes(
        x0 = custom_lon,
        y0 = custom_lat,
        r0 = 0,
        r = sqrt(总数值) * 3,
        start = start_angle,
        end = end_angle,
        fill = 处理类型 
      ),
      color = "white",
      linewidth = 0.1
    ) +
    geom_text(
      data = decade_data %>% distinct(yingwen, custom_lon, custom_lat),
      aes(x = custom_lon, y = custom_lat + 3.5, label = yingwen),
      size = 2.5,
      family = "Arial",
      color = "black"
    ) +
    scale_fill_manual(
      values = legend_colors, 
      name = NULL,
      guide = guide_legend(
        nrow = 2,
        byrow = TRUE,
        keywidth = unit(0.3, "cm"),
        keyheight = unit(0.3, "cm")
      )
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(t = 10, r = 5, b = 5, l = 10), 
    )
}


decades <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
plot_list <- list()
for (i in seq_along(decades)) {
  data_current <- filter(pie_data, 年代分组 == decades[i])
  plot_list[[i]] <- plot_by_decade(data_current, decades[i])
}


combined_plot <- wrap_plots(plot_list, nrow = 2, guides = "collect") +
  plot_annotation(tag_levels = 'a') & 
  theme(
    plot.tag = element_text(
      size = 8, 
      face = 'bold', 
      family = "Arial"
    ),
    plot.tag.position = c(0, 1), 
    plot.margin = margin(2, 1, 1, 1),
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(family = "Arial", size = 6),
    legend.title = element_blank(),
    legend.box.margin = margin(t = 0, b = 0)
  )


OUTPUT_DIR <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG10"
FILE_NAME <- "FIG10"
NEW_WIDTH_CM <- 12
NEW_HEIGHT_CM <- 12

print(combined_plot)

ggsave(
  paste0(OUTPUT_DIR, "/", FILE_NAME, ".png"),
  combined_plot,
  width = NEW_WIDTH_CM,
  height = NEW_HEIGHT_CM,
  units = "cm",
  dpi = 300
)

ggsave(
  paste0(OUTPUT_DIR, "/", FILE_NAME, ".pdf"),
  combined_plot,
  width = NEW_WIDTH_CM,
  height = NEW_HEIGHT_CM,
  units = "cm",
  dpi = 300,
  device = "pdf"
)

cat("绘图已完成。标签已改为小写 (a, b, c, d)，图中年份已取消加粗。\n")