library(sf)
library(ggplot2)
library(dplyr)
library(tidyr) 
library(readxl)
library(cowplot)      
library(scales)
library(RColorBrewer) 
library(purrr) 


map_path  <- "/Users/dongjingjing/Desktop/GHG/FIG/shengfenbianjie.json"
data_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG5/FIG5.xlsx"
output_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG5/FIG5_Final_SmoothCurve_WithPoints.png"

china_map <- st_read(map_path, quiet = TRUE, check_ring_dir = FALSE) %>% st_make_valid()

zone_configs <- list(
  list(sheet = "a", letter = "a", label = "ZoneI",   data_id = c("Zone1", "ZoneI"),   prov = c("内蒙古自治区", "新疆维吾尔自治区", "甘肃省", "青海省", "西藏自治区", "陕西省", "山西省", "宁夏回族自治区")),
  list(sheet = "b", letter = "b", label = "ZoneII",  data_id = c("Zone2", "ZoneII"),  prov = c("黑龙江省", "吉林省", "辽宁省")),
  list(sheet = "c", letter = "c", label = "ZoneIII", data_id = c("Zone3", "ZoneIII"), prov = c("北京市", "天津市", "河北省", "河南省", "山东省")),
  list(sheet = "d", letter = "d", label = "ZoneIV",  data_id = c("Zone4", "ZoneIV"),  prov = c("浙江省", "上海市", "安徽省", "江西省", "湖南省", "湖北省", "四川省", "重庆市", "江苏省")),
  list(sheet = "e", letter = "e", label = "ZoneV",   data_id = c("Zone5", "ZoneV"),   prov = c("广东省", "广西壮族自治区", "海南省", "福建省")),
  list(sheet = "f", letter = "f", label = "ZoneVI",  data_id = c("Zone6", "ZoneVI"),  prov = c("云南省", "贵州省"))
)

common_theme <- theme_bw(base_size = 7.5, base_family = "Arial") +  
  theme(panel.grid = element_blank(), text = element_text(color = "black"),
        axis.text = element_text(color = "black"), legend.position = "none", plot.margin = margin(2, 2, 2, 2))

map_theme <- theme_void(base_family = "Arial") + theme(legend.position = "none", plot.margin = margin(0,0,0,0))


create_zone_plot <- function(config) {
  raw_data <- suppressMessages(read_excel(data_path, sheet = config$sheet, col_names = FALSE, range = cell_cols("A:I")))
  
  df_line <- raw_data[, 1:4] %>% setNames(c("Year", "Region", "Scenario", "GHG")) %>%
    mutate(Year = as.numeric(as.character(Year)), GHG = as.numeric(as.character(GHG)), Region = trimws(as.character(Region))) %>%
    filter(!is.na(Year), Region %in% config$data_id)
  
  df_bar <- raw_data[, 5:9] %>% setNames(c("Year", "Type", "Scenario", "Region", "Value")) %>%
    mutate(Year = as.numeric(as.character(Year)), Value = as.numeric(as.character(Value)), Region = trimws(as.character(Region))) %>%
    filter(!is.na(Year), Region %in% config$data_id, Year %in% c(2030, 2060))
  
  p_map <- ggplot() +
    geom_sf(data = subset(china_map, !name %in% config$prov), fill = "#F0F0F0", color = NA) +
    geom_sf(data = subset(china_map, name %in% config$prov), fill = "#B3CDE3", color = NA) +
    geom_sf(data = china_map, fill = NA, color = "black", linewidth = 0.15) +
    annotate("text", x = 74, y = 53, label = config$letter, family = "Arial", size = 10/.pt, fontface = "bold", hjust = 0, vjust = 1) +
    coord_sf(expand = FALSE) + map_theme
  
  hist_name <- "History"
  ref_hist <- df_line %>% filter(Scenario == hist_name, Year == 2023) %>% select(Value_2023 = GHG)
  if(nrow(ref_hist) == 0) return(NULL)
  
  ref_proj <- df_line %>% filter(Scenario != hist_name, Year == 2024) %>% select(Scenario, Value_2024 = GHG)
  scaling <- ref_proj %>% mutate(Ratio = ref_hist$Value_2023 / Value_2024)
  df_proj_adj <- df_line %>% filter(Scenario != hist_name) %>% left_join(scaling[,c("Scenario", "Ratio")], by = "Scenario") %>% mutate(GHG = GHG * Ratio)
  anchor <- scaling %>% select(Scenario) %>% mutate(Year = 2023, GHG = ref_hist$Value_2023, Region = df_line$Region[1])
  
  df_line_raw <- bind_rows(df_line %>% filter(Scenario == hist_name), df_proj_adj %>% select(-Ratio), anchor) %>% arrange(Scenario, Year)

  df_hist_part <- df_line_raw %>% filter(Scenario == hist_name)
  
  df_proj_smooth <- df_line_raw %>%
    filter(Scenario != hist_name) %>%
    group_by(Scenario) %>%
    summarise(
      res = list(predict(smooth.spline(Year, GHG, spar = 0.7), seq(2023, 2060, length.out = 200)))
    ) %>%
    mutate(Year = map(res, ~ .x$x), GHG = map(res, ~ .x$y)) %>%
    select(-res) %>% unnest(cols = c(Year, GHG)) %>% ungroup()

  df_line_smooth <- bind_rows(df_hist_part, df_proj_smooth) %>%
    mutate(
      U_Rate = ifelse(Scenario == hist_name, 0.05, 0.15 * (Year - 2023)/(2060-2023)),
      U_Rate = ifelse(U_Rate < 0, 0, U_Rate), 
      Lower = GHG*(1-U_Rate), Upper = GHG*(1+U_Rate)
    )
  
  all_scenarios <- unique(df_line_smooth$Scenario)
  my_colors <- setNames(scales::hue_pal()(length(all_scenarios)), all_scenarios)
  if(hist_name %in% names(my_colors)) my_colors[hist_name] <- "black"
  
  df_points_mark <- df_line_raw %>% filter(Year %in% c(2025, 2030, 2040, 2050, 2060))
  
  p_line <- ggplot(df_line_smooth, aes(x = Year, y = GHG, group = Scenario)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Scenario), alpha = 0.2, color = NA) +
    geom_vline(xintercept = 2024, linetype = "dotted", color = "black", linewidth = 0.4) +
    geom_line(aes(color = Scenario), linewidth = 0.5) +
    geom_point(data = df_points_mark, aes(x = Year, y = GHG, color = Scenario), shape = 21, fill = "white", size = 1.2, stroke = 0.6) +
    scale_x_continuous(limits = c(1980, 2060), breaks = c(1980, 2000, 2024, 2060)) +
    scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors) +
    common_theme + labs(y = expression(atop("Net GHG emission", paste("(Mt CO"[2]*"-eq)"))), x = NULL)
  
  df_bar_plot <- df_bar %>% group_by(Year, Type, Scenario) %>% summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")
  df_bar_total <- df_bar_plot %>% group_by(Year, Scenario) %>% summarize(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(ErrorSD = abs(TotalValue * 0.1), Ymin = TotalValue - ErrorSD, Ymax = TotalValue + ErrorSD)
  df_labels <- df_bar_total %>% select(Scenario) %>% distinct()
  
  p_bar <- ggplot() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    geom_col(data = df_bar_plot, aes(x = factor(Year), y = Value, fill = Type), position = "stack", width = 0.6) +
    scale_fill_brewer(palette = "Pastel1") +
    geom_errorbar(data = df_bar_total, aes(x = factor(Year), ymin = Ymin, ymax = Ymax), width = 0.2, linewidth = 0.4) +
    geom_point(data = df_bar_total, aes(x = factor(Year), y = TotalValue), shape = 21, fill = "white", color = "black", size = 1.2, stroke = 0.4) +
    geom_text(data = df_labels, aes(x = 1.5, y = Inf, label = Scenario), vjust = 1.5,  size = 7/.pt, family = "Arial") +
    facet_grid(. ~ Scenario) + common_theme +
    theme(panel.spacing = unit(0, "lines"), panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
          strip.background = element_blank(), strip.text = element_blank(), axis.title.x = element_blank()) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) + labs(y = "100 million USD")
  
  plot_grid(p_map, p_line, p_bar, ncol = 3, align = "h", axis = "tb", rel_widths = c(3.7, 7, 7))
}

plot_list <- list()
for (i in seq_along(zone_configs)) { plot_list[[i]] <- create_zone_plot(zone_configs[[i]]) }
plot_list <- plot_list[!sapply(plot_list, is.null)]
main_plot_grid <- plot_grid(plotlist = plot_list, ncol = 1)

raw_data_a <- suppressMessages(read_excel(data_path, sheet = "a", col_names = FALSE, range = cell_cols("A:I")))
df_line_legend_data <- raw_data_a[, 3] %>% setNames("Scenario") %>% distinct() %>% filter(!is.na(Scenario), Scenario != "Scenario")
df_bar_legend_data  <- raw_data_a[, 6] %>% setNames("Type") %>% distinct() %>% filter(!is.na(Type), Type != "Type")
hist_name <- "History"
all_scenarios_legend <- df_line_legend_data$Scenario
line_colors_legend <- setNames(scales::hue_pal()(length(all_scenarios_legend)), all_scenarios_legend)
if(hist_name %in% names(line_colors_legend)) line_colors_legend[hist_name] <- "black"

dummy_plot_line <- ggplot(df_line_legend_data, aes(x=1, y=1, color=Scenario, fill=Scenario)) +
  geom_line(linewidth = 1) + geom_ribbon(aes(ymin=0, ymax=2), alpha=0.2) + 
  scale_color_manual(values = line_colors_legend, name = NULL) + scale_fill_manual(values = line_colors_legend, name = NULL) +
  theme_bw(base_family = "Arial", base_size = 8) + theme(legend.position = "bottom", legend.direction = "horizontal",
                                                         legend.text = element_text(margin = margin(r = 20, unit = "pt"))) + guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

dummy_plot_bar <- ggplot(df_bar_legend_data, aes(x=1, y=1, fill=Type)) +
  geom_col() + scale_fill_brewer(palette = "Pastel1", name = NULL) +
  theme_bw(base_family = "Arial", base_size = 8) + theme(legend.position = "bottom", legend.direction = "horizontal",
                                                         legend.text = element_text(margin = margin(r = 20, unit = "pt"))) + guides(fill = guide_legend(nrow = 1))

legend_line <- get_legend(dummy_plot_line)
legend_bar <- get_legend(dummy_plot_bar)
combined_legend <- plot_grid(legend_line, legend_bar, ncol = 1, align = "v", rel_heights = c(1, 1))

final_plot_with_legend <- plot_grid(main_plot_grid, combined_legend, ncol = 1, rel_heights = c(24, 2))
ggsave(output_path, final_plot_with_legend, width = 18, height = 24, units = "cm", dpi = 600, bg = "white")
print(paste(":", output_path))