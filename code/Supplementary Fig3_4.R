library(readxl)
library(tidyverse)

file_path_read <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG13_14/FIG13.xlsx"
output_dir <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG13_14"

if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

time_cols_index <- 2:5
time_period_names <- c("1980-1989", "1990-1999", "2000-2009", "2010-2023")
time_period_levels <- time_period_names

academic_palette <- c(
  "1980-1989" = "#4E84C4", 
  "1990-1999" = "#8DBC80", 
  "2000-2009" = "#EBB37D", 
  "2010-2023" = "#D67D7F"  
)

theme_compact_clean <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 8, color = "black"), 
    
    axis.title = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"), 

    axis.text.x = element_blank(),
    
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_text(size = 8, color = "black", face = "bold"),
    legend.text = element_text(size = 8, color = "black"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),     
    legend.box.margin = margin(t = 2, b = 5), 
    legend.key.size = unit(0.35, "cm"),                    
    
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5), 
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "white", colour = NA), 
    
    panel.spacing = unit(0.15, "cm"),        
    plot.margin = margin(2, 2, 2, 2, "mm"),  
    
    axis.line.y = element_line(colour = "black", linewidth = 0.35), 
    axis.ticks.y = element_line(colour = "black", linewidth = 0.35), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(), 
    
    strip.background = element_blank(),
    strip.text = element_blank() 
  )

# ================= PART 1: Sheet1 (Emission/Area) =================

data_sheet1 <- read_excel(file_path_read, sheet = "Sheet1", .name_repair = "minimal")
colnames(data_sheet1)[time_cols_index] <- time_period_names
strict_province_order <- unique(data_sheet1$Name)

long_data_sheet1 <- data_sheet1 %>%
  select(Name, all_of(time_period_names)) %>%
  pivot_longer(cols = all_of(time_period_names), names_to = "Time_Period", values_to = "Value") %>%
  mutate(
    Name = factor(Name, levels = strict_province_order), 
    Time_Period = factor(Time_Period, levels = time_period_levels)
  )

label_data_1 <- long_data_sheet1 %>% distinct(Name)

p1_final <- ggplot(long_data_sheet1, aes(x = Time_Period, y = Value)) +
  
  geom_line(aes(group = 1), color = "grey60", linewidth = 0.5) +
  geom_point(aes(color = Time_Period), size = 2.5) +
  geom_text(data = label_data_1, aes(label = Name, x = 2.5, y = Inf), 
            vjust = 1.8, size = 8 / .pt, fontface = "bold", family = "Arial", color = "black", inherit.aes = FALSE) +
  
  scale_color_manual(values = academic_palette) +
  coord_cartesian(ylim = c(0, 40)) + 
  
  facet_wrap(~ Name, ncol = 4) + 
  
  labs(title = "", x = "", y = "Emission/Area (t/ha)", color = "") +
  
  theme_compact_clean

ggsave(file.path(output_dir, "ghg_emission_area_line_chart_0_40.png"), 
       p1_final, width = 15, height = 20, unit="cm", dpi = 300)

# ================= PART 2: Sheet2 (Emission/Yield) =================

data_sheet2 <- read_excel(file_path_read, sheet = "Sheet2", .name_repair = "minimal")
colnames(data_sheet2)[time_cols_index] <- time_period_names
strict_province_order_2 <- unique(data_sheet2$Name)

long_data_sheet2 <- data_sheet2 %>%
  select(Name, all_of(time_period_names)) %>%
  pivot_longer(cols = all_of(time_period_names), names_to = "Time_Period", values_to = "Value") %>%
  mutate(
    Name = factor(Name, levels = strict_province_order_2), 
    Time_Period = factor(Time_Period, levels = time_period_levels)
  )

label_data_2 <- long_data_sheet2 %>% distinct(Name)

p2_final <- ggplot(long_data_sheet2, aes(x = Time_Period, y = Value)) +
  
  geom_line(aes(group = 1), color = "grey60", linewidth = 0.5) +
  geom_point(aes(color = Time_Period), size = 2.5) +
  
  geom_text(data = label_data_2, aes(label = Name, x = 2.5, y = Inf), 
            vjust = 1.8, size = 8 / .pt, fontface = "bold", family = "Arial", color = "black", inherit.aes = FALSE) +
  
  scale_color_manual(values = academic_palette) +
  
  coord_cartesian(ylim = c(0, 40)) + 
  
  facet_wrap(~ Name, ncol = 4) + 
  
  labs(title = "", x = "", y = "Emission/Yield (t/t)", color = "") + 
  
  theme_compact_clean

ggsave(file.path(output_dir, "ghg_emission_sheet2_line_chart_0_40.png"), 
       p2_final, width = 15, height = 18, unit="cm", dpi = 300)

print("Sheet 2 。")