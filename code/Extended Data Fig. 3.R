library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)
library(stats) 

input_path <- "/Users/dongjingjing/Desktop/GHG/FIG/Extended Data Fig. 3/AddS3.xlsx"
df_wide <- read_excel(input_path)

if("sum" %in% names(df_wide)){
  df_plot_data <- df_wide %>% select(-sum)
} else {
  df_plot_data <- df_wide
}

df_long <- pivot_longer(df_plot_data, 
                        cols = -Year, 
                        names_to = "Emission_Source", 
                        values_to = "Value")


# -------------------------------------------------------------
df_smooth <- df_long %>%
  group_by(Emission_Source) %>%
  summarise(
   
    Year_smooth = seq(min(Year), max(Year), length.out = 100), 
    Value_smooth = spline(Year, Value, xout = seq(min(Year), max(Year), length.out = 100))$y,
    .groups = "drop"
  )


my_colors <- c(
  "Fertilizer application" = "#9E4C6E", "Manure application" = "#F16C27", "Straw returning" = "#D74C4F", 
  "Paddy rice" = "#7FABD1", "Straw burning"= "#91CCC0", "Machinery energy" = "#EEB6D4", 
  "N leaching/runoff"= "#585858", "N deposition emissions"= "#D1A1E2", "Microbial N fixation" = "#2D8875", 
  "Carbon sequestration"="#C2A12B"
)

p <- ggplot(df_smooth, aes(x = Year_smooth, y = Value_smooth, fill = Emission_Source)) +
  geom_area(position = "stack", 
            alpha = 0.9, 
            color = "white", 
            linewidth = 0.1) +
  
  scale_fill_manual(values = my_colors) +
  
  guides(fill = guide_legend(nrow = 3)) +
  
  labs(x = NULL, 
       y = expression("GHG (MT " * CO[2] * "-eq/yr)"), 
       fill = NULL) +  
  
  theme_minimal(base_size = 8, base_family = "ArialMT") +
  
  scale_x_continuous(expand = c(0, 0)) +
  
 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-120, 1080),
                     breaks = seq(-120, 1080, 120)) + 
  
  theme(
    panel.grid = element_blank(),           
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    axis.line = element_blank(),
    
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    
    legend.position = "top", 
    legend.margin = margin(t = 0, r = 35, b = 0, l = 0),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"), 
    
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8, color = "black") 
  )

output_dir <- "/Users/dongjingjing/Desktop/GHG/FIG/AddS3"
output_filename <- "GHG_EMISSION_SMOOTH.png"
full_output_path <- file.path(output_dir, output_filename)

ggsave(filename = full_output_path, 
       plot = p, 
       width = 15, 
       height = 10, 
       units = "cm", 
       dpi = 300, 
       bg = "white")

message(" ", full_output_path)
print(p)