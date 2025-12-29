library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)

input_path <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG9/FIG9.xlsx"
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



my_colors <- c(
  "Fertilizer Application"        = "#BD7795",
  "Manure Application"            = "#F39865",
  "Straw Returning"               = "#EC6E66",
  "Straw Burning"                 = "#91CCC0",
  "Leaching/Runoff"               = "#7C7979",
  "Paddy Rice Cultivation"        = "#7FABD1",
  "Irrigation Energy"             = "#EEB6D4",
  "Agricultural Machinery Energy" = "#2D8875"
)


p <- ggplot(df_long, aes(x = Year, y = Value, fill = Emission_Source)) +
  geom_area(position = "stack", 
            alpha = 0.9, 
            color = "white", 
            linewidth = 0.1) +
  
  scale_fill_manual(values = my_colors) +
  
 
  labs(x = NULL, 
       y = expression("GHG (MT " * CO[2] * "-eq/yr)"), 
       fill = NULL) +  
  
  theme_minimal(base_size = 8, base_family = "ArialMT") +
  
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  theme(
  
    panel.grid = element_blank(),           
    axis.line = element_line(color = "black", linewidth = 0.3), 
    axis.ticks = element_line(color = "black", linewidth = 0.3),
 
    legend.position = "top", 
    legend.margin = margin(t = 0, r = 35, b = 0, l = 0),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"), 
    
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8, color = "black") 
  )


output_dir <- "/Users/a18388581658/Desktop/R-FILE/R-file/SI/Linechart"
output_filename <- "GHG_EMISSION.png"
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