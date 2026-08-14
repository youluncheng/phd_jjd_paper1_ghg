
#####################################################aaaaaaaaaaaaaaa###########################################
#####################################################aaaaaaaaaaaaaaa###########################################
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("sysfonts")) install.packages("sysfonts")
if (!require("showtext")) install.packages("showtext")

library(ggplot2)
library(readxl)
library(scales)
library(stringr)
library(ggExtra)
library(cowplot)
library(ggrepel)
library(showtext)
library(sysfonts) 

showtext_auto()
arial_path_1 <- "/Library/Fonts/Arial.ttf"
arial_path_2 <- "/System/Library/Fonts/Supplemental/Arial.ttf"

if (file.exists(arial_path_1)) {
  font_add("Arial", regular = arial_path_1)
} else if (file.exists(arial_path_2)) {
  font_add("Arial", regular = arial_path_2)
} else {
  message("Warning: Arial font file not found in standard paths. Trying system default.")
}
showtext_opts(dpi = 600)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG16.xlsx"
data <- read_excel(file_path, sheet = 1) 

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
size_breaks <- unique(quantile(data$Bubble_Size, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))

if(length(size_breaks) < 2) {
  size_labels <- "All"
  data$Size_Category <- as.factor(data$Bubble_Size)
  size_values <- 3 
} else {
  size_labels <- paste0(round(size_breaks[-length(size_breaks)], 1), 
                        "-", 
                        round(size_breaks[-1], 1))
  data$Size_Category <- cut(
    data$Bubble_Size,
    breaks = size_breaks,
    labels = size_labels,
    include.lowest = TRUE
  )
  size_values <- c(1, 2, 3, 4)
}

custom_colors <- c("#1E3A8A", "#3B82F6", "#34D399", "#10B981", "#b8cdab", 
                   "#FBBF24", "#FDBA74", "#F97316", "#FB923C", "#DC2626")

p <- ggplot(data, aes(x = X_Value, y = Y_Value)) +
  geom_point(aes(size = Size_Category, color = Color_Value), alpha = 0.7, fill = NA) +
  
  geom_text_repel(
    aes(label = FirstCol_Label),
    size = 8 / .pt,     
    color = "black", 
    family = "Arial",    
    box.padding = 0.2,   
    point.padding = 0.2, 
    max.overlaps = 25,   
    force = 1,            
    segment.size = 0.2    
  ) +
  
  geom_vline(xintercept = 0.5, color = "black", linewidth = 0.4) + 
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.4) + 
  
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = 0.05)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0.05)) +
  
  scale_size_manual(
    name = expression(paste("Carbon sequestration (Mt CO"[2]*"-eq)")), 
    values = size_values,
    breaks = if(exists("size_labels")) size_labels else waiver(),
    labels = if(exists("size_labels")) size_labels else waiver(),
    guide = guide_legend(
      order = 1, 
      title.position = "left", title.hjust = 0.5, 
      label.position = "bottom", label.hjust = 0.5, 
      keyheight = unit(0.1, "cm"), keywidth = unit(0.1, "cm") 
    )
  ) +
  
  scale_color_gradientn(
    name = "Carbon sequestration intensity (t/ha)", 
    colours = custom_colors,
    limits = c(0, 0.2),
    breaks = seq(0, 0.2, by = 0.05),
    guide = guide_colorbar(
      order = 2, 
      barwidth = 0.2, barheight = 4,
      ticks = TRUE, ticks.colour = "black",
      title.position = "left", title.hjust = 0.7, title.vjust = 0.5, 
      frame.colour = "black", frame.linewidth = 0.2, 
      barcolour = "#D3D3D3", 
      title.theme = element_text(margin = margin(r = 12))
    )
  ) +
  
  labs(
    x = "Proportion of dryland area", 
    y = "Food crop planting area"    
  ) +
  
  theme_minimal(base_family = "Arial") +
  theme(
    aspect.ratio = 1, 
    
    text = element_text(family = "Arial", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none", 
    axis.text = element_text(size = 8, color = "black", family = "Arial"), 
    axis.title.y = element_text(size = 8, color = "black", family = "Arial", margin = margin(r = -110)), 
    axis.title.x = element_text(size = 8, color = "black", family = "Arial", margin = margin(t = -40)),
    legend.text = element_text(size = 6, color = "black", family = "Arial"), 
    legend.title = element_text(size = 6, color = "black", family = "Arial"), 
    
    strip.text = element_text(size = 8, color = "black", family = "Arial"), 
    plot.title = element_text(size = 8, color = "black", family = "Arial"), 
    plot.subtitle = element_text(size = 8, color = "black", family = "Arial"), 
    plot.caption = element_text(size = 8, color = "black", family = "Arial") 
  ) +
  
  annotate("text", x = 0.25, y = 0.96, 
           label = "Carbon~sequestration", 
           parse = TRUE,                      
           size = 8 / .pt, 
           hjust = 0.5, vjust = 0, color = "black", family = "Arial")

p_with_marginal <- ggExtra::ggMarginal(p, type = "density", fill = "skyblue", size = 10)

legend <- get_legend(p + theme(legend.position = "right", 
                               legend.title = element_text(angle = -270),
                               legend.margin = margin(t = 10, r = 1, b = -10, l = 0)))

FIG16 <- plot_grid(p_with_marginal, legend, rel_widths = c(0.7, 0.15)) 

ggsave("/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG16/FIG16a.png", plot = FIG16, width = 9, height = 8, unit = "cm", dpi = 600)



if (!require("ggrepel")) install.packages("ggrepel")
if (!require("sysfonts")) install.packages("sysfonts")
if (!require("showtext")) install.packages("showtext")
library(readxl)
library(ggplot2)
library(ggrepel)  
library(showtext)
library(sysfonts)

showtext_auto()

arial_path_1 <- "/Library/Fonts/Arial.ttf"
arial_path_2 <- "/System/Library/Fonts/Supplemental/Arial.ttf"

if (file.exists(arial_path_1)) {
  font_add("Arial", regular = arial_path_1)
} else if (file.exists(arial_path_2)) {
  font_add("Arial", regular = arial_path_2)
} else {
  message("Warning: Arial font file not found in standard paths. Trying system default.")
}


showtext_opts(dpi = 600)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG16.xlsx"
data <- read_excel(file_path, sheet = "Sheet2")

colnames(data) <- c("Name", "Delta_Net_GHG", "Delta_Yield")

data$Delta_Net_GHG <- as.numeric(data$Delta_Net_GHG)
data$Delta_Yield <- as.numeric(data$Delta_Yield)
data_clean <- data[, c("Name", "Delta_Net_GHG", "Delta_Yield")]
data_clean <- na.omit(data_clean)

decoupling_labels <- data.frame(
  x = c(70, 90, 70, 70, -65, -65, -60),  
  y = c(90, 50, 20, -100, 90, -5, -100), 
  label = c("Weak Decoupling", "Coupling", "Weak Decoupling", 
            "Absolute Decoupling", "Absolute Decoupling", 
            "Relative Decoupling", "Negative Decoupling") 
)

FIG3e <- ggplot(data_clean, aes(x = Delta_Yield, y = Delta_Net_GHG, label = Name)) +
  geom_point(color = "skyblue", size = 1) +
  
  geom_abline(slope = 1, intercept = 0, color = "gray50", 
              size = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray40", 
             size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray40", 
             size = 0.5, linetype = "dashed") +

  geom_text_repel(
    size = 8 / .pt,    
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
    size = 8 / .pt,     
    family = "Arial"    
  ) +
  
  labs(
    x = "Delta Yield (%)", 
    y = "Delta C sequestration (%)" 
  ) +

  theme_minimal(base_family = "Arial") +
  
  scale_x_continuous(breaks = seq(-100, 100, by = 40)) +
  scale_y_continuous(breaks = seq(-100, 100, by = 40)) +
  coord_cartesian(xlim = c(-100, 100), ylim = c(-100, 100)) +
  
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
ggsave("/Users/dongjingjing/Desktop/GHG/FIG/SFIG/FIG16/FIG16b.png", plot = FIG3e, width = 9, height = 8, unit = "cm", dpi = 600)
