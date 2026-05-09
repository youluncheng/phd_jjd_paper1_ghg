library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

file_path <- "/Users/dongjingjing/Desktop/GHG/FIG/FIG4/FIG42.xlsx"
out_dir <- dirname(file_path)

if (Sys.info()[['sysname']] == "Darwin") {
  quartzFonts(Arial = quartzFont(c("Arial", "Arial Bold", "Arial Italic", "Arial Bold Italic")))
}
font_family <- "Arial"

custom_colors <- c(
  "Fertilizer application"       = "#D1869E",
  "Manure application"           = "#F6A078",
  "Straw returning"              = "#EB8A8C",
  "Paddy rice"                   = "#A5CBE6",
  "Straw burning"                = "#B8E0D6",
  "Machinery energy"             = "#F5D1E3",
  "N leaching runoff"              = "#A0A0A0",
  "N deposition"       = "#E3C8EB",
  "Biological N fixation" ="#7ACCC0",
  "Carbon sequestration"         = "#E6CF7E",
  "Net emissions"                = "#666666"
)

legend_order <- names(custom_colors)
common_theme <- theme_bw() +
  theme(
    text = element_text(family = font_family, size = 8),
    axis.title = element_text(size = 8, color = "black"),
    axis.text  = element_text(size = 8, color = "black"),
    axis.ticks.length = unit(0.06, "cm"),
    axis.ticks = element_line(size = 0.2, colour = "black"),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.3),
    plot.margin = margin(5, 5, 5, 5)
  )

data_left <- read_excel(file_path, sheet = "Sheet1") %>%
  filter(Year <= 2060)

target_col_index <- grep("(?i)net\\s*emission", colnames(data_left))
if (length(target_col_index) > 0) {
  colnames(data_left)[target_col_index] <- "Net emissions"
} else if ("Total" %in% colnames(data_left)) {
  colnames(data_left)[colnames(data_left) == "Total"] <- "Net emissions"
}

plot_data_line <- data_left %>%
  select(Year, Scenario, `Net emissions`) %>%
  rename(Emission = `Net emissions`)

plot_data_area <- data_left %>%
  select(-`Net emissions`) %>%
  pivot_longer(cols = -c(Year, Scenario),
               names_to = "Measure",
               values_to = "Emission") %>%
  mutate(Measure = factor(Measure, levels = legend_order))
title_left_df <- plot_data_area %>%
  distinct(Scenario) %>%
  mutate(
    x = mean(range(plot_data_area$Year, na.rm = TRUE)),
    y = 1300 * 0.95
  )

unique_scenarios <- unique(plot_data_area$Scenario)
letter_df <- data.frame(
  Scenario = unique_scenarios,
  label = letters[1:length(unique_scenarios)] 
)

p1 <- ggplot() +
  geom_area(
    data = plot_data_area,
    aes(x = Year, y = Emission, fill = Measure),
    alpha = 1, colour = NA, show.legend = FALSE 
  ) +
  geom_line(
    data = plot_data_line,
    aes(x = Year, y = Emission),
    color = custom_colors["Net emissions"], 
    size = 0.8, show.legend = FALSE
  ) +
  geom_point(
    data = plot_data_line,
    aes(x = Year, y = Emission),
    color = custom_colors["Net emissions"], 
    size = 1.0, show.legend = FALSE
  ) +
  facet_wrap(~Scenario, ncol = 1) +
  geom_text(
    data = title_left_df,
    aes(x = x, y = y, label = Scenario),
    inherit.aes = FALSE,
    family = font_family,
    size = 8, size.unit = "pt",
    vjust = 1.3
  ) +
  geom_text(
    data = letter_df,
    aes(label = label, group = Scenario),
    x = -Inf, y = Inf,   
    hjust = -0.5,       
    vjust = 1.5,        
    family = font_family,
    fontface = "bold",   
    size = 10, size.unit = "pt",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = custom_colors, breaks = legend_order) +
  scale_y_continuous(limits = c(-200, 1300), expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050,2060), 
    expand = c(0, 0)
  ) +
  common_theme +
  theme(legend.position = "none") + 
  labs(x = NULL, y = expression("GHG emission (Mt CO"[2]*"-eq)"))

raw_data_right <- read_excel(file_path, sheet = "Sheet2")
colnames(raw_data_right)[1:2] <- c("Year", "Scenario")
raw_data_right$Year <- as.numeric(as.character(raw_data_right$Year))

process_single_scenario <- function(df) {
  start_year <- 2024 
  end_year <- 2060
  
  df_sub <- df %>% filter(Year %in% c(start_year, end_year))
  if(nrow(df_sub) == 0) stop(paste("Sheet2 中找不到年份数据"))
  
  df_long <- df_sub %>%
    pivot_longer(cols = -c(Year, Scenario), names_to = "Measure", values_to = "Value") %>%
    mutate(Measure = trimws(Measure)) %>%
    mutate(Measure = case_when(
      Measure == "Total" ~ "Net emissions",
      grepl("(?i)^net\\s*emission", Measure) ~ "Net emissions",
      TRUE ~ Measure
    ))
  
  df_wide <- df_long %>%
    pivot_wider(names_from = Year, values_from = Value, names_prefix = "Y")
  
  total_row <- df_wide %>% filter(Measure == "Net emissions")
  val_start <- total_row[[paste0("Y", start_year)]]
  val_end   <- total_row[[paste0("Y", end_year)]]
  
  measure_order_calc <- legend_order[legend_order != "Net emissions"]
  
  measures_df <- df_wide %>%
    filter(Measure %in% measure_order_calc) %>%
    mutate(Diff = .data[[paste0("Y", end_year)]] - .data[[paste0("Y", start_year)]]) %>%
    mutate(Measure = factor(Measure, levels = measure_order_calc))
  
  step_1 <- data.frame(Scenario = unique(df$Scenario), Measure = "Net emissions", Diff = NA_real_,
                       ymin = 0, ymax = val_start, x_id = 1)
  
  current_y <- val_start
  steps_mid <- measures_df %>%
    mutate(ymin = current_y + c(0, head(cumsum(Diff), -1)),
           ymax = current_y + cumsum(Diff),
           x_id = row_number() + 1,
           Scenario = unique(df$Scenario))
  
  step_last <- data.frame(Scenario = unique(df$Scenario), Measure = "Net emissions", Diff = NA_real_,
                          ymin = 0, ymax = val_end, x_id = max(steps_mid$x_id) + 1)
  
  bind_rows(step_1, steps_mid, step_last)
}

plot_data_right <- bind_rows(lapply(split(raw_data_right, raw_data_right$Scenario), process_single_scenario)) %>%
  mutate(Measure = factor(Measure, levels = legend_order))

label_df <- plot_data_right %>%
  filter(!is.na(Diff)) %>%
  mutate(Diff_r = round(Diff, 1)) %>%
  group_by(Scenario) %>%
  mutate(
    x_lab = x_id,
    y_base = ifelse(Diff_r >= 0, ymax, ymin),
    v_lab  = ifelse(Diff_r >= 0, -1.2, 2.2),
    dy = case_when(
      Diff_r == -133.3 ~ -60,
      Diff_r == 0.3    ~  60,
      Diff_r == -86.6  ~ -70,
      TRUE ~ 0
    ),
    y_lab = y_base + dy,
    lab = sprintf("%.1f", Diff_r)
  ) %>%
  ungroup()

legend_df <- expand.grid(Scenario = unique(plot_data_right$Scenario), Measure = legend_order) %>%
  mutate(Measure = factor(Measure, levels = legend_order), x_id = 1, y = 0)

max_xid <- max(plot_data_right$x_id)
title_right_df <- plot_data_right %>%
  distinct(Scenario) %>%
  mutate(x = (1 + max_xid) / 2, y = 1800 * 0.95)

p2 <- ggplot(plot_data_right) +
  geom_rect(
    aes(xmin = x_id - 0.45, xmax = x_id + 0.45, ymin = ymin, ymax = ymax, fill = Measure),
    colour = NA, show.legend = FALSE 
  ) +
  geom_text(
    data = label_df, 
    aes(x = x_lab, y = y_lab, vjust = v_lab, label = lab),
    size = 8, size.unit = "pt", family = font_family, color = "black", inherit.aes = FALSE
  ) +
  geom_point(
    data = legend_df, aes(x = x_id, y = y, fill = Measure),
    shape = 22, size = 0, alpha = 0,
    inherit.aes = FALSE, show.legend = TRUE
  ) +
  facet_wrap(~Scenario, ncol = 1, scales = "free_x") +
  geom_text(
    data = title_right_df, aes(x = x, y = y, label = Scenario),
    inherit.aes = FALSE, family = font_family, fontface = "bold", size = 8, size.unit = "pt", vjust = 1.3
  ) +
  scale_fill_manual(values = custom_colors, breaks = legend_order, drop = FALSE, name = NULL) +
  scale_y_continuous(limits = c(0, 1200), expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(1, max_xid),
    labels = c("2024", "2060")
  ) +
  common_theme +
  labs(x = NULL, y = expression("GHG emission (Mt CO"[2]*"-eq)"))

p_combined <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(1, 3), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, family = font_family),
    legend.key.size = unit(0.3, "cm")
  ) &
  guides(
    fill = guide_legend(
      nrow = 3, byrow = TRUE,
      override.aes = list(shape = 22, size = 2, alpha = 1, colour = NA)
    )
  )

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
pdf_path <- file.path(out_dir, "FIG4_with_labels.pdf")
png_path <- file.path(out_dir, "FIG4_Final_Layout_Labels.png")

ggsave(filename = pdf_path, plot = p_combined, width = 15, height = 18, units = "cm", device = "pdf")
ggsave(filename = png_path, plot = p_combined, width = 18, height = 18, units = "cm", dpi = 600)

cat("PDF saved to:", pdf_path, "\n")
cat("PNG saved to:", png_path, "\n")