library(ggplot2)
library(cowplot)

all_jumps_time <- readRDS("/scratch/gev25289/workdir/georgia/delta/all_jumps_time.rds")
all_jumpsTime_9 <- all_jumps_time %>%
  filter(Cluster %in% "Cluster 9")

# Define a custom theme to avoid repetition
theme_custom <- theme(
  axis.title.x = element_blank(),    # Remove x-axis title
  axis.ticks.y = element_blank(),
  axis.text = element_text(size = 7, family = "Helvetica"),
  panel.border = element_rect(color = "black", size = 0.4, fill = NA),  # Add panel border
  plot.title = element_text(hjust = 0),
  strip.text = element_text(hjust = 0),
  panel.margin.x = unit(0.5, "cm"),
  panel.margin.y = unit(0.5, "cm"),
  legend.title = element_text(size = 6),
  legend.title.align = 1,
  legend.text = element_text(size = 6),
  legend.position = "bottom",       # Position legend at bottom
  legend.direction = "horizontal",  # Make the legend horizontal
  legend.key.size = unit(0.2, "cm"),
  legend.key.width = unit(1, "cm")
)

plotJumpsTime <- function(data, c) {
  p <- ggplot(data, aes(x = year, y = From, fill = ave)) +
    scale_fill_gradient2(low = 'lightblue1', mid = 'papayawhip', high = 'deepskyblue3', 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "Average Markov\nJump Counts/Year")) +
    labs(x="Month", y="Source", title = paste("Average Markov Jump Counts over Time: Cluster", c, sep = " ")) 
  return(p)
}

jumpsTime9_plot <- plotJumpsTime(all_jumpsTime_9, 9)
jumpsTime9_plot <- jumpsTime9_plot +
  facet_wrap(~To, ncol = 5) + #for all, 7 columns
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  theme_custom

# Extract the legend from the full dataset (all_jumpsTime_9) 
legend <- get_legend(jumpsTime9_plot)

# Define a function to generate the plots based on a filter value
generate_plots <- function(filter_value) {
  
  # Apply the filter to the data
  all_jumpsTime_9_from <- all_jumpsTime_9 %>% filter(From == filter_value)
  all_jumpsTime_9_to <- all_jumpsTime_9 %>% filter(To == filter_value)
  
  # FROM PH District X Plot
  fromPlot <- ggplot(all_jumpsTime_9_from, aes(x = year, y = To, fill = ave)) +
    scale_fill_gradient2(low = 'lightblue1', mid = 'papayawhip', high = 'deepskyblue3', 
                         midpoint = mean(all_jumpsTime_9$ave, na.rm = TRUE), 
                         limits = c(0, max(all_jumpsTime_9$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "")) +
    labs(x = "", y = paste("From", filter_value)) +  # Add axis labels
    geom_tile(color = "white", size = 0.01) +
    theme_tufte(base_family = "Helvetica") +
    theme_custom + theme(
      axis.text.x = element_blank(),     # Remove x-axis labels
      axis.ticks.x = element_blank(),    # Remove x-axis ticks
      axis.ticks = element_blank(),
      legend.position = "none")
  
  # TO PH District X Plot
  toPlot <- ggplot(all_jumpsTime_9_to, aes(x = year, y = From, fill = ave)) +
    scale_fill_gradient2(low = 'lightblue1', mid = 'papayawhip', high = 'deepskyblue3', 
                         midpoint = mean(all_jumpsTime_9$ave, na.rm = TRUE), 
                         limits = c(0, max(all_jumpsTime_9$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "")) +
    geom_tile(color = "white", size = 0.01) +
    labs(x = "", y = paste("To", filter_value)) +  # Add axis labels
    theme_tufte(base_family = "Helvetica") +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) + 
    theme_custom
  
  # Remove the legend 
  p1_no_legend <- fromPlot + theme(legend.position = "none")
  p2_no_legend <- toPlot + theme(legend.position = "none")
  
  # Combine the plots using plot_grid, aligning them vertically
  combined_plot <- plot_grid(p1_no_legend, p2_no_legend, ncol = 1, align = "v", rel_heights = c(1, 1.2))
  
  # Combine the plot and the horizontal legend in a single plot grid
  # final_plot <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.1))
  
  # Display the final plot with the legend at the bottom and horizontally aligned
  # combined_plot
  
  # Return the plot
  return(combined_plot)
}

# Call the function with public health district
Northwest <- generate_plots("Northwest")
NorthGeorgia <- generate_plots("North Georgia")
North <- generate_plots("North")
CobbDouglas <- generate_plots("Cobb-Douglas")
Fulton <- generate_plots("Fulton")
Clayton <- generate_plots("Clayton")
DeKalb <- generate_plots("DeKalb")
GNR <- generate_plots("GNR")
District4 <- generate_plots("District 4")
Northeast <- generate_plots("Northeast")
WestCentral <- generate_plots("West Central")
NorthCentral <- generate_plots("North Central")
EastCentral <- generate_plots("East Central")
SouthCentral <- generate_plots("South Central")
Southwest <- generate_plots("Southwest")
South <- generate_plots("South")
Southeast <- generate_plots("Southeast")
Coastal <- generate_plots("Coastal")
OOS <- generate_plots("OOS") 

all9 <- plot_grid(Northwest, NorthGeorgia, North, CobbDouglas, Fulton, Clayton, DeKalb, GNR, District4, Northeast,
          WestCentral, NorthCentral, EastCentral, SouthCentral, Southwest, South, Southeast, Coastal, OOS)
plot_grid(all9, legend,  ncol = 1, align = "v", rel_heights = c(1, 0.05)) 
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/jumpsTimesAll_9_plot.pdf", height = 20, width = 20)

main9 <- plot_grid(Coastal, OOS)
main9_with_legend <- plot_grid(main9, legend, ncol = 1, align = "v", rel_heights = c(1, 0.05))

main9_with_title <- ggdraw() +
  draw_label("Cluster 9", fontface = 'bold', size = 10, hjust = 0.5, vjust = -20) +
  draw_plot(main9_with_legend, y=-0.02)
main9_with_title
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/test.pdf", height = 5, width = 9)


























