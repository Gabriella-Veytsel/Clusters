library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(ggthemes)

ClusterLevels24 <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6",  "Cluster 9", 
                     "Cluster 11", "Cluster 12", "Cluster 13","Cluster 14", "Cluster 15", "Cluster 16", "Cluster 17", 
                     "Cluster 18", "Cluster 20", "Cluster 23", "Cluster 24", "Cluster 28", "Cluster 29", "Cluster 30", 
                     "Cluster 32", "Cluster 33", "Cluster 34")
ph_levels <- c("Northwest", "North Georgia", "North", "Cobb-Douglas", "Fulton", "Clayton", "DeKalb", "GNR", "District 4", "Northeast", "West Central", "North Central", "East Central", "South Central", "Southwest", "South", "Southeast", "Coastal", "OOS")

# totalJumps <- function(c) {
#   # Define the input file path
#   input_dir <- paste("/scratch/gev25289/workdir/georgia/delta/clusters/individual_dta/rep0/c", c, "/jumptimes.txt", sep = "")
#   
#   # Attempt to read the file and handle errors
#   jumps <- tryCatch(
#     read.table(input_dir, header = TRUE),
#     error = function(e) {
#       message(sprintf("Error reading file for cluster %s: %s", c, e))
#       return(NULL)  # Return NULL if an error occurs
#     }
#   )
#   
#   # If reading the file failed, return NULL
#   if (is.null(jumps)) return(NULL)
#   
#   jumps <- read.table(input_dir, header = TRUE)
#   
#   state <- n_distinct(jumps$state) #Number of total state counts
#   jumps$from_to = paste(jumps$from,jumps$to, sep=".")
#   jumps <- jumps %>% mutate(time = max(cluster_tips[[c]]$`numeric date`) - as.numeric(time))
#   jumps$year <- format(date_decimal(jumps$time), "%Y-%m")
#   
#   #From, To
#   count_total <- jumps %>% group_by(from_to) %>% count()
#   count_total <- count_total %>% separate_wider_delim(from_to, ".", names = c("From", "To"))
#   count_total <- count_total %>% mutate(ave=n/state) %>% mutate(Cluster = c)
#   return(count_total)
# }
# 
cluster_tips <- readRDS("/scratch/gev25289/workdir/georgia/delta/rds/cluster_tips.rds")
cluster_index <- c(1, 2, 3, 4, 5, 6, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 23, 24, 28, 29, 30, 32, 33, 34)
# c1_data <- jumpTimes(1) #example
# 
# # Apply function to all clusters and combine the results
# all_jumps <- bind_rows(lapply(cluster_index, totalJumps))
# saveRDS(all_jumps, "/scratch/gev25289/workdir/georgia/delta/all_jumps.rds")

all_jumps <- readRDS("/scratch/gev25289/workdir/georgia/delta/all_jumps.rds")
all_jumps$Cluster <- paste("Cluster", all_jumps$Cluster, sep = " ")
all_jumps$Cluster <- factor(all_jumps$Cluster, levels = ClusterLevels24)   
all_jumps$From <- gsub("_", " ", all_jumps$From)
all_jumps$To <- gsub("_", " ", all_jumps$To)
all_jumps$From <- factor(all_jumps$From, levels = ph_levels)
all_jumps$To <- factor(all_jumps$To, levels = ph_levels)

# Function to Plot Heatmaps
##############################################################################################################################################
plotJumps <- function(data, color) {
  p <- ggplot(data, aes(x = To, y = From, fill = ave)) +
    scale_fill_gradient2(low = 'wheat', mid = 'papayawhip', high = color, 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "Average Markov\nJump Counts")) 
  return(p)
}

# Apply Function to All Clusters
##############################################################################################################################################
jumps_plot <- plotJumps(all_jumps, "skyblue")
gg <- jumps_plot + 
  facet_wrap(~ Cluster, nrow = 4)  + # Facet by Cluster
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  labs(x="Sink", y="Source", title="Average Markov Jump Counts") +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 5),
        axis.text.x = element_text(angle = 90, hjust = 0.5), 
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        panel.margin.x = unit(0.5, "cm"),
        panel.margin.y = unit(0.5, "cm"),
        legend.title = element_text(size = 6),
        legend.title.align = 1,
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"))
gg
ggsave("/scratch/gev25289/workdir/georgia/delta/jumps_plot.png", dpi = 300, height =8, width = 11, limitsize=FALSE)

rows_per_cluster <- lapply(cluster_index, function(i) nrow(cluster_tips[[i]]))
names(rows_per_cluster) <- paste0("Cluster ", cluster_index)

small_clusters <- rows_per_cluster[rows_per_cluster < 10]
medium_clusters <- rows_per_cluster[rows_per_cluster >= 10 & rows_per_cluster < 100]
large_clusters <- rows_per_cluster[rows_per_cluster >= 100]

all_jumps_9 <- all_jumps %>%
  filter(Cluster %in% "Cluster 9")

all_jumps_34 <- all_jumps %>%
  filter(Cluster %in% "Cluster 34")

# Differences in clusters
#########################################################
c34 <- cluster_tips [[34]]
c9 <- cluster_tips[[9]]
c <- bind_rows(c9, c34)
ggplot(c, aes(location)) + geom_bar(aes(fill = Cluster))
table(c9$location)
table(c34$location)

# Apply Function to Small Clusters
##############################################################################################################################################
all_jumps_small <- all_jumps %>%
  filter(Cluster %in% c("Cluster 2", "Cluster 6", "Cluster 11", "Cluster 13", "Cluster 15", "Cluster 20", "Cluster 28", "Cluster 29", "Cluster 33"))

small_jumps_plot <- plotJumps(all_jumps_small, "skyblue")
gg <- small_jumps_plot + 
  facet_wrap(~ Cluster, nrow = 2) + # Facet by Cluster
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  labs(x="Sink", y="Source", title="Average Markov Jump Counts, Small Clusters") +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 5),
        axis.text.x = element_text(angle = 90, hjust = 0.5), 
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0, size=10),
        strip.text = element_text(hjust = 0),
        panel.margin.x = unit(0.5, "cm"),
        panel.margin.y = unit(0.5, "cm"),
        legend.title = element_text(size = 6),
        legend.title.align = 1,
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"))
gg
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/small_jumps_plot.png", dpi = 300, height =4, width = 7, limitsize=FALSE)

# Apply Function to Medium Clusters
##############################################################################################################################################
all_jumps_medium <- all_jumps %>%
  filter(Cluster %in% c("Cluster 18", "Cluster 23", "Cluster 3", "Cluster 12", "Cluster 5", "Cluster 30", "Cluster 17", "Cluster 32", "Cluster 4", "Cluster 14", "Cluster 24", "Cluster 1", "Cluster 16"))

medium_jumps_plot <- plotJumps(all_jumps_medium, "lightgreen")
gg <- medium_jumps_plot + 
  facet_wrap(~ Cluster, nrow = 3) + # Facet by Cluster
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  labs(x="Sink", y="Source", title="Average Markov Jump Counts, Medium Clusters") +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 5),
        axis.text.x = element_text(angle = 90, hjust = 0.5), 
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0, size=10),
        strip.text = element_text(hjust = 0),
        panel.margin.x = unit(0.5, "cm"),
        panel.margin.y = unit(0.5, "cm"),
        legend.title = element_text(size = 6),
        legend.title.align = 1,
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"))
gg
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/medium_jumps_plot.png", dpi = 300, height =6, width = 8, limitsize=FALSE)

# Apply Function to Large Clusters
##############################################################################################################################################
all_jumps_large<- all_jumps %>%
  filter(Cluster %in% c("Cluster 9", "Cluster 34"))

large_jumps_plot <- plotJumps(all_jumps_large, "deeppink4")
gg <- large_jumps_plot + 
  facet_wrap(~ Cluster, nrow = 1) + # Facet by Cluster
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  labs(x="Sink", y="Source", title="Average Markov Jump Counts, Large Clusters", size=8) +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 5),
        axis.text.x = element_text(angle = 90, hjust = 0.5), 
        panel.border = element_blank(),
        plot.title = element_text(size = 10, hjust = 0),
        strip.text = element_text(hjust = 0),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), # Reduce plot margins
        legend.title = element_text(size = 6),
        legend.title.align = 1,
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"))
gg
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/large_jumps_plot.png", dpi = 300, height =3, width = 4.5)

totalJumpsTime <- function(c) {
  # Define the input file path
  input_dir <- paste("/scratch/gev25289/workdir/georgia/delta/clusters/individual_dta/rep0/c", c, "/jumptimes.txt", sep = "")
  
  # Attempt to read the file and handle errors
  jumps <- tryCatch(
    read.table(input_dir, header = TRUE),
    error = function(e) {
      message(sprintf("Error reading file for cluster %s: %s", c, e))
      return(NULL)  # Return NULL if an error occurs
    }
  )
  
  # If reading the file failed, return NULL
  if (is.null(jumps)) return(NULL)
  jumps <- read.table(input_dir, header = TRUE)
  
  state <- n_distinct(jumps$state) #Number of total state counts
  jumps$from_to = paste(jumps$from,jumps$to, sep=".")
  jumps <- jumps %>% mutate(time = max(cluster_tips[[c]]$`numeric date`) - as.numeric(time))
  jumps$year <- format(date_decimal(jumps$time), "%Y-%m")
  
  #From, To
  count <- jumps %>% group_by(from_to, year) %>% count()
  count2 <- cbind(count, read.table(text = as.character(count$from_to), sep = ".")) #Forgot to do "From" and "To", so it's V1, V2
  count2 <- count2 %>% mutate(ave=n/state) %>% mutate(Cluster = c)

  return(count2)
}

# Apply function to all clusters and combine the results
cluster_index_subset <- c(9, 34)
all_jumps_time <- bind_rows(lapply(cluster_index_subset, totalJumpsTime))
saveRDS(all_jumps_time, "/scratch/gev25289/workdir/georgia/delta/all_jumps_time.rds")

all_jumps_time$Cluster <- paste("Cluster", all_jumps_time$Cluster, sep = " ")
all_jumps_time$Cluster <- factor(all_jumps_time$Cluster, levels = ClusterLevels24)   
all_jumps_time$From <- gsub("_", " ", all_jumps_time$V1)
all_jumps_time$To <- gsub("_", " ", all_jumps_time$V2)
all_jumps_time$From <- factor(all_jumps_time$From, levels = ph_levels)
all_jumps_time$To <- factor(all_jumps_time$To, levels = ph_levels)

# Function to Plot Heatmaps
##############################################################################################################################################
plotJumpsTime <- function(data, c) {
  p <- ggplot(data, aes(x = year, y = From, fill = ave)) +
    scale_fill_gradient2(low = 'thistle', mid = 'papayawhip', high = 'purple4', 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "Average Markov\nJump Counts")) +
    labs(x="Month", y="Source", title = paste("Average Markov Jump Counts over Time: Cluster", c, sep = " ")) 
  return(p)
}

all_jumpsTime_9 <- all_jumps_time %>%
  filter(Cluster %in% "Cluster 9")

all_jumpsTime_34 <- all_jumps_time %>%
  filter(Cluster %in% "Cluster 34")

jumpsTime9_plot <- plotJumpsTime(all_jumpsTime_9, 9)
a <- jumpsTime9_plot +
  facet_wrap(~To, ncol = 5) + #for all, 7 columns
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + 
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 7), #for all, size 5 font
        axis.text.x = element_text(angle = 90, hjust = 0.5),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        panel.margin.x = unit(0.5, "cm"),
        panel.margin.y = unit(0.5, "cm"),
        legend.title = element_text(size = 6),
        legend.title.align = 1,
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"))

jumpsTime34_plot <- plotJumpsTime(all_jumpsTime_34, 34)
b <- jumpsTime34_plot +
  facet_wrap(~To, ncol = 5) +
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + 
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, hjust = 0.5),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        panel.margin.x = unit(0.5, "cm"),
        panel.margin.y = unit(0.5, "cm"),
        legend.title = element_text(size = 6),
        legend.title.align = 1,
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"))


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

# To and From Plots over Time
###############################################################################################
plotJumpsTime <- function(data, c, lowcolor, midcolor, highcolor) {
  p <- ggplot(data, aes(x = year, y = From, fill = ave)) +
    scale_fill_gradient2(low = lowcolor, mid = midcolor, high = highcolor, 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "Average Markov\nJump Counts/Year")) +
    labs(x="Month", y="Source", title = paste("Average Markov Jump Counts over Time: Cluster", c, sep = " ")) 
  return(p)
}

jumpsTime34_plot <- plotJumpsTime(all_jumps_time, 34, 'darkseagreen1','papayawhip', 'seagreen3')
jumpsTime34_plot <- jumpsTime34_plot +
  facet_wrap(~To, ncol = 5) + #for all, 7 columns
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  theme_custom

jumpsTime9_plot <- plotJumpsTime(all_jumps_time, 9, 'lightblue1','papayawhip', 'deepskyblue3')
jumpsTime9_plot <- jumpsTime9_plot +
  facet_wrap(~To, ncol = 5) + #for all, 7 columns
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  theme_custom

# Extract the legend from the full dataset (all_jumps_time) 
legend_34 <- get_legend(jumpsTime34_plot)
legend_9 <- get_legend(jumpsTime9_plot)

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

# Define a function to generate the plots based on a filter value
generate_plots <- function(data, filter_value, lowcolor, midcolor, highcolor) {
  
  # Apply the filter to the data
  data_from <- data %>% filter(From == filter_value)
  data_to <- data %>% filter(To == filter_value)
  
  # FROM PH District X Plot
  fromPlot <- ggplot(data_from, aes(x = year, y = To, fill = ave)) +
    scale_fill_gradient2(low = lowcolor, mid = midcolor, high = highcolor, 
                         midpoint = mean(data$ave, na.rm = TRUE), 
                         limits = c(0, max(data$ave, na.rm = TRUE))) +
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
  toPlot <- ggplot(data_to, aes(x = year, y = From, fill = ave)) +
    scale_fill_gradient2(low = lowcolor, mid = midcolor, high = highcolor, 
                         midpoint = mean(data$ave, na.rm = TRUE), 
                         limits = c(0, max(data$ave, na.rm = TRUE))) +
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
Northwest <- generate_plots(all_jumpsTime_34, "Northwest", 'darkseagreen1','papayawhip', 'seagreen3')
NorthGeorgia <- generate_plots(all_jumpsTime_34, "North Georgia", 'darkseagreen1','papayawhip', 'seagreen3')
North <- generate_plots(all_jumpsTime_34, "North", 'darkseagreen1','papayawhip', 'seagreen3')
CobbDouglas <- generate_plots(all_jumpsTime_34, "Cobb-Douglas", 'darkseagreen1','papayawhip', 'seagreen3')
Fulton <- generate_plots(all_jumpsTime_34, "Fulton", 'darkseagreen1','papayawhip', 'seagreen3')
Clayton <- generate_plots(all_jumpsTime_34, "Clayton", 'darkseagreen1','papayawhip', 'seagreen3')
DeKalb <- generate_plots(all_jumpsTime_34, "DeKalb", 'darkseagreen1','papayawhip', 'seagreen3')
GNR <- generate_plots(all_jumpsTime_34, "GNR", 'darkseagreen1','papayawhip', 'seagreen3')
District4 <- generate_plots(all_jumpsTime_34, "District 4", 'darkseagreen1','papayawhip', 'seagreen3')
Northeast <- generate_plots(all_jumpsTime_34, "Northeast", 'darkseagreen1','papayawhip', 'seagreen3')
WestCentral <- generate_plots(all_jumpsTime_34, "West Central", 'darkseagreen1','papayawhip', 'seagreen3')
NorthCentral <- generate_plots(all_jumpsTime_34, "North Central", 'darkseagreen1','papayawhip', 'seagreen3')
EastCentral <- generate_plots(all_jumpsTime_34, "East Central", 'darkseagreen1','papayawhip', 'seagreen3')
SouthCentral <- generate_plots(all_jumpsTime_34, "South Central", 'darkseagreen1','papayawhip', 'seagreen3')
Southwest <- generate_plots(all_jumpsTime_34, "Southwest", 'darkseagreen1','papayawhip', 'seagreen3')
South <- generate_plots(all_jumpsTime_34, "South", 'darkseagreen1','papayawhip', 'seagreen3')
Southeast <- generate_plots(all_jumpsTime_34, "Southeast", 'darkseagreen1','papayawhip', 'seagreen3')
Coastal <- generate_plots(all_jumpsTime_34, "Coastal", 'darkseagreen1','papayawhip', 'seagreen3')
OOS <- generate_plots(all_jumpsTime_34, "OOS", 'darkseagreen1','papayawhip', 'seagreen3') 

all34 <- plot_grid(Northwest, NorthGeorgia, North, CobbDouglas, Fulton, Clayton, DeKalb, GNR, District4, Northeast,
                   WestCentral, NorthCentral, EastCentral, SouthCentral, Southwest, South, Southeast, Coastal, OOS)
plot_grid(all34, legend_34,  ncol = 1, align = "v", rel_heights = c(1, 0.05))
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/jumpsTimesAll_34_plot.pdf", height = 20, width = 20)

# Call the function with public health district
Northwest <- generate_plots(all_jumpsTime_9, "Northwest", 'lightblue1','papayawhip', 'deepskyblue3')
NorthGeorgia <- generate_plots(all_jumpsTime_9, "North Georgia", 'lightblue1','papayawhip', 'deepskyblue3')
North <- generate_plots(all_jumpsTime_9, "North", 'lightblue1','papayawhip', 'deepskyblue3')
CobbDouglas <- generate_plots(all_jumpsTime_9, "Cobb-Douglas", 'lightblue1','papayawhip', 'deepskyblue3')
Fulton <- generate_plots(all_jumpsTime_9, "Fulton", 'lightblue1','papayawhip', 'deepskyblue3')
Clayton <- generate_plots(all_jumpsTime_9, "Clayton", 'lightblue1','papayawhip', 'deepskyblue3')
DeKalb <- generate_plots(all_jumpsTime_9, "DeKalb", 'lightblue1','papayawhip', 'deepskyblue3')
GNR <- generate_plots(all_jumpsTime_9, "GNR", 'lightblue1','papayawhip', 'deepskyblue3')
District4 <- generate_plots(all_jumpsTime_9, "District 4", 'lightblue1','papayawhip', 'deepskyblue3')
Northeast <- generate_plots(all_jumpsTime_9, "Northeast", 'lightblue1','papayawhip', 'deepskyblue3')
WestCentral <- generate_plots(all_jumpsTime_9, "West Central", 'lightblue1','papayawhip', 'deepskyblue3')
NorthCentral <- generate_plots(all_jumpsTime_9, "North Central", 'lightblue1','papayawhip', 'deepskyblue3')
EastCentral <- generate_plots(all_jumpsTime_9, "East Central", 'lightblue1','papayawhip', 'deepskyblue3')
SouthCentral <- generate_plots(all_jumpsTime_9, "South Central", 'lightblue1','papayawhip', 'deepskyblue3')
Southwest <- generate_plots(all_jumpsTime_9, "Southwest", 'lightblue1','papayawhip', 'deepskyblue3')
South <- generate_plots(all_jumpsTime_9, "South", 'lightblue1','papayawhip', 'deepskyblue3')
Southeast <- generate_plots(all_jumpsTime_9, "Southeast", 'lightblue1','papayawhip', 'deepskyblue3')
Coastal <- generate_plots(all_jumpsTime_9, "Coastal", 'lightblue1','papayawhip', 'deepskyblue3')
OOS <- generate_plots(all_jumpsTime_9, "OOS", 'lightblue1','papayawhip', 'deepskyblue3') 

all9 <- plot_grid(Northwest, NorthGeorgia, North, CobbDouglas, Fulton, Clayton, DeKalb, GNR, District4, Northeast,
                   WestCentral, NorthCentral, EastCentral, SouthCentral, Southwest, South, Southeast, Coastal, OOS)
plot_grid(all9, legend_9,  ncol = 1, align = "v", rel_heights = c(1, 0.05))
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/jumpsTimesAll_9_plot.pdf", height = 20, width = 20)

# main34 <- plot_grid(District4, Fulton)
# main34_with_legend <- plot_grid(main34, legend,  ncol = 1, align = "v", rel_heights = c(1, 0.05))
# main34
# ggsave("/scratch/gev25289/workdir/georgia/delta/figures/jumpsTimeMain_34_plot.pdf", height = 4.5, width = 7)
# 
# main34_with_title <- ggdraw() +
#   draw_label("Cluster 34", fontface = 'bold', size = 10, hjust = 0.5, vjust = -20) +
#   draw_plot(main34_with_legend, y=-0.02)
# main34_with_title
# 
# plot_grid(main9_with_title, main34_with_title, ncol = 1, align = "v")
# ggsave("/scratch/gev25289/workdir/georgia/delta/figures/main9_main34_jumpTimes.pdf", height = 8.5, width = 9)






