library(dplyr)
library(HDInterval)

# List of cluster IDs
clusters <- c(1, 2, 3, 4, 5, 6, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 23, 24, 28, 29, 30, 32, 33, 34)

################################################################################################################
colorset = c(
  "Northwest" = "#990F0F",
  "North Georgia" = "red3",
  "North" = "tomato",
  
  "Cobb-Douglas" = "lightpink1",
  "Fulton" = "hotpink2",
  "Clayton" = "deeppink1",
  "DeKalb" = "deeppink3",
  "GNR" = "deeppink4",
  
  "District 4"= "#264CFF",
  "Northeast" = "#3FA0FF",
  "West Central" = "#2C85B2",
  "North Central" = "#51A3CC",
  "East Central" = "#7EC3E5",
  "South Central" = "#B2E5FF",
  
  "Southwest" = "#260F99",
  "South" = "#6551CC",
  "Southeast" = "#8F7EE5",
  "Coastal" = "#BFB2FF",
  
  "OOS" = "dimgray"
)

ph_levels <- c("Northwest", "North Georgia", "North", "Cobb-Douglas", "Fulton", "Clayton", "DeKalb", "GNR", "District 4", "Northeast", "West Central", "North Central", "East Central", "South Central", "Southwest", "South", "Southeast", "Coastal", "OOS")
ClusterLevels <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10", "Cluster 11", "Cluster 12", "Cluster 13", "Cluster 14", "Cluster 15", "Cluster 16", "Cluster 17", "Cluster 18", "Cluster 19", "Cluster 20", "Cluster 21", "Cluster 22", "Cluster 23", "Cluster 24", "Cluster 25", "Cluster 26", "Cluster 27", "Cluster 28", "Cluster 29", "Cluster 30", "Cluster 31", "Cluster 32", "Cluster 33", "Cluster 34")

#################################################################################################
# Rename maps for specific clusters
# List of clusters to process
clusters <- c(1, 2, 3, 4, 5, 6, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 23, 24, 28, 29, 30, 32, 33, 34)

rename_maps <- list(
  `9` = c(c_Clayton_reward.1. = "c_Clayton.1."),
  `1` = c(c_CobbDouglas_reward.1. = "c_Cobb.Douglas_reward.1."),
  `34` = c(c_Clayton_reward.1. = "c_Clayton.1.")
)

process_log <- function(cluster_id, rename_map = NULL) {
  log_file <- paste0("/scratch/gev25289/workdir/georgia/delta/clusters/individual_dta/rep0/c", cluster_id, "/combined.log.txt")
  log_data <- read.table(log_file, header = TRUE) %>%
    select(starts_with("c_"), "treeLength") 
  
  if (!is.null(rename_map)) {
    log_data <- rename(log_data, !!!rename_map)
  }
  
  return(log_data)
}

# Function to calculate statistics
calc_stats_hpd <- function(log_data, reward_col) {
  # Calculate the mean reward for the specified column
  mean_reward <- mean(log_data[[reward_col]], na.rm = TRUE)
  
  # Calculate the mean tree length
  mean_tree_length <- mean(log_data$treeLength, na.rm = TRUE)
  
  # Calculate the proportion
  proportion <- mean_reward / mean_tree_length * 100
  
  # Return a data frame with all metrics
  return(data.frame(
    MeanReward = mean_reward,
    MeanTreeLength = mean_tree_length,
    Proportion = proportion,
    PHDistrict = reward_col
  ))
}

# Process all logs and calculate statistics
logs <- lapply(clusters, function(cluster_id) {
  rename_map <- rename_maps[[as.character(cluster_id)]]
  log_data <- process_log(cluster_id, rename_map)
  
  # Identify reward columns dynamically (those starting with "c_" but excluding "treeLength" or counts)
  reward_columns <- names(log_data)[grepl("^c_", names(log_data)) & !grepl("all|count", names(log_data))]
  
  # Calculate stats for each reward column
  stats_results <- do.call(rbind, lapply(reward_columns, function(col) {
    calc_stats_hpd(log_data, col)
  }))
  
  # Add cluster information
  stats_results$Cluster <- paste("Cluster", cluster_id)
  return(stats_results)
})

# Combine all results into a single data frame
rewards <- bind_rows(logs)
rewards %>% group_by(Cluster) %>% summarize(sum = sum(Proportion)) %>% print(n=24)

# Rename 
rewards <- rewards %>% 
  select(Cluster, PHDistrict, everything()) %>%
  mutate(PHDistrict = case_when(
    PHDistrict == "c_Coastal_reward.1." ~ "Coastal",
    PHDistrict == "c_CobbDouglas_reward.1." ~ "Cobb-Douglas",
    PHDistrict == "c_District4_reward.1." ~ "District 4",
    PHDistrict == "c_EastCentral_reward.1." ~ "East Central",
    PHDistrict == "c_Fulton_reward.1." ~ "Fulton",
    PHDistrict == "c_GNR_reward.1." ~ "GNR",
    PHDistrict == "c_North_reward.1." ~ "North",
    PHDistrict == "c_NorthCentral_reward.1." ~ "North Central",
    PHDistrict == "c_NorthGeorgia_reward.1." ~ "North Georgia",
    PHDistrict == "c_Northwest_reward.1." ~ "Northwest",
    PHDistrict == "c_OOS_reward.1." ~ "OOS",
    PHDistrict == "c_South_reward.1." ~ "South",
    PHDistrict == "c_Southeast_reward.1." ~ "Southeast",
    PHDistrict == "c_Southwest_reward.1." ~ "Southwest",
    PHDistrict == "c_WestCentral_reward.1." ~ "West Central",
    PHDistrict == "c_Clayton_reward.1." ~ "Clayton",
    PHDistrict == "c_Dekalb_reward.1." ~ "DeKalb",
    PHDistrict == "c_Northeast_reward.1." ~ "Northeast",
    PHDistrict == "c_SouthCentral_reward.1." ~ "South Central"))

rewards$PHDistrict <- factor(rewards$PHDistrict, levels = ph_levels)
rewards$Cluster <- factor(rewards$Cluster, levels = ClusterLevels)

c9 <- rewards %>% filter(Cluster == "Cluster 9")
c34 <- rewards %>% filter(Cluster == "Cluster 34")

ggplot(rewards, aes(x = Cluster, y = Proportion, fill = PHDistrict)) +
  geom_bar(stat = "identity", position = "fill") +  # Normalize to 100%
  scale_y_continuous(labels = scales::percent) +    # Format y-axis as percentages
  scale_fill_manual(values = colorset) +
  labs(
    x = "",
    y = "Proportion of the Mean Time (%)",
    fill = "PH District",
    title = "Mean SARS-CoV-2 Waiting Times per Location"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with adjusted text size
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2, margin = margin(t = -18)),  # Rotate x-axis labels for readability
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = -20)),  # Centered bold plot title with reduced bottom margin
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 12),  # Adjust legend text size
    panel.grid = element_blank(),  # Remove background grid lines
    #axis.ticks = element_line(size = 0.5),  # Add subtle axis ticks for clarity
    #axis.ticks.x = element_line(margin = margin(t = -50),  # Adjust margin to bring ticks closer to the plot
  )
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/reward_plot.pdf", height = 8, width = 14, limitsize=FALSE)

rewards_9_34 <- rewards %>%
  filter(Cluster %in% c("Cluster 9", "Cluster 34"))
ggplot(rewards_9_34, aes(x = Cluster, y = Proportion, fill = PHDistrict)) +
  geom_bar(stat = "identity", position = "fill", width=0.70) +  # Normalize to 100%
  scale_y_continuous(labels = scales::percent) +    # Format y-axis as percentages
  scale_fill_manual(values = colorset) +
  labs(
    x = "",
    y = "Proportion of the Mean Time (%)",
    fill = "PH District",
    title = "Mean SARS-CoV-2 Waiting Times per Location"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with adjusted text size
  theme(
    axis.text.x = element_text(hjust = 0.5, vjust = 0.2, margin = margin(t = -18)),  
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, 
                              margin = margin(t = -3, b = 2)),  # Centered bold plot title with reduced bottom margin
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(face = "bold", size = 10),  # Bold legend title
    legend.text = element_text(size = 8),  # Adjust legend text size
    panel.grid = element_blank(),  # Remove background grid lines
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    #axis.ticks = element_line(size = 0.5),  # Add subtle axis ticks for clarity
    #axis.ticks.x = element_line(margin = margin(t = -50),  # Adjust margin to bring ticks closer to the plot
  )
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/main_reward_plot.pdf", height = 7, width = 8, limitsize=FALSE)
