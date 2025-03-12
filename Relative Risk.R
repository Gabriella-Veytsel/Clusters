library(bayestestR)
library(tidyverse)
library(cowplot)

#Do I need this?
clusters <- c(1, 2, 3, 4, 5, 6, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 23, 24, 28, 29, 30, 32, 33, 34)
ClusterLevels24 <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6",  "Cluster 9", "Cluster 11", "Cluster 12", "Cluster 13","Cluster 14", "Cluster 15", "Cluster 16", "Cluster 17", "Cluster 18", "Cluster 20", "Cluster 23", "Cluster 24", "Cluster 28", "Cluster 29", "Cluster 30", "Cluster 32", "Cluster 33", "Cluster 34")

#Source - Sink Ratio
# all_jumps_from <- all_jumps %>% group_by(Cluster, From) %>% summarize(source = sum(n))
# all_jumps_to <- all_jumps %>% group_by(Cluster, To) %>% summarize(sink = sum(n))
# all_jumps_source_sink_ratio <- all_jumps_from %>% left_join(all_jumps_to, by = c("From" = "To", "Cluster"))
# all_jumps_source_sink_ratio <- all_jumps_source_sink_ratio %>% 
#   mutate(SourceSinkRatio = round(source/sink, digits = 2)) %>% 
#   rename("PH District" = "From") 
# all_jumps_source_sink_ratio$SourceSinkStatus <- ifelse(all_jumps_source_sink_ratio$SourceSinkRatio > 1, "Source", "Sink")

#Contigency table
#Cell A: Total # of jumps from X to Y
#Cell B: Total # of jumps from other districts (not X) to Y
#Cell C: Total # of jumps from X into other districts (not Y)
#Cell D: Total # of jumps where X is not the source && Y is not the sink

# Function that will be applied to get all possible combinations
compute_summary <- function(data, from_region, to_region) {
  A <- data %>%
    filter(from == from_region & to == to_region) %>%
    group_by(state) %>%
    summarize(A = n(), .groups = "drop")
  
  B <- data %>%
    filter(from != from_region & to == to_region) %>%
    group_by(state) %>%
    summarize(B = n(), .groups = "drop")
  
  C <- data %>%
    filter(from == from_region & to != to_region) %>%
    group_by(state) %>%
    summarize(C = n(), .groups = "drop")
  
  D <- data %>%
    filter(from != from_region & to != to_region) %>%
    group_by(state) %>%
    summarize(D = n(), .groups = "drop")
  
  # Combine all components into a single dataset
  combined <- A %>%
    full_join(B, by = "state") %>%
    full_join(C, by = "state") %>%
    full_join(D, by = "state") %>%
    
    mutate(RR = (A / (A + B)) / (C / (C + D)))
  
  # Calculate mean RR and HDI
  mean_rr <- mean(combined$RR, na.rm = TRUE)
  hdi_rr <- bayestestR::hdi(combined$RR, ci = 0.95, na.rm = TRUE)
  
  print("HDI output:")
  print(hdi_rr)
  
  RR <- data.frame(mean = mean_rr, hdi = hdi_rr)
  
  details <- list(
    combined = combined,
    summary = data.frame(
      mean = mean_rr,
      lower_hdi = hdi_rr$CI_low,
      upper_hdi = hdi_rr$CI_high
    )
  )
  
  return(RR)
  #return(details)
}

# Example usage:
# test <- compute_summary(c9, "Coastal", "Northwest") #return RR
# a few instances where: ci` is too large or x does not contain enough data points, returning NAs.

compute_all_combinations <- function(data, regions) {
  results <- list()
  
  for (from_value in regions) {
    for (to_value in regions) {
      summary <- compute_summary(data, from_value, to_value)
      summary$from <- from_value
      summary$to <- to_value
      results[[paste(from_value, to_value, sep = "_")]] <- summary
    }
  }
  
  combined_results <- bind_rows(results)
  return(combined_results)
}

# Across all possible combinations of pairs
# Get unique regions
c9 <- read.table("/scratch/gev25289/workdir/georgia/delta/clusters/individual_dta/rep0/c9/jumptimes.txt", header=TRUE)
regions <- unique(c(c9$from, c9$to))
c9_hdi <- compute_all_combinations(c9, regions)
saveRDS(c9_hdi, "/scratch/gev25289/workdir/georgia/delta/rds/c9_rr_hdi_95.rds")

c34 <- read.table("/scratch/gev25289/workdir/georgia/delta/clusters/individual_dta/rep0/c34/jumptimes.txt", header=TRUE)
regions <- unique(c(c34$from, c34$to))
c34_hdi <- compute_all_combinations(c34, regions)
saveRDS(c34_hdi, "/scratch/gev25289/workdir/georgia/delta/rds/c34_rr_hdi_95.rds")

########################################################################################################################
c9_hdi_table <- readRDS("/scratch/gev25289/workdir/georgia/delta/rds/c9_rr_hdi_95.rds")
c9_hdi_table <- c9_hdi_table %>% filter(!is.nan(mean))
c9_hdi_table$from <- gsub("_", " ", c9_hdi_table$from)
c9_hdi_table$to <- gsub("_", " ", c9_hdi_table$to)

c34_hdi_table <- readRDS("/scratch/gev25289/workdir/georgia/delta/rds/c34_rr_hdi_95.rds")
c34_hdi_table <- c34_hdi_table %>% filter(!is.nan(mean))
c34_hdi_table$from <- gsub("_", " ", c34_hdi_table$from)
c34_hdi_table$to <- gsub("_", " ", c34_hdi_table$to)

ph_levels <- c("Northwest", "North Georgia", "North", "Cobb-Douglas", "Fulton", "Clayton", "DeKalb", "GNR", "District 4", "Northeast", "West Central", "North Central", "East Central", "South Central", "Southwest", "South", "Southeast", "Coastal", "OOS")

# Facet Plot
########################################################################################################################
facet_plot_c9 <- ggplot(data=c9_hdi_table, aes(x=from, y=mean, ymin=hdi.CI_low, ymax=hdi.CI_high)) +
  geom_pointrange(aes(color = (hdi.CI_low > 1 | hdi.CI_high < 1))) +
  scale_color_manual(
    values = c(`FALSE` = "black", `TRUE` = "red"),  # Enclosing FALSE and TRUE in backticks
    guide = "none"
  ) +
  geom_hline(yintercept=1, lty=2) +
  coord_flip(ylim = c(0,max(c9_hdi_table$hdi.CI_high))) + #so I can compare CI width across plots
  xlab("Source") +
  ylab("Mean (95% HDI)") +
  theme(aspect.ratio = 1) +
  ggtitle("Relative Risk: Cluster 9") +
  facet_wrap(~ to, scales = "free_y", ncol=4) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
facet_plot_c9
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/c9_RR_plot_facet.pdf", facet_plot_c9, height = 12, width = 12)

facet_plot_c34 <- ggplot(data=c34_hdi_table, aes(x=from, y=mean, ymin=hdi.CI_low, ymax=hdi.CI_high)) +
  geom_pointrange(aes(color = (hdi.CI_low > 1 | hdi.CI_high < 1))) +
  scale_color_manual(
    values = c(`FALSE` = "black", `TRUE` = "red"),  # Enclosing FALSE and TRUE in backticks
    guide = "none"
  ) +
  geom_hline(yintercept=1, lty=2) +
  coord_flip(ylim = c(0,max(c34_hdi_table$hdi.CI_high))) + #so I can compare CI width across plots
  xlab("Source") +
  ylab("Mean (95% HDI)") +
  theme(aspect.ratio = 1) +
  ggtitle("Relative Risk: Cluster 34") +
  facet_wrap(~ to, scales = "free_y", ncol=4) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/c34_RR_plot_facet.pdf", facet_plot_c34, height = 12, width = 12)

# Individual Plots (if needed)
########################################################################################################################
rr_forest_plot <- function(level) {
  filtered_data <- c9_hdi_table %>% filter(to == level)
  plot <- ggplot(data=filtered_data, aes(x=from, y=mean, ymin=hdi.CI_low, ymax=hdi.CI_high)) +
    geom_pointrange(aes(color = (hdi.CI_low > 1 | hdi.CI_high < 1))) +
    scale_color_manual(
      values = c(`FALSE` = "black", `TRUE` = "red"),  # Enclosing FALSE and TRUE in backticks
      guide = "none"
    ) +
    geom_hline(yintercept=1, lty=2) +
    coord_flip(ylim = c(0,330)) + #so I can compare CI width across plots
    ggtitle(paste(level)) + 
    xlab("Source") +
    ylab("Mean (95% HDI)") +
    theme(aspect.ratio = 1) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(plot)
}

# Apply the function to each level in ph_levels
forest_plots <- lapply(ph_levels, rr_forest_plot)

# Access a plot (e.g., "Coastal")
forest_plots[[which(ph_levels == "Coastal")]]

combined_plot <- plot_grid(plotlist = forest_plots, ncol = 3) 
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/RR_plot.pdf", combined_plot, height = 20, width = 20)