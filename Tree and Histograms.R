lapply(c(
  "conflicted", "rjson","tidyverse","janitor","lubridate","forstringr","rio","ape","treeio","ggtree","geiger","magrittr","stringr",
  "phylobase","scales","ggplot2","cowplot","ggridges","ggthemes", "arsenal"), library, character.only = TRUE)

source("/home/gev25289/work/Distinct Clusters Leke Function.R")

ph_levels <- c("Northwest", "North Georgia", "North", "Cobb-Douglas", "Fulton", "Clayton", "DeKalb", "GNR", 
               "District 4", "Northeast", "West Central", "North Central", "East Central", "South Central", "Southwest", "South", "Southeast", "Coastal", "OOS")

colorset = c(
  "Northwest" = "#990F0F", "North Georgia" = "red3", "North" = "tomato",
  "Cobb-Douglas" = "lightpink1", "Fulton" = "hotpink2", "Clayton" = "deeppink1", 
  "DeKalb" = "deeppink3", "GNR" = "deeppink4",
  "District 4"= "#264CFF", "Northeast" = "#3FA0FF", "West Central" = "#2C85B2", 
  "North Central" = "#51A3CC", "East Central" = "#7EC3E5", "South Central" = "#B2E5FF",
  "Southwest" = "#260F99","South" = "#6551CC", "Southeast" = "#8F7EE5", "Coastal" = "#BFB2FF",
  "OOS" = "dimgray")

# Figure: Number of Sequences over Time, by Location (Bar Plot)
###############################################################
cluster_ids <- c(1, 2, 3, 4, 5, 6, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 23, 24, 28, 29, 30, 32, 33, 34)

clusters <- readRDS("/scratch/gev25289/workdir/georgia/delta/rds/cluster_tips.rds")
combined_cluster_tips <- bind_rows(clusters)

c_all <- combined_cluster_tips %>%
  group_by(date_chr) %>%
  summarize(n = n()) #you need this for the plot x limits

sequenceHist <- function(data) {
  c <- data %>% 
    group_by(date_chr, location) %>%
    summarize(n=n(), .groups = "drop") 
  
  n_total <- sum(c$n)
  
  p <- ggplot(c, aes(x = date_chr, y = n, fill = location)) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_manual(values = colorset) +
    ylab("No. Sequences") + xlab("Date") +
    scale_x_date(
      date_labels = "%Y-%m", breaks = "1 month",
      limits = c(min(as.Date(c_all$date_chr) - 1), max(as.Date(c_all$date_chr) + 1), expand=c(0,0))
    ) +
    scale_y_continuous(limits = c(0, 25)) +
    theme_bw() +
    ggtitle(paste(data$Cluster, " (n=", n_total, ")", sep = "")) +
    theme(legend.position = "none")
  
  return(p)
}

# Generate plots for all clusters
sequenceHistogram <- lapply(clusters, sequenceHist)

# Access individual plots if needed
# cluster1 <- sequenceHistogram[[1]]
# cluster2 <- sequenceHistogram[[2]]
# cluster3 <- sequenceHistogram[[3]]

bar_plots <- plot_grid(plotlist = sequenceHistogram, ncol = 1, label_size = 7)

# Function to generate tree plot for each cluster
treePlot <- function(cluster, mrsd, colorset, ph_levels) {
  # Define the input file path
  options(ignore.negative.edge=TRUE)
  input_dir <- paste("/scratch/gev25289/workdir/georgia/delta/clusters/rep0/bssvs/combinedtrees/", cluster, ".combined.annot.tree", sep = "")
  tree <- read.beast(input_dir)
  
  # Find the column that ends with "Location"
  location_column <- names(tree@data)[endsWith(names(tree@data), "Location")]
  
  # Create a new variable for color by replacing underscores with spaces
  tree@data <- tree@data %>%
    mutate(LocationColor = gsub("_", " ", sub(".*\\+", "", .data[[location_column]])))
  
  # Convert LocationColor to a factor with levels specified in ph_levels
  tree@data$LocationColor <- factor(tree@data$LocationColor, levels = ph_levels)
  
  # Create the plot using LocationColor for coloring
  p <- ggtree(tree, mrsd = mrsd, aes(color = LocationColor)) + theme_tree2()
  
  p1 <- p +
    geom_tippoint(aes(color = LocationColor), size = 2, alpha = 0.8) +
    xlim(c(2020.8, 2022.1)) +
    theme_bw() +
    scale_color_manual(values = colorset) +
    ggtree::theme(legend.title = element_blank(), legend.key = element_blank(), axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank())
  
  return(p1)
}

# Manual check
# tree1 <- read.beast("/scratch/gev25289/workdir/georgia/delta/clusters/rep0/bssvs/combinedtrees/1.combined.annot.tree")
# ggtree(tree1, mrsd = max(clusters[[1]]$date_chr)) + theme_tree2()

# Use lapply to loop over all cluster IDs and generate the plots
tree_plots <- lapply(cluster_ids, function(cluster) {
  treePlot(cluster, max(clusters[[cluster]]$date_chr), colorset, ph_levels)
})

# Access individual plots if needed
# tree1 <- tree_plots[[1]]
# tree2 <- tree_plots[[2]]
# tree3 <- tree_plots[[3]]

# Pair the sequenceHistograms with treePlots using Map to apply the pairing
treeandhistogram_list <- Map(function(cluster_id) {
  # Generate bar plot for the given cluster ID
  sequence_plot <- sequenceHist(clusters[[cluster_id]])
  
  # Generate tree plot for the given cluster ID
  tree_plot <- treePlot(cluster_id, max(clusters[[cluster_id]]$date_chr), colorset, ph_levels)
  
  # Combine the bar plot and tree plot for the given cluster ID, placing them side by side
  plot_grid(sequence_plot, tree_plot, ncol = 2)  # Bar plot and tree plot in one row
}, cluster_ids)

# Now combine all the individual plot pairs into one large plot (two columns of combined plots)
final_combined_plot <- plot_grid(plotlist = treeandhistogram_list, ncol = 1)  # All paired plots stacked in a single column
final_combined_plot
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/combined_tree_bar_plots.pdf", width = 20, height=115, limitsize=FALSE)
