library(tidyverse)
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
#ggsave("/scratch/gev25289/workdir/georgia/delta/jumps_plot.pdf", height =8, width = 11, limitsize=FALSE)

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
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/small_jumps_plot.pdf", height =4, width = 7, limitsize=FALSE)

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
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/medium_jumps_plot.pdf", height =6, width = 8, limitsize=FALSE)

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
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/large_jumps_plot.pdf", height =3, width = 4.5)

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
all_jumps_time <- bind_rows(lapply(cluster_index, totalJumpsTime))
saveRDS(all_jumps, "/scratch/gev25289/workdir/georgia/delta/all_jumps_time.rds")

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
