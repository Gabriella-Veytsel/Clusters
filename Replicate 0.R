# Load Libraries
# If package can only be installed with newer R version, go into CRAN archive and download package version for current R version
# e.g., (install.packages('https://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-60.0.1.tar.gz'))

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("treeio")
# BiocManager::install("ggtree")

lapply(c(
  "conflicted", "rjson","tidyverse","janitor","lubridate","forstringr","rio","ape","treeio","ggtree","geiger","magrittr","stringr",
  "phylobase","scales","ggplot2","cowplot","ggridges","ggthemes", "arsenal"), library, character.only = TRUE)

source("/home/gev25289/work/Distinct Clusters Leke Function.R")

`%!in%` = Negate(`%in%`)
options(scipen = 999)

# Customizations
colorset = c(
"Northwest" = "#990F0F", "North Georgia" = "red3", "North" = "tomato",
  "Cobb-Douglas" = "lightpink1", "Fulton" = "hotpink2", "Clayton" = "deeppink1", 
  "DeKalb" = "deeppink3", "GNR" = "deeppink4",
  "District 4"= "#264CFF", "Northeast" = "#3FA0FF", "West Central" = "#2C85B2", 
  "North Central" = "#51A3CC", "East Central" = "#7EC3E5", "South Central" = "#B2E5FF",
  "Southwest" = "#260F99","South" = "#6551CC", "Southeast" = "#8F7EE5", "Coastal" = "#BFB2FF",
  "OOS" = "dimgray")

ph_levels <- c("Northwest", "North Georgia", "North", "Cobb-Douglas", "Fulton", "Clayton", "DeKalb", "GNR", 
               "District 4", "Northeast", "West Central", "North Central", "East Central", "South Central", "Southwest", "South", "Southeast", "Coastal", "OOS")

ClusterLevels34 <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", 
                     "Cluster 10", "Cluster 11", "Cluster 12", "Cluster 13", "Cluster 14", "Cluster 15", "Cluster 16", "Cluster 17", 
                     "Cluster 18", "Cluster 19", "Cluster 20", "Cluster 21", "Cluster 22", "Cluster 23", "Cluster 24", "Cluster 25", 
                     "Cluster 26", "Cluster 27", "Cluster 28", "Cluster 29", "Cluster 30", "Cluster 31", "Cluster 32", "Cluster 33", 
                     "Cluster 34")

ClusterLevels24 <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6",  "Cluster 9", "Cluster 11", 
                     "Cluster 12", "Cluster 13","Cluster 14", "Cluster 15", "Cluster 16", "Cluster 17", "Cluster 18", "Cluster 20", 
                     "Cluster 23", "Cluster 24", "Cluster 28", "Cluster 29", "Cluster 30", "Cluster 32", "Cluster 33", "Cluster 34")

# Files from treetime 
##########################################################################################################################################

# Metadata
metadata <- read_tsv("/scratch/gev25289/workdir/georgia/delta/ncov/rep0/results/metadata_adjusted.tsv") #9783
metadata %>% group_by(region) %>% summarize(n=n()) #6 continents
metadata %>% group_by(country) %>% summarize(n=n()) #122 countries
metadata %>% group_by(division) %>% summarize(n=n()) #Georgia, n= 4937; OOS, n = 4846

# Import annotated trees - have to use read.beast to retain location and date annotations
timetree_noci <- read.beast("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep0/2024-01-16_treetime/timetree.nexus") #has no CI info
timetree_noci #Phylogenetic tree with 9783 tips and 3094 internal nodes (date annotations, no location)

# Convert time tree nexus to newick to retain branch support labels in mugration analysis
timetree_noci_tibble <- timetree_noci %>% as_tibble()
# timetree_noci_phylo <- timetree_noci_tibble %>% as.phylo()
# write.tree(timetree_noci_phylo, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep0/2024-01-16_treetime/timetree.nwk")

# Skip first row: "#Lower and upper bound delineate the 90% max posterior region"
dates <- read_tsv("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep0/2024-01-16_treetime/dates.tsv", skip = 1) %>%
  rename(date_chr = date, label="#node") #Confidence Intervals

new.tree <- read.beast("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep0/2024-01-16_mugration/annotated_tree.nexus")
new.tree #9783 tips and 3094 internal nodes

# IQ-TREE website - "It is recommended to also perform the SH-aLRT test (Guindon et al., 2010)
# Each branch will then be assigned with SH-aLRT and UFBoot supports
# One would typically start to rely on the clade if its SH-aLRT >= 80% and UFboot >= 95%"

# Adjust short branch lengths so they don't cause issues
any(new.tree@phylo$edge.length == 0) #are there branch lengths that are 0?
new.tree@phylo$edge.length[new.tree@phylo$edge.length==0] = new.tree@phylo$edge.length[new.tree@phylo$edge.length==0] + 1e-8 
timetree_noci@phylo$edge.length[timetree_noci@phylo$edge.length==0] = 
  timetree_noci@phylo$edge.length[timetree_noci@phylo$edge.length==0] + 1e-8 
timetree_noci_tibble <- timetree_noci %>% as_tibble()

# Formatting
#############################################################################################################################################
phylodata <- new.tree %>% as_tibble()
phylodata <- phylodata %>% mutate(col1 = ifelse(grepl("/", phylodata$label), phylodata$label, NA)) 
phylodata <- phylodata %>% separate(col1, sep="_", into = c("oldlabel", "branchsupport"), convert=TRUE) 
phylodata <- phylodata %>% separate(branchsupport, sep="/", into = c("SH-aLRT", "aBayes", "UFBoot"), convert=FALSE) %>% select(-oldlabel)
phylodata <- phylodata %>% left_join(timetree_noci_tibble) 
phylodata <- phylodata %>% left_join(dates)

phydata <- phylodata %>%
  left_join(select(metadata, strain, date, location), by = c("label" = "strain")) %>%
  mutate(`SH-aLRT` = as.numeric(`SH-aLRT`)) %>%
  mutate(UFBoot = as.numeric(UFBoot)) %>%
  mutate(aBayes = as.numeric(aBayes)) %>%
  mutate(epi_week = epiweek(date.y)) %>%
  mutate(epi_year = epiyear(date.y))  %>%
  mutate(week = cut(date.y, breaks = "week", start.on.monday=FALSE)) %>% #start on Sunday, not Monday
  mutate(month = cut(date.y, breaks = "month"))

#Formatting label
phydata$strain <- ifelse(phydata$division != "OOS", gsub("_", "/", phydata$label), phydata$label)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("USA/", "USA_", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("North/Georgia", "North_Georgia", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("District/4", "District_4", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("East/Central", "East_Central", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("North/Central", "North_Central", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("South/Central", "South_Central", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("West/Central", "West_Central", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division != "OOS", gsub("/2021/", "_2021/", phydata$strain), phydata$strain)
phydata$strain <- ifelse(phydata$division == "OOS", paste(phydata$strain, phydata$date.y, phydata$epi_week, phydata$division, sep = "/"), phydata$strain)

# 2033 supported nodes (66%) - aBayes criteria makes no difference, same results without it
Nnode(new.tree) #3094 nodes
supportTibble <- phydata %>% filter(`SH-aLRT` >= 80 & aBayes >= .95 & UFBoot >= 95) 
supportedNodes <- supportTibble %>% dplyr::select(node) %>% as.matrix() %>% as.numeric() #vector of supported nodes
p <- ggtree(new.tree, mrsd="2022-01-25") + geom_treescale(x=2019.5, y = 1, offset=2, width=0.4) + theme_tree2() #Visualize the tree

parent <- phydata %>% select(node, division) %>% distinct()
phylodata_j <- phydata %>% left_join(parent, by = c("parent" = "node")) #division.x is node, division.y is parent

phylodata_j <- phylodata_j %>%
  mutate(
    event = case_when(
      division.y == "OOS" & division.x == "Georgia" ~ "import",
      division.y == "Georgia" & division.x == "OOS" ~ "export",
      division.y == "OOS" & division.x == "OOS" ~ "OOS",
      division.y == "Georgia" & division.x == "Georgia" ~ "within ga",
    )
  )

imports <- phylodata_j %>% filter(event == "import") #there are 344 import events, including singletons
imports_nodes <- imports %>% filter(!is.na(UFBoot)) #Remove tips from this dataset, to collect only nodes 

georgia <- phylodata_j %>% filter(division.x=="Georgia") %>% filter(is.na(UFBoot)) %>% filter(!grepl("NODE", label)) %>% pull(node) #georgia tips
getMRCA(new.tree@phylo, georgia) #mrca of all georgia sequences in tree = 9785
phylodata_j %>% filter(node == parent) #mrca estimated date = 2019-12-26

# Get descendents
######################################################################################################################################################
# Function to get a list of tips for a node
descendents <- function(node) {
  tips(new.tree@phylo, node = node)
}

# Extract unique nodes from your dataset
unique_nodes <- imports %>% pull(node) %>% unique() 

# Get tips for each unique node
listTips <- lapply(unique_nodes, descendents) 

# Create a data frame to store node tips
nodelist <- data.frame(Node = unique_nodes, tipNames = I(listTips)) # Store tips as a list

descendantsdf <- nodelist %>%
  unnest(tipNames) %>%
  rename(Descendant = tipNames) 

# Loop through nodelist
df = data.frame()
c = data.frame()
for(i in 1:nrow(nodelist)){
  No.Georgia = length(grep("USA_GA", nodelist[i,]$tipNames[[1]]))
  No.Total = length(nodelist[i,]$tipNames[[1]])
  df = rbind(df, No.Georgia)
  c = rbind(c, No.Total)
}
colnames(df) <- c("No.Georgia")
colnames(c) <- c("No.Total")
nodelist <- cbind(nodelist, df, c) %>%
  mutate(meanPer = No.Georgia/No.Total)

imports_nodes_short <- imports_nodes %>% select(node, 'numeric date')
tipdates <- phylodata_j %>% select(label, parent, 'numeric date', branch.length)

# Clusters should have >= 3 GA sequences
nodelist3 <- nodelist %>% 
  filter(No.Georgia >= 3) #58

# Clusters should not overlap in sequences
nodelist_supported <- nodelist3 %>% filter(Node %in% supportedNodes) #34

# Leke's monophyletic function
vec_supported <- nodelist_supported %>% pull(Node) %>% unique() #Vector of nodes, 34

newNodes_supported <- selectMonophyleticGroup(new.tree@phylo, vec_supported) #34
setdiff(vec_supported, newNodes_supported) #what is the difference? 0

# Comparing clusters before and after adding branch support: they're smaller on average, but ratio of GA: non-GA same
# Even if I had a more lenient support threshold: of the 9 clusters > 100 seqs, 2 make the strict cut-off, 1 is close (ufboot 93, sh-alrt 84), and the rest range from ufboot 21 to 71
summary(nodelist3$No.Total) #the cluster size ranges from 3 to 1097 seqs, median 13.5 seqs
summary(nodelist_supported$No.Total) #the cluster size ranges from 3 to 863 seqs, median 8 seqs

summary(nodelist3$meanPer) #ga seqs range from 33-100% of the total cluster size, median 87%
summary(nodelist_supported$meanPer) #ga seqs range from 33-100% of the total cluster size, median 87%

# Size of clusters
clustersize <- ggplot(nodelist_supported, aes(x=No.Total)) +
  geom_histogram(position="identity", fill = "palegreen3") +
  labs(y = "Number of Sequences") +
  #ggtitle("Number of Sequences in Each Cluster") +
  theme_classic()

# Number of Sequences (that are included in the tree, not gisaid)
delta <- metadata %>% filter(strain != "Wuhan_Hu-1_2019")
seqstime <- ggplot(delta, aes(x=date)) +
  geom_histogram(position="identity") +
  labs(y = "Number of Sequences") +
  #ggtitle("Number of Delta Sequences Sampled over Time") +
  theme_classic()
   
#closer look at what didn't make the support cut-off
# nodelist_supported <- nodelist_supported %>% 
#   left_join(phylodata_j, by = c("Node" = "node")) 

nodelist_supported <- nodelist_supported %>% 
  left_join(phylodata_j, by = c("Node" = "node"))  

nodelist_supported <- nodelist_supported %>% 
  cbind(ClusterLevels34) %>% 
  mutate(lowerCI = decimal2Date(`lower bound`), 
         upperCI = decimal2Date(`upper bound`)) 

# Is there a relationship between date of introduction and cluster size?
clustersizedate <- nodelist_supported %>% select(No.Georgia, No.Total, date_chr) %>% arrange(date_chr)
clustersizedate_plot <- ggplot(clustersizedate, aes(x=date_chr, y=No.Total)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 

lm_model <- lm(No.Total ~ date_chr, data = clustersizedate)
summary(lm_model)

clusterTips <- function(data) {
  # node <- nodelist_supported[data,]$Node # node number
  cluster <- nodelist_supported[data,]$tipNames %>% # tip names
    unlist() %>%
    as.data.frame() %>%
    rename("label" = ".") %>%
    left_join(phylodata_j) %>%
    mutate(
      location = gsub("_", " ", location),
      Cluster = paste("Cluster", data, sep = " ")  # Add cluster identifier here
    )
  
  return(cluster)
}

# Generate cluster details for all 34 clusters (tips and nodes)
clusters <- lapply(1:34, clusterTips)
#saveRDS(clusters, "/scratch/gev25289/workdir/georgia/delta/rds/cluster_tips.rds")

# Combine all clusters into a single data frame
combined_cluster_tips <- bind_rows(clusters)

# Access individual clusters if needed
# c1 <- clusters[[1]]
# c2 <- clusters[[2]]
# c3 <- clusters[[3]]

# Figure: Highlighted clusters on phylogeny
################################################################################################################
p <- ggtree(new.tree, aes(color = division), mrsd="2022-01-25") +
  geom_tippoint(aes(color = division)) +
  geom_treescale(x=2019.5, y = 1, offset=2, width=0.4) + theme_tree2() +
  scale_color_manual(values = c("midnightblue", "seashell3")) 

p344 <- p + geom_hilight(data=imports, mapping=aes(node=node),
                         alpha=0.2, fill="darkred", color="black",
                         size=0.4, extend = 0.1) 
p58 <- p + geom_hilight(data=nodelist3, mapping=aes(node=Node),
                        alpha=0.2, fill="yellow", color="black",
                        size=0.4, extend = 0.1) 
p34 <- p + geom_hilight(data=nodelist_supported, mapping=aes(node=Node),
                        alpha=0.2, fill="palegreen3", color="black",
                        size=0.4, extend = 0.1) 

plot_grid(p344, p58, p34, ncol=3, labels = c("A", "B", "C"))
# ggsave("/scratch/gev25289/workdir/georgia/delta/figures/tree_highlights.pdf", width=15, height=7)

# Figure: Number of Sequences over Time (Bubble Plot)
#####################################################
# Step 1: Aggregate data to calculate the number of rows per date and cluster
aggregated_clusters <- combined_cluster_tips %>%
  group_by(date_chr, Cluster) %>%
  summarize(n = n(), .groups = "drop")  # Calculate count of rows (n)

# Step 2: Create the bubble plot
bubbleplot <- ggplot(aggregated_clusters, aes(
  x = as.Date(date_chr), 
  y = factor(Cluster, levels = ClusterLevels34), size = n)) +
  geom_point(alpha = 0.7, color = "black", fill = "palegreen3", shape = 21) +
  theme_classic() +
  ylab("") +
  xlab("Date of Collection") +
  labs(size = "No. of Sequences") +
  scale_x_date(
    breaks = seq(
      from = min(as.Date(aggregated_clusters$date_chr)), 
      to = max(as.Date(aggregated_clusters$date_chr)), 
      by = "1 month"
    ),
    date_labels = "%Y-%m"
  )

#Figure: TMRCA plot
################################################################################################################
tmrca_plot <- ggplot(nodelist_supported, aes(date_chr, y = factor(ClusterLevels34, levels = ClusterLevels34))) + 
  geom_point(color="black",fill="palegreen3", shape=21, size=3) + 
  geom_errorbar(aes(xmin = lowerCI, xmax = upperCI), color="black") +
  ylab("") + xlab("tMRCA") + ggtitle("") + 
  theme_classic()

#Combined Figure: Tree, Bubble Plot, TMRCA Plot
################################################################################################################
a <- plot_grid(bubbleplot, tmrca_plot, nrow = 2,  labels = c("B", "C"))
plot_grid(p34, a, labels = "A")
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/Figure1.pdf", width = 15, height = 10)

#Do I want to include the other plots?
b <- plot_grid(seqstime, clustersize, nrow = 2)
d <- plot_grid(tmrca_plot, b, nrow = 1, rel_widths = c(2, 1), rel_heights = c(2, 1))
a <- plot_grid(d, bubbleplot, nrow = 2,  labels = c("B", "C"))
c <- plot_grid(p34, a, labels = "A", rel_widths = c(1,1))

#First detection
################################################################################################################
descendantsdf_dates <- descendantsdf %>% left_join(tipdates, by = c("Descendant" = "label"))

first_detection <- descendantsdf_dates %>%
  group_by(Node) %>%
  slice_min(`numeric date`, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(first_detection = `numeric date`)

#Quick check of the earliest sequence within each cluster
first_detection_34 <- nodelist_supported %>% 
  left_join(first_detection, by = c("Node" = "Node")) #join each descendant with its parent node

#This joins with imports_nodes dataset to prevent singletons from being matched with false parent
first_detection <- first_detection %>% 
  left_join(imports_nodes_short, by = c("Node" = "node")) %>% #join each descendant with its parent node
  rename(date_node = 'numeric date') %>%
  mutate(first_detection_date = as.Date(date_decimal(first_detection))) %>%
  mutate(node_date = as.Date(date_decimal(date_node))) %>%
  mutate(date_dff = time_length(ymd(first_detection_date) - ymd(node_date), "days")) 

first_detection_clusters <- first_detection %>% filter(!is.na(date_node)) %>% mutate(type = "Cluster") #102
first_detection_singletons <- first_detection %>% filter(is.na(date_node)) %>% select(-c(date_node, node_date))

parent_singletons <- phylodata_j %>% 
  select(node, `numeric date`, date_chr) %>% 
  rename(date_node = `numeric date`) %>% 
  rename(node_date = date_chr) %>% distinct()

first_detection_singletons <- first_detection_singletons %>% 
  left_join(parent_singletons, by = c("parent" = "node")) %>% 
  mutate(date_dff = time_length(ymd(first_detection_date) - ymd(node_date), "days")) #These include singletons that are part of a cluster

#Some singletons are captured already in the clusters
#Count occurrences of each ID in dataset2
id_counts <- descendantsdf_dates %>%
  group_by(Descendant) %>%
  summarise(count = n()) %>%
  filter(count > 1) # Keep only IDs that appear more than once

  #Singletons that are not part of a cluster 
  #Remove if any IDs from dataset1 appear more than once in dataset2)
  first_detection_singletons_unique <- first_detection_singletons %>%
    anti_join(id_counts, by = "Descendant") %>% 
    mutate(type = "Singleton") #155
  
  #Singletons that are part of a cluster 
  #This is just to remove these from the singleton dataset, as they likely won't go under the radar
  first_detection_singletons_partofcluster <- first_detection_singletons %>%
    semi_join(id_counts, by = "Descendant") %>% 
    mutate(type = "Cluster") #87

first_detection <- bind_rows(first_detection_clusters, first_detection_singletons_unique, first_detection_singletons_partofcluster) 

# Why are there such long detection times?
metadata %>% 
  filter(strain != "Wuhan_Hu-1_2019") %>%
  summarize(min(date), max(date)) #542 days bw 2020-08-01 and 2022-01-25

problems <-first_detection %>%
  filter(date_dff > 300)

p + geom_hilight(data=problems, mapping=aes(node=Node),
                 alpha=0.2, fill="darkred", color="darkred",
                 size=0.4, extend = 5) 

first_detection %>% filter(type == "Singleton") %>% select(date_dff) %>% summary() #mean = 222.5 days, range 4-494 days
first_detection %>% filter(type == "Cluster") %>% select(date_dff) %>% summary() #mean = 34.07 days, range 0-147 days

# Overlaid histograms
ggplot(first_detection, aes(x=date_dff, fill=type)) +
  geom_histogram(color="black", alpha=0.5) + #position = identity overlaps the bars, instead of stacking
  labs(title="Coalescent Times of 344 Introduction Events into Georgia", x="Coalescent Time (Days)", y = "Count", fill = NULL) + #remove legend title with fill/color = NULL
  theme_classic() +
  theme(plot.title = element_text(size = 10))
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/coalescenttimes_firstdetection.pdf", width=6, height=3)

firstdetection_edit <- bind_rows(first_detection_clusters, first_detection_singletons_partofcluster) 
ggplot(firstdetection_edit, aes(x=date_dff)) +
  geom_histogram(color="black", alpha=0.5) + #position = identity overlaps the bars, instead of stacking
  labs(x="Coalescent Time (Days)", y = "Count", fill = NULL) + #remove legend title with fill/color = NULL
  theme_classic() + 
  ggtitle(expression("Coalescent Times of 189 Introduction Events into Georgia Resulting in" * phantom() >= phantom() * "2 Descendants")) +
  theme(plot.title = element_text(size = 10))
ggsave("/scratch/gev25289/workdir/georgia/delta/figures/coalescenttimes_firstdetection_2tips.pdf", width=6, height=3)
