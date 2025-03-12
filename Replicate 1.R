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

ClusterLevels28 <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", 
                     "Cluster 10", "Cluster 11", "Cluster 12", "Cluster 13", "Cluster 14", "Cluster 15", "Cluster 16", "Cluster 17", 
                     "Cluster 18", "Cluster 19", "Cluster 20", "Cluster 21", "Cluster 22", "Cluster 23", "Cluster 24", "Cluster 25", 
                     "Cluster 26", "Cluster 27", "Cluster 28")
# Replicate 1

# Metadata
metadata <- read_tsv(gzfile("/scratch/gev25289/workdir/georgia/delta/ncov/rep1/results/georgia/metadata_adjusted.tsv.xz"))
metadata <- metadata %>% mutate(strain = gsub("/", "_", strain))
metadata %>% group_by(region) %>% summarize(n=n()) #6 continents
metadata %>% group_by(country) %>% summarize(n=n()) #123 countries
metadata %>% group_by(division) %>% summarize(n=n()) #Georgia, n= 4968; OOS, n = 4846

# Import annotated trees - have to use read.beast to retain location and date annotations
timetree_noci <- read.beast("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/2025-01-02_treetime/timetree.nexus") #has no CI info
timetree_noci #Phylogenetic tree with 9814 tips and 3162 internal nodes (date annotations, no location)

# Convert time tree nexus to newick to retain branch support labels in mugration analysis
timetree_noci_tibble <- timetree_noci %>% as_tibble()
#timetree_noci_phylo <- timetree_noci_tibble %>% as.phylo()
#write.tree(timetree_noci_phylo, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/2025-01-02_treetime/timetree.nwk")

# Skip first row: "#Lower and upper bound delineate the 90% max posterior region"
dates <- read_tsv("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/2025-01-02_treetime/dates.tsv", skip = 1) %>%
  rename(date_chr = date, label="#node") #Confidence Intervals

# Read mugration tree
new.tree <- read.beast("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/2025-01-02_mugration/annotated_tree.nexus")
new.tree #9814 tips and 3162 internal nodes

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

# 2042 supported nodes (65%) - aBayes criteria makes no difference, same results without it
Nnode(new.tree) #3162 nodes
supportTibble <- phydata %>% filter(`SH-aLRT` >= 80 & aBayes >= .95 & UFBoot >= 95) 
supportedNodes <- supportTibble %>% dplyr::select(node) %>% as.matrix() %>% as.numeric() #vector of supported nodes
p <- ggtree(new.tree, mrsd=max(phylodata$date_chr)) + geom_treescale(x=2019.5, y = 1, offset=2, width=0.4) + theme_tree2() #Visualize the tree

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

imports <- phylodata_j %>% filter(event == "import") #there are 289 import events, including singletons
imports_nodes <- imports %>% filter(!is.na(UFBoot)) #Remove tips from this dataset, to collect only nodes 

georgia <- phylodata_j %>% filter(division.x=="Georgia") %>% filter(is.na(UFBoot)) %>% filter(!grepl("NODE", label)) %>% pull(node) #georgia tips
getMRCA(new.tree@phylo, georgia) #mrca of all georgia sequences in tree = 9815
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
descendantsdf_dates <- descendantsdf %>% left_join(tipdates, by = c("Descendant" = "label"))

# Clusters should have >= 3 GA sequences
nodelist3 <- nodelist %>% 
  filter(No.Georgia >= 3) #57

# Clusters should not overlap in sequences
nodelist_supported <- nodelist3 %>% filter(Node %in% supportedNodes) #28

# Leke's monophyletic function
vec_supported <- nodelist_supported %>% pull(Node) %>% unique() #Vector of nodes, 28

newNodes_supported <- selectMonophyleticGroup(new.tree@phylo, vec_supported) #28
setdiff(vec_supported, newNodes_supported) #what is the difference? 0

# Comparing clusters before and after adding branch support: they're smaller on average, but ratio of GA: non-GA same
summary(nodelist3$No.Total) #the cluster size ranges from 3 to 1156 seqs, median 11 seqs
summary(nodelist_supported$No.Total) #the cluster size ranges from 3 to 439 seqs, median 10 seqs

summary(nodelist3$meanPer) #ga seqs range from 37.5-100% of the total cluster size, median 86%
summary(nodelist_supported$meanPer) #ga seqs range from 37.5-100% of the total cluster size, median 94%

#closer look at what didn't make the support cut-off
# nodelist_supported <- nodelist_supported %>% 
#   left_join(phylodata_j, by = c("Node" = "node")) 

nodelist_supported <- nodelist_supported %>% 
  left_join(phylodata_j, by = c("Node" = "node"))  

nodelist_supported <- nodelist_supported %>% 
  cbind(ClusterLevels28) %>% 
  mutate(lowerCI = decimal2Date(`lower bound`), 
         upperCI = decimal2Date(`upper bound`)) 

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

# Generate cluster details for all 28 clusters (tips and nodes)
clusters <- lapply(1:28, clusterTips)
saveRDS(clusters, "/scratch/gev25289/workdir/georgia/delta/rds/rep1_cluster_tips.rds")

# Combine all clusters into a single data frame
combined_cluster_tips <- bind_rows(clusters)

# Access individual clusters if needed
# c1 <- clusters[[1]]
# c2 <- clusters[[2]]
# c3 <- clusters[[17]]

# Clusters with at least 5 sequences and heterogenous on ph district (for beast runs)
# clusters[[i]] %>% group_by(location) %>% summarize(n=n()) %>% nrow() #manual test

df5 <- c()
for (i in 1:28) {
  # At least 5 sequences
  seqs <- clusters[[i]] %>% summarize(n=n())
  # And more than 1 ph district
  districts <- clusters[[i]] %>% group_by(location) %>% summarize(n=n()) %>% nrow()
  if (seqs >= 5 & districts > 1) {
    df5 <- c(df5, i)  # Append the cluster number to df5
  }
}

# Figure: Highlighted clusters on phylogeny
################################################################################################################
options(ignore.negative.edge=TRUE)
p <- ggtree(new.tree, aes(color = division), mrsd=max(phylodata_j$date_chr)) +
  geom_tippoint(aes(color = division)) +
  geom_treescale(x=2019.5, y = 1, offset=2, width=0.4) + theme_tree2() +
  scale_color_manual(values = c("midnightblue", "seashell3")) 

p289 <- p + geom_hilight(data=imports, mapping=aes(node=node),
                         alpha=0.2, fill="darkred", color="black",
                         size=0.4, extend = 0.1) 
p57 <- p + geom_hilight(data=nodelist3, mapping=aes(node=Node),
                        alpha=0.2, fill="yellow", color="black",
                        size=0.4, extend = 0.1) 
p28 <- p + geom_hilight(data=nodelist_supported, mapping=aes(node=Node),
                        alpha=0.2, fill="palegreen3", color="black",
                        size=0.4, extend = 0.1) 

plot_grid(p289, p57, p28, ncol=3, labels = c("A", "B", "C"))
# ggsave("/scratch/gev25289/workdir/georgia/delta/tree_highlights.pdf", width=15, height=7)

# Figure: Number of Sequences over Time (Bubble Plot)
#####################################################
# Step 1: Aggregate data to calculate the number of rows per date and cluster
aggregated_clusters <- combined_cluster_tips %>%
  group_by(date_chr, Cluster) %>%
  summarize(n = n(), .groups = "drop")  # Calculate count of rows (n)

# Step 2: Create the bubble plot
bubbleplot <- ggplot(aggregated_clusters, aes(
  x = as.Date(date_chr), 
  y = factor(Cluster, levels = ClusterLevels28), size = n)) +
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
tmrca_plot <- ggplot(nodelist_supported, aes(date_chr, y = factor(ClusterLevels28, levels = ClusterLevels28))) + 
  geom_point(color="black",fill="palegreen3", shape=21, size=3) + 
  geom_errorbar(aes(xmin = lowerCI, xmax = upperCI), color="black") +
  ylab("") + xlab("tMRCA") + ggtitle("") + 
  theme_classic()

#Combined Figure: Tree, Bubble Plot, TMRCA Plot
################################################################################################################
a <- plot_grid(bubbleplot, tmrca_plot, nrow = 2,  labels = c("B", "C"))
plot_grid(p28, a, labels = "A")
# ggsave("/scratch/gev25289/workdir/georgia/delta/Figure1.pdf", width = 15, height = 10)

#Each cluster needs its own fasta file for beast
################################################
metadata$strain <- gsub("/", "_", metadata $strain)
metadata$division[metadata$division == "Hubei"] <- "OOS"
metadata$location[metadata$division == "Hubei"] <- "OOS"

fasta <- phylotools::read.fasta("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/masked_filtered_rep1.fasta") 

fasta <- fasta %>% 
  left_join(metadata, by = c("seq.name" = "strain")) %>% 
  select(c(seq.name, seq.text, date, division, location, epi_week, epi_year))

fasta <- fasta %>%
  mutate(epi_week = epiweek(date)) %>%
  mutate(epi_year = epiyear(date))

fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("_", "/", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("USA/", "USA_", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("North/Georgia", "North_Georgia", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("District/4", "District_4", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("East/Central", "East_Central", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("North/Central", "North_Central", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("South/Central", "South_Central", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("West/Central", "West_Central", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location != "OOS", gsub("/2021/", "_2021/", fasta$seq.name), fasta$seq.name)
fasta$seq.name <- ifelse(fasta$location == "OOS", paste(fasta$seq.name, fasta$date, fasta$epi_week, fasta$location, sep = "/"), fasta$seq.name)

# Initialize an empty list to store the results
fasta_list <- list()
for (i in df5) {
  cluster_name <- paste0("C", i)
  fasta_cluster <- fasta %>% 
    filter(fasta$seq.name %in% clusters[[i]]$strain) %>% 
    mutate(Cluster = cluster_name)
  fasta_list[[cluster_name]] <- fasta_cluster
}

# Combine all filtered data frames into one
combined_fasta <- bind_rows(fasta_list)
combined_fasta <- combined_fasta %>% mutate(seq.name = paste(seq.name, Cluster, sep = "/"))
phylotools::dat2fasta(combined_fasta, outfile = "/scratch/gev25289/workdir/georgia/delta/clusters/rep1/all_clusters_fasta.fasta") #don't need to keep updating this

# Fasta file of each cluster
output_dir <- "/scratch/gev25289/workdir/georgia/delta/clusters/rep1/"

for (cluster_name in names(fasta_list)) {
  # Construct the output file path
  outfile <- file.path(output_dir, paste0(cluster_name, "_fasta.fasta"))
  
  # Save the data frame as a FASTA file
  phylotools::dat2fasta(fasta_list[[cluster_name]], outfile = outfile)
}

