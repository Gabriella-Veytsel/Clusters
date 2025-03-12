#First detection
####################################################################################################
first_detection <- descendantsdf_dates %>%
  group_by(Node) %>%
  slice_min(`numeric date`, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(first_detection = `numeric date`)

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

#Some (or all?) of the singletons are captured in the clusters
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
first_detection_singletons_partofcluster <- first_detection_singletons %>%
  semi_join(id_counts, by = "Descendant") %>% 
  mutate(type = "Cluster") #87

first_detection <- bind_rows(first_detection_clusters, first_detection_singletons_unique, first_detection_singletons_partofcluster) 

#Thinking through weird scenarios
#Scenario where date_dff = 0 days
#There's cases where the branch length is like 0.00000001 from the introduction node, like node 9795, so the date diff is 0
test <- tidytree::tree_subset(new.tree, node = 37, levels_back = 1)
ggtree(test) + geom_tiplab() + xlim(0,1) 

tips(new.tree@phylo, 37)
tips(new.tree@phylo, 9795)

test <- tidytree::tree_subset(new.tree, node = 11408, levels_back = 1)
ggtree(test) + geom_tiplab() + xlim(0,1) 
tips(new.tree@phylo, 11408) #698 tips. Event though some have a branch length of 0.00001, it's not from the introduction node, that's why the date_dff is not 0. 

#Scenario where there are two distinct introduction events in one cluster
tips(new.tree@phylo, 963)
tips(new.tree@phylo, 10093)

test <- tidytree::tree_subset(new.tree, node = 10093, levels_back = 1)
ggtree(test) + geom_tiplab() + xlim(0,1)

test <- tidytree::tree_subset(new.tree, node = 963, levels_back = 1)
test %>% 
  ggtree(aes(color = division)) + 
  geom_tippoint(aes(color = division)) +
  #geom_nodelab() +
  geom_nodepoint() +
  geom_tiplab() + 
  theme_tree2() + 
  xlim(0,2)
ggsave("/scratch/gev25289/workdir/georgia/delta/check.pdf", width=8.5, height=15)

#Scenario where there is a singleton that is not captured in any cluster
tips(new.tree@phylo, 8724)

test <- tidytree::tree_subset(new.tree, node = 8724, levels_back = 1)
test %>% 
  ggtree(aes(color = division)) + 
  geom_tippoint(aes(color = division)) +
  #geom_nodelab() +
  geom_nodepoint() +
  geom_tiplab() + 
  theme_tree2() + 
  xlim(0,10)

# Overlaid histograms
ggplot(first_detection, aes(x=date_dff, fill=type)) +
  geom_histogram(color="black", alpha=0.5) + #position = identity overlaps the bars, instead of stacking
  labs(title="Coalescent Times of 344 Introduction Events into Georgia", x="Coalescent Time (Days)", y = "Count", fill = NULL) + #remove legend title with fill/color = NULL
  theme_classic()

ggsave("/scratch/gev25289/workdir/georgia/delta/coalescenttimes_firstdetection.pdf", width=6, height=3)
first_detection %>% filter(type == "Singleton") %>% select(date_dff) %>% summary() #mean = 222.5 days, range 4-494 days
first_detection %>% filter(type == "Cluster") %>% select(date_dff) %>% summary() #mean = 34.07 days, range 0-147 days