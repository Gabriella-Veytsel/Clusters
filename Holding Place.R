# dates <- metadata %>% select(c(strain, date))
# export(dates, file = "/scratch/gev25289/workdir/georgia/delta/hm/dates.tsv", col.names=FALSE)
# states <- metadata %>% select(c(strain, division))
# export(states, file = "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/states.tsv")

#Clade labels look terrible
# for (j in 1:nrow(nodelist_supported1)) {
#   p34 <- p34 + geom_cladelabel(
#     node = nodelist_supported1$Node[j], 
#     label = nodelist_supported1$Cluster[j], 
#     offset.text = -23.5, 
#     fontsize = 3,
#     offset = -30.5, 
#     align = FALSE
#   )
# }
# 
# p34
# ggsave("/scratch/gev25289/workdir/georgia/delta/p34.pdf", width=10, height=7)

# First Detection Thinking through weird scenarios
################################################################################
# #Scenario where date_dff > 400 days
# test <- tidytree::tree_subset(new.tree, node = 12524, levels_back = 1)
# ggtree(test) + geom_tiplab() + xlim(0,1) 
# ggtree(new.tree) + geom_hilight(node=12524, fill="steelblue", alpha=.6, extend =5)
# tips(new.tree@phylo, 12524) 
# 
# #Scenario where date_dff = 0 days
# #There's cases where the branch length is like 0.00000001 from the introduction node, like node 9795, so the date diff is 0
# test <- tidytree::tree_subset(new.tree, node = 37, levels_back = 1)
# ggtree(test) + geom_tiplab() + xlim(0,20) 
# 
# tips(new.tree@phylo, 37)
# tips(new.tree@phylo, 9795)
# 
# test <- tidytree::tree_subset(new.tree, node = 11408, levels_back = 1)
# ggtree(test) + geom_tiplab() + xlim(0,1) 
# tips(new.tree@phylo, 11408) #698 tips. Event though some have a branch length of 0.00001, it's not from the introduction node, that's why the date_dff is not 0. 
# 
# #Scenario where there are two distinct introduction events in one cluster
# tips(new.tree@phylo, 963)
# tips(new.tree@phylo, 10093)
# 
# test <- tidytree::tree_subset(new.tree, node = 10093, levels_back = 1)
# ggtree(test) + geom_tiplab() + xlim(0,1)
# 
# test <- tidytree::tree_subset(new.tree, node = 963, levels_back = 1)
# test %>% 
#   ggtree(aes(color = division)) + 
#   geom_tippoint(aes(color = division)) +
#   #geom_nodelab() +
#   geom_nodepoint() +
#   geom_tiplab() + 
#   theme_tree2() + 
#   xlim(0,2)
# # ggsave("/scratch/gev25289/workdir/georgia/delta/check.pdf", width=8.5, height=15)
# 
# #Scenario where there is a singleton that is not captured in any cluster
# tips(new.tree@phylo, 8724)
# 
# test <- tidytree::tree_subset(new.tree, node = 8724, levels_back = 1)
# test %>% 
#   ggtree(aes(color = division)) + 
#   geom_tippoint(aes(color = division)) +
#   #geom_nodelab() +
#   geom_nodepoint() +
#   geom_tiplab() + 
#   theme_tree2() + 
#   xlim(0,10)

# test <- tidytree::tree_subset(new.tree, node = 8903, levels_back = 1)
# test %>%
#   ggtree(aes(color = division)) +
#   geom_tippoint(aes(color = division)) +
#   #geom_nodelab() +
#   geom_nodepoint() +
#   geom_tiplab() +
#   theme_tree2() +
#   xlim(0,10)
