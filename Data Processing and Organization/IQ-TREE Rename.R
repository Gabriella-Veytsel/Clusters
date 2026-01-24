`%!in%` = Negate(`%in%`)

# IQ-TREE names node labels based on branch support values
# These names are not unique, which causes downstream issues
# Rename node labels to format node#_oldnodelabel

process_replicate <- function(rep) {
  # Construct input and output file paths dynamically
  input_path <- glue::glue("/scratch/gev25289/workdir/georgia/delta/iqtree/{rep}/masked_filtered_{rep}.fasta.treefile")
  output_path <- glue::glue("/scratch/gev25289/workdir/georgia/delta/iqtree/{rep}/masked_filtered_{rep}_relabel.fasta.treefile")
  
  # Read the tree and convert to tibble
  iqtree_tree <- read.tree(input_path)
  iqtree_tibble <- iqtree_tree %>% as.treedata() %>% as_tibble()
  
  # Rename node labels
  tip_labels <- iqtree_tree$tip.label  # Get tip labels
  iqtree_tibble_relabel <- iqtree_tibble %>%
    mutate(label = ifelse(label %in% iqtree_tree$tip.label,
                          label,  # Keep tip labels unchanged
                          paste(node, "_", label, sep = "")))  # Modify node labels
  
  iqtree_tree_relabel <- iqtree_tibble_relabel %>% as.phylo()
  
  # Write the relabeled tree to the output file
  write.tree(iqtree_tree_relabel, file = output_path)
}

# Apply the function for multiple replicates
reps <- c("rep1", "rep2", "rep3", "rep4")
lapply(reps, process_replicate)

