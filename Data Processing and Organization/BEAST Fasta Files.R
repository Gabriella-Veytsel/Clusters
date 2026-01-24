#Each cluster needs its own fasta file for beast
###############################################
metadata <- read_tsv(gzfile("/scratch/gev25289/workdir/georgia/delta/ncov/rep1/results/georgia/metadata_adjusted.tsv.xz")) 
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
#phylotools::dat2fasta(combined_fasta, outfile = "/scratch/gev25289/workdir/georgia/delta/clusters/rep1/all_clusters_fasta.fasta") #don't need to keep updating this

#Fasta file of each cluster
output_dir <- "/scratch/gev25289/workdir/georgia/delta/clusters/rep1/"

for (cluster_name in names(fasta_list)) {
  # Construct the output file path
  outfile <- file.path(output_dir, paste0(cluster_name, "_fasta.fasta"))
  
  # Save the data frame as a FASTA file
  phylotools::dat2fasta(fasta_list[[cluster_name]], outfile = outfile)
}
