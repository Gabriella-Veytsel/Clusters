# FASTA
rep0 <- phylotools::read.fasta("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep0/masked_filtered.fasta")
rep1 <- phylotools::read.fasta("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/masked_filtered_rep1.fasta")
rep2 <- phylotools::read.fasta("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep2/masked_filtered_rep2.fasta")
rep3 <- phylotools::read.fasta("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep3/masked_filtered_rep3.fasta")
rep4 <- phylotools::read.fasta("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep4/masked_filtered_rep4.fasta")

# Tree Files
rep1_tree <- read.tree("/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/masked_filtered_rep1_relabel.fasta.treefile")

# Check that the tree names and fasta names are the same
tip_labels <- rep1_tree$tip.label
seqnames <- rep1$seq.name
setdiff(seqnames, tip_labels)

# Metadata
metadata_rep0 <- read_tsv("/scratch/gev25289/workdir/georgia/delta/ncov/rep0/results/metadata_adjusted.tsv") #9783
metadata_rep1 <- read_tsv(gzfile("/scratch/gev25289/workdir/georgia/delta/ncov/rep1/results/georgia/metadata_adjusted.tsv.xz"))
metadata_rep2 <- read_tsv(gzfile("/scratch/gev25289/workdir/georgia/delta/ncov/rep2/results/georgia2/metadata_adjusted.tsv.xz"))
metadata_rep3 <- read_tsv(gzfile("/scratch/gev25289/workdir/georgia/delta/ncov/rep3/results/georgia3/metadata_adjusted.tsv.xz"))
metadata_rep4 <- read_tsv(gzfile("/scratch/gev25289/workdir/georgia/delta/ncov/rep4/results/georgia4/metadata_adjusted.tsv.xz"))

# Join FASTA and Metadata
rep0_metadata <- rep0 %>% left_join(metadata_rep0, by = c("seq.name" = "strain")) 
rep1_metadata <- rep1 %>% left_join(metadata_rep1, by = c("seq.name" = "strain")) %>% mutate(seq.name = gsub("/", "_", seq.name))
rep2_metadata <- rep2 %>% left_join(metadata_rep2, by = c("seq.name" = "strain")) %>% mutate(seq.name = gsub("/", "_", seq.name))
rep3_metadata <- rep3 %>% left_join(metadata_rep3, by = c("seq.name" = "strain")) %>% mutate(seq.name = gsub("/", "_", seq.name))
rep4_metadata <- rep4 %>% left_join(metadata_rep4, by = c("seq.name" = "strain")) %>% mutate(seq.name = gsub("/", "_", seq.name))

rep1 <- rep1 %>% mutate(seq.name = gsub("/", "_", seq.name))
rep2 <- rep2 %>% mutate(seq.name = gsub("/", "_", seq.name))
rep3 <- rep3 %>% mutate(seq.name = gsub("/", "_", seq.name))
rep4 <- rep4 %>% mutate(seq.name = gsub("/", "_", seq.name))

#dat2fasta(rep1, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/masked_filtered_rep1.fasta")
dat2fasta(rep2, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep2/masked_filtered_rep2.fasta")
dat2fasta(rep3, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep3/masked_filtered_rep3.fasta")
dat2fasta(rep4, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep4/masked_filtered_rep4.fasta")

#Treetime states.tsv
rep0_states <- rep0_metadata %>% select(c(seq.name, division)) %>% rename(strain = seq.name) 
rep1_states <- rep1_metadata %>% select(c(seq.name, division)) %>% rename(strain = seq.name)
rep2_states <- rep2_metadata %>% select(c(seq.name, division)) %>% rename(strain = seq.name)
rep3_states <- rep3_metadata %>% select(c(seq.name, division)) %>% rename(strain = seq.name)
rep4_states <- rep4_metadata %>% select(c(seq.name, division)) %>% rename(strain = seq.name)

rep1_states$division[rep1_states$division == "Hubei"] <- "OOS"
rep2_states$division[rep2_states$division == "Hubei"] <- "OOS"
rep3_states$division[rep3_states$division == "Hubei"] <- "OOS"
rep4_states$division[rep4_states$division == "Hubei"] <- "OOS"

#Export
#write_tsv(rep1_states, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/states.tsv")
write_tsv(rep2_states, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep2/states.tsv")
write_tsv(rep3_states, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep3/states.tsv")
write_tsv(rep4_states, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep4/states.tsv")

#Treetime dates.tsv
rep0_dates <- rep0_metadata %>% select(c(seq.name, date)) %>% rename(strain = seq.name)
rep1_dates <- rep1_metadata %>% select(c(seq.name, date)) %>% rename(strain = seq.name)
rep2_dates <- rep2_metadata %>% select(c(seq.name, date)) %>% rename(strain = seq.name)
rep3_dates <- rep3_metadata %>% select(c(seq.name, date)) %>% rename(strain = seq.name)
rep4_dates <- rep4_metadata %>% select(c(seq.name, date)) %>% rename(strain = seq.name)

#Export 
#write_tsv(rep1_dates, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep1/dates.tsv")
write_tsv(rep2_dates, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep2/dates.tsv")
write_tsv(rep3_dates, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep3/dates.tsv")
write_tsv(rep4_dates, "/scratch/gev25289/workdir/georgia/delta/treetime/treetime/rep4/dates.tsv")


