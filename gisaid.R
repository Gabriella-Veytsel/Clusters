library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(RColorBrewer)
library(zipcodeR)
library(chron)
library(rio)
library(ape)
library(seqinr)
library(phylotools) #read.fasta

source("C:/Users/gev25289/Desktop/xps/georgia/code/functions.R")
`%!in%` = Negate(`%in%`)

metadata_delta_2023 <- read_tsv("C:/Users/gev25289/Desktop/xps/georgia/GISAID/georgia delta complete high_cov collect/combined.tsv") %>%
  filter(strain != "strain") %>% mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  select(-length) %>%
  mutate(date_submitted = as.Date(date_submitted, "%Y-%m-%d")) %>% 
  filter(gisaid_epi_isl %!in% c("EPI_ISL_3640412", "EPI_ISL_3640461")) %>% #When matching to fasta, discovered that 2 isolates (GISAID Name: hCoV-19/USA/GA-GD-081721-21072022899/2021 and hCoV-19/USA/GA-GD-081721-21071922011/2021) each have 2 GISAID Assession IDs (duplicate rows in GISAID)
  distinct() #24,279, downloaded 6/20/2023

metadata <- read_tsv("C:/Users/gev25289/Desktop/xps/georgia old/gisaid/complete delta genomes/gisaid_metadata.combined.tsv") %>%
  filter(strain != "strain") %>% mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  select(-length) %>%
  mutate(date_submitted = as.Date(date_submitted, "%Y-%m-%d")) %>%
  distinct() #66,331, downloaded 12/7/2022

# WHO Name
#########################################################################################
metadata <- metadata %>%
  mutate(
    WHO_name = case_when(
      
      #Other
      
      #Alpha (B.1.1.7 and Q lineages)
      pangolin_lineage == "B.1.1.7" ~ "Alpha", #exact match
      grepl("^Q.", pangolin_lineage) ~ "Alpha", #starts with (avoid issues with BQ)
      
      #Beta (B.1.351 and descendent lineages)
      pangolin_lineage == "B.1.351" ~ "Beta", #exact match
      pangolin_lineage == "B.1.351.2" ~ "Beta", #exact match
      pangolin_lineage == "B.1.351.3" ~ "Beta", #exact match
      
      #Gamma (P.1 and descendent lineages)
      pangolin_lineage == "P.1" ~ "Gamma",
      grepl("^P.1.", pangolin_lineage) ~ "Gamma", #startw with (avoids issues with CP.1)
      
      #Delta (B.1.617.2 and AY lineages), break up Delta
      pangolin_lineage == "B.1.617.2" ~ "Delta:B.1.617.2", #exact match
      grepl("AY", pangolin_lineage) ~ "Delta:AY", 
      
      #Epsilon (B.1.427 and B.1.429)
      pangolin_lineage == "B.1.427" ~ "Epsilon", #exact match
      pangolin_lineage == "B.1.429" ~ "Epsilon", #exact match
      
      pangolin_lineage == "P.2" ~ "Zeta", #Zeta (P.2) #exact match
      pangolin_lineage == "B.1.525" ~ "Eta", #Eta (B.1.525) #exact match
      pangolin_lineage == "P.3" ~ "Theta", #exact match
      pangolin_lineage == "B.1.526" ~ "Iota", #Iota (B.1.526) #exact match
      pangolin_lineage == "B.1.617.1" ~ "Kappa", #Kappa (B.1.617.1) #exact match
      
      pangolin_lineage == "C.37" ~ "Lambda", #exact match
      pangolin_lineage == "C.37.1" ~ "Lambda", #exact match
      
      #Mu (B.1.621, B.1.621.1)
      pangolin_lineage == "B.1.621" ~ "Mu", #exact match
      pangolin_lineage == "B.1.621.1" ~ "Mu", #exact match
      
      #Break up Omicron: Omicron (B.1.1.529, BA.1, BA.2, BA.3, BA.4 and BA.5 lineages)
      pangolin_lineage == "B.1.1.529" ~ "Omicron:B.1.1.529", #exact match
      
      grepl("BA.1", pangolin_lineage, fixed=TRUE) ~ "Omicron:BA.1", #BA.1
      grepl("BA.2", pangolin_lineage, fixed=TRUE) ~ "Omicron:BA.2",
      grepl("BA.3", pangolin_lineage, fixed=TRUE) ~ "Omicron:BA.3",
      grepl("BA.4", pangolin_lineage, fixed=TRUE) ~ "Omicron:BA.4",
      grepl("BA.5", pangolin_lineage, fixed=TRUE) ~ "Omicron:BA.5",
      pangolin_lineage == "XE" ~ "Omicron:XE", #exact match
      
      is.na(pangolin_lineage) ~ "Unassigned",
      pangolin_lineage == "NA" ~ "Unassigned",
      pangolin_lineage == "Unassigned" ~ "Unassigned"
    )
  )

no_WHO_name <- metadata %>%
  dplyr::select(pangolin_lineage, WHO_name) %>%
  filter(is.na(WHO_name)) %>%
  group_by(pangolin_lineage) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

metadata$WHO_name <- ifelse(is.na(metadata$WHO_name), "Other", metadata$WHO_name) #No WHO name

#Order levels by earliest date of collection
metadata$WHO_name <- factor(metadata$WHO_name,
                            levels = c("Other", "Unassigned", "Alpha", "Epsilon", "Zeta", "Iota", 
                                       "Eta", "Beta", "Lambda","Gamma", "Kappa", "Delta:B.1.617.2", "Delta:AY", "Mu",
                                       "Omicron:B.1.1.529", "Omicron:BA.1", "Omicron:BA.2",  "Omicron:BA.3", 
                                       "Omicron:BA.4", "Omicron:BA.5")) #Only a couple of seqs now that are BA.3 and XE in GA in GISAID and they are low coverage...weird

#Set color scheme to be the same across all graphs, regardless of dropout
Palette <-
  setNames( c("slategray", "black", "red4", "firebrick3", "orange4", "sienna1", 
              "yellow", "khaki", "mediumspringgreen", "green4", "darkseagreen2", "mediumblue", "dodgerblue", "lightpink3", 
              "mistyrose","plum", "violet","palevioletred1", 
              "violetred3", "mediumpurple")
            , levels(metadata$WHO_name)  )

levels(metadata$WHO_name)

freq_WHO_name_date <- metadata %>% group_by(WHO_name, date) %>% summarize(n=n()) #Daily aggregate 
freq_who_name_week <- aggregate_week(freq_WHO_name_date) #Weekly aggregate
freq_who_name_week %>% select(WHO_name,n_week) %>% group_by(WHO_name) %>% summarize(sum=sum(n_week))
freq_who_name_week %>% select(week,n_week) %>% group_by(week) %>% summarize(sum=sum(n_week)) %>% arrange(desc(sum))

#Weekly Stacked Barchart, Georgia
p <- weekly_stacked_barchart(freq_who_name_week, Palette)
p + ggtitle("SARS-CoV-2 Sequences, Georgia") +
  theme(axis.text.x = element_text(angle = 90, size = 8),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA))
  
  # annotate(geom = "vline",
  #          x = c("2021-04-04", "2021-11-28"),
  #          xintercept = c("2021-04-04", "2021-11-28"), color = c("mediumblue", "palevioletred1"), 
  #          size=1, linetype = "dashed") +
  # annotate(geom = "text", size=3.5,
  #          label = c("First Delta sequence: epi week 14, 2021-04-09", 
  #                    "First Omicron sequence: epi week 48, 2021-11-30"),
  #          x = c("2021-04-04", "2021-11-28"),
  #          y = 2000,
  #          angle = 90, vjust =1.1) 
ggsave("C:/Users/gev25289/Desktop/december clusters/figures/Seqs Over Time GA.pdf", width = 18, height = 7)

#100% Stacked Barplot, Georgia
freq_WHO_name_date_sub <- metadata %>% 
  group_by(WHO_name, date) %>% 
  summarize(n=n()) %>%
  filter(date >= as.Date("2021-02-01")) %>%
  filter(date <= as.Date("2022-03-31")) 
freq_WHO_name_week_sub <- aggregate_week(freq_WHO_name_date_sub)
freq_WHO_name_week_sub %>% select(WHO_name,n_week) %>% group_by(WHO_name) %>% summarize(sum=sum(n_week))
weekly_100_stacked_barchart(freq_WHO_name_week_sub) +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        )
ggsave("C:/Users/gev25289/Desktop/december clusters/figures/Seqs Over Time 100 GA.pdf", width = 14, height = 6)
