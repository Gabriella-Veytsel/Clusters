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

library(devtools)
install_github("helixcn/phylotools", build_vignettes = TRUE)
library(phylotools) #read.fasta

source("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/GitHub/Functions.R")
`%!in%` = Negate(`%in%`)

#Import data 24,270 
metadata <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/georgia delta complete high_cov collect/combined.tsv") %>%
  filter(strain != "strain") %>% mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  mutate(date_submitted = as.Date(date_submitted, "%Y-%m-%d")) %>% 
  filter(gisaid_epi_isl %!in% c("EPI_ISL_3640412", "EPI_ISL_3640461")) #When matching to fasta, discovered that 2 isolates (GISAID Name: hCoV-19/USA/GA-GD-081721-21072022899/2021 and hCoV-19/USA/GA-GD-081721-21071922011/2021) each have 2 GISAID Assession IDs (duplicate rows in GISAID)

fasta <- phylotools::read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/georgia delta complete high_cov collect/combined.fasta") 

#case data
outbreak <- read_csv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/raw data/epicurve_rpt_date.csv") %>% 
  filter(measure == "county_stats") %>%
  select(county, report_date, total_cases) #total cases = PCR confirmed + antigen positive

#aggregate cases by week : denominator for weighted
week_cases_ga <- outbreak %>% mutate(epi_week = epiweek(report_date)) %>% mutate(epi_year = epiyear(report_date)) %>%
  group_by(epi_week, epi_year) %>% summarize(total_cases_ga = sum(total_cases)) 

#aggregate by ph district and week: numerator for weighted
outbreak_district_ga <- public_health_district_county(outbreak) %>%
  filter(!is.na(public_health_district)) %>% mutate(report_date = as.Date(report_date, "%Y-%m-%d")) %>%
  filter(report_date >= as.Date("2021-04-09")) %>% filter(report_date <= as.Date("2022-03-10")) %>%
  mutate(epi_week = lubridate::epiweek(report_date)) %>% mutate(epi_year = lubridate::epiyear(report_date)) %>%
  group_by(public_health_district, epi_week, epi_year) %>% summarize(cases = sum(total_cases))

#aggregate cases by public health district : figure
ph_cases <- outbreak_district_ga %>% group_by(public_health_district) %>% summarize(total_cases = sum(cases))

NS3 <- read_csv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/raw data/NS3.csv") %>% dplyr::select(c(`GISAID Accession`, `GISAID Name`, Zip)) %>% distinct()
ELR <- read_csv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/raw data/ELR.csv") %>% dplyr::select(c(`GISAID Accession`, `GISAID Name`, Zip)) %>% distinct()
#All I need is zip code from this dataset

#Clean zip code field
#table(NS3$zip) #don't want to guess the ones with intervals
NS3 <- NS3 %>% mutate(Zip = as.integer(Zip)) %>% #but, can fix the ones with decimals for crosswalk 
  mutate(Zip = as.character(Zip)) %>% distinct() #change back for crosswalk join
ELR <- ELR %>% mutate(Zip = as.integer(Zip)) %>%  
  mutate(Zip = as.character(Zip)) 

NS3_name <- NS3 %>% filter(!is.na(`GISAID Name`)) %>% select(-`GISAID Accession`)
NS3_accession <- NS3 %>% filter(!is.na(`GISAID Accession`)) %>% select(-`GISAID Name`)

#Add zip code column to larger GISAID download
metadata <- left_join(metadata, NS3_name, by = c("strain"="GISAID Name")) 
metadata <- left_join(metadata, NS3_accession, by = c("gisaid_epi_isl"= "GISAID Accession")) %>% distinct() 
metadata %>% group_by_all() %>% filter(n()>1) %>% ungroup() #used distinct() above since one was duplicate in NS3 
metadata$zip <- ifelse(is.na(metadata$Zip.x), metadata$Zip.y, metadata$Zip.x)
metadata <- left_join(metadata, ELR, by = c("gisaid_epi_isl"= "GISAID Accession"))
metadata$zip <- ifelse(is.na(metadata$zip), metadata$Zip, metadata$zip)

#Take a look at what the GISAID variable location is comprised of before adding NS3 data
location <- metadata %>%
  filter(!is.na(location)) %>%
  dplyr::select(location) %>% group_by(location) %>% summarize(n=n())

#Package zipcodeR for zip code croswalk 
zip_code_db <- zipcodeR::zip_code_db %>%
  dplyr::select(c(zipcode, major_city, county, state, lat, lng, 
                  population, population_density, land_area_in_sqmi, median_household_income))

metadata <- left_join(metadata, zip_code_db, by = c("zip" = "zipcode")) 
metadata %>% filter(!is.na(location)) %>% filter(!is.na(zip)) #there are no observations where zip (dph) and location (gisaid) are both reported, so don't have to worry about them conflicting/overriding

#Clean GISAID location field
#Don't guess: Austell, Buford, Evans, Loganville, Villa Rica
metadata <- metadata %>%
  mutate(location = case_when(
    grepl("Atlanta|ATLANTA|Alpharetta|ALPHARETTA|Fulton|Snellville|Sandy Springs|East Point|Roswell|Johns Creek|JOHNS CREEK|Milton|Union City", location) ~ "Fulton County",  grepl("DeKalb|Dekalb|Decatur|DECATUR|Doraville|Dunwoody|DUNWOODY|Brookhaven|Lithonia|LITHONIA|STONE MOUNTAIN|Stonecrest|TUCKER", location) ~ "DeKalb County", grepl("Gwinnett|Duluth|DULUTH|Lawrenceville|LAWRENCEVILLE|Norcross|NORCROSS|PEACHTREE CORNERS|Sugar Hill", location) ~ "Gwinnett County", grepl("MILLEDGEVILLE", location) ~ "Baldwin County", grepl("CARTERSVILKE", location) ~ "Bartow County", grepl("Bibb|Macon", location) ~ "Bibb County", grepl("Richmond Hill|RICHMOND HILL", location) ~ "Bryan County", grepl("Bulloch|Statesboro|STATESBORO", location) ~ "Bulloch County",
    grepl("Kingsland|KINGSLAND|SAINT MARY'S|WOODBINE", location) ~ "Camden County", grepl("METTER", location) ~ "Candler County", grepl("Carrollton|Whitesburg", location) ~ "Carroll County", grepl("Ringgold|RINGGOLD", location) ~ "Catoosa County", grepl("Chattahoochee County", location) ~ "Chattahoochee County", grepl("Chatham County|Savannah|SAVANNAH|SAVANNAV", location) ~ "Chatham County",grepl("Canton|CANTON", location) ~ "Cherokee County", grepl("ATHENS", location) ~ "Clarke County", grepl("ClaytonCounty|Clayton County|Ellenwood|ELLENWOOD|Jonesboro|Riverdale", location) ~ "Clayton County",
    grepl("Cobb|Kennesaw|KENNESAW|Mableton|MABLETON|Marietta|MARIETTA|POWDER SPRINGS", location) ~ "Cobb County", grepl("Colquitt County", location) ~ "Colquitt County", grepl("Columbia|Martinez", location) ~ "Columbia County", grepl("Cass County", county) ~ "Bartow County", grepl("NEWNAN|Newnan|Newman|SENOIA|Sharpsburg|SHARPSBURG", location) ~ "Coweta County", grepl("DAWSONVILLE", location) ~ "Dawson County", grepl("Albany|ALBANY", location) ~ "Dougherty County", grepl("Douglasville|DOUGLASVILLE|Lithia Springs", location) ~ "Douglas County", grepl("SPRINGFIELD|Effingham County", location) ~ "Effingham County", grepl("Fayette|Fayetteville|FAYETTEVILLE|Peachtree City", location) ~ "Fayette County", grepl("CUMMING|Forsyth County", location) ~ "Forsyth County",
    grepl("ELLIJAY", location) ~ "Gilmer County", grepl("BRUNSWICK", location) ~ "Glynn County", grepl("Flowery Branch|FLOWERY BRANCH", location) ~ "Hall County", grepl("McDonough|Hampton|HAMPTON|MCDONOUGH|STOCKBRIDGE", location) ~ "Henry County", grepl("Warner Robins|Houston|WARNER ROBINS|Warner Robins", location) ~ "Houston County", grepl("Commerce|COMMERCE|Jackson County", location) ~ "Jackson County", grepl("Lakeland", location) ~ "Lanier County", grepl("Liberty", location) ~ "Liberty County", grepl("Ludowici", location) ~ "Long County", grepl("COLBERT", location) ~ "Madison County", grepl("Thomson|THOMSON", location) ~ "McDuffie County",
    grepl("Columbus|COLUMBUS", location) ~ "Muscogee County", grepl("Dallas|DALLAS|HIRAM", location) ~ "Paulding County", grepl("Peach County", location) ~ "Peach County", grepl("AUGUSTA", location) ~ "Richmond County", grepl("Conyers|CONYERS", location) ~ "Rockdale County", grepl("Griffin", location) ~ "Spalding County", grepl("Toccoa", location) ~ "Stephens County", grepl("Thomasville|Thomas County", location) ~ "Thomas County", grepl("Lagrange|LAGRANGE|Troup", location) ~ "Troup County", grepl("BLAIRSVILLE", location) ~ "Union County", grepl("THOMASTON", location) ~ "Upson County", grepl("Rossville|FLINTSTONE|ROSSVILLE", location) ~ "Walker County", grepl("JESUP", location) ~ "Wayne County", grepl("Dalton|DALTON|COHUTTA", location) ~ "Whitfield County", grepl("Wilkinson County", location) ~ "Wilkinson County"
  ))

metadata$location <- ifelse(is.na(metadata$location), metadata$county, metadata$location)

#Convert county to public health district
metadata <- public_health_district(metadata)
metadata %>% filter(is.na(public_health_district)) %>% group_by(location) %>% summarize(n=n()) 
metadata %>% filter(is.na(public_health_district)) %>% group_by(county) %>% summarize(n=n()) 

#Add epiweek and epiyear for collection date
#colnames(metadata)
metadata <- metadata %>% mutate(epi_week = epiweek(date)) %>% mutate(epi_year = epiyear(date)) %>% 
  select(-c(location, county)) %>% 
  mutate(public_health_district = gsub(" ", "_", public_health_district)) %>%
  rename(location = public_health_district)%>% 
  select(-c("Zip.x", "Zip.y", "Zip", "GISAID Name"))

metadata <- metadata %>% filter(!is.na(location)) #n = 20,674, 3,596 don't have a public health district (15%). This may be a bias against small areas that don't feel comfortable reporting zip code, but I want to have as many ph districts as possible

delta_fasta <- left_join(metadata, fasta, by = c("strain" = "seq.name")) %>% distinct()
delta_fasta <- left_join(delta_fasta, outbreak_district_ga, by = c("epi_week", "epi_year", "location" = "public_health_district"))
delta_fasta <- left_join(delta_fasta, week_cases_ga, by=c("epi_week", "epi_year") )

metadata <- metadata %>% 
  mutate(strain = paste(metadata$strain, 
                        metadata$date, 
                        metadata$epi_week,  
                        metadata$location, sep = "/"))

#export(metadata, "C:/Users/gev25289/Desktop/xps/georgia/delta_highcoverage.tsv")
metadata_delta_highcoverage <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/delta_highcoverage.tsv")

delta_fasta <- delta_fasta %>% filter(!is.na(location))
delta_fasta <- delta_fasta %>% 
  mutate(strain = paste(delta_fasta$strain, 
                        delta_fasta$date, 
                        delta_fasta$epi_week,  
                        delta_fasta$location, sep = "/"))
#write_tsv(delta_fasta, "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/delta_fasta.fasta")

#WEIGHTED SUBSAMPLING STRATEGY FOR GEORGIA SEQUENCES
####################################################
delta_fasta$weight <- delta_fasta$cases/delta_fasta$total_cases_ga #weight

weighted_subsampling <- function(data, seed) {
  set.seed(seed)
  wk_weight <- dplyr::slice_sample(data, weight_by = weight, n = 5000) 
  wk_weight <- wk_weight %>% select(c("strain", "seq.text")) %>% rename("seq.name" = "strain")
  return(wk_weight)
}

#dat2fasta(weighted_subsampling(delta_fasta, seed=101), "C:/Users/gev25289/Desktop/xps/georgia/nextstrain/weighted_sub.fasta") #original
#dat2fasta(weighted_subsampling(delta_fasta, seed=50), "C:/Users/gev25289/Desktop/xps/georgia/nextstrain/weighted_sub_rep1.fasta")
#dat2fasta(weighted_subsampling(delta_fasta, seed=30), "C:/Users/gev25289/Desktop/xps/georgia/nextstrain/weighted_sub_rep2.fasta")
#dat2fasta(weighted_subsampling(delta_fasta, seed=500), "C:/Users/gev25289/Desktop/xps/georgia/nextstrain/weighted_sub_rep3.fasta")
#dat2fasta(weighted_subsampling(delta_fasta, seed=9), "C:/Users/gev25289/Desktop/xps/georgia/nextstrain/weighted_sub_rep4.fasta")

#Uniform Subsampling Strategy for Georgia Sequences
###################################################
uniform_subsampling_week <- function(data, seed, prop) {
  set.seed(seed)
  
  data %>%
    mutate(week_id = paste(epi_year, epi_week, sep = "_")) %>%
    group_by(week_id) %>%
    slice_sample(prop = prop) %>%
    ungroup() %>%
    select(strain, seq.text) %>%
    rename(seq.name = strain)
}

#dat2fasta(uniform_subsampling_week(delta_fasta, seed=101, prop = 0.1), "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.1.fasta")
#dat2fasta(uniform_subsampling_week(delta_fasta, seed=50, prop = 0.2), "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.2.fasta")
#dat2fasta(uniform_subsampling_week(delta_fasta, seed=30, prop = 0.3), "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.3.fasta")

#dat2fasta(uniform_subsampling_week(delta_fasta, seed=500, prop = 0.4), "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.4.fasta")
#dat2fasta(uniform_subsampling_week(delta_fasta, seed=9, prop = 0.5), "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.5.fasta")

uniform_sub_0.1 <- phylotools::read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.1.fasta")
uniform_sub_0.2 <- phylotools::read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.2.fasta")
uniform_sub_0.3 <- phylotools::read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.3.fasta")
uniform_sub_0.4 <- phylotools::read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.4.fasta")
uniform_sub_0.5 <- phylotools::read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/nextstrain/uniform_sub_0.5.fasta")

#GLOBAL CONTEXTUAL SEQUENCES
#########################################################################################################################
#Download those 50k genomes from GISAID, will serve as a dataset to "blast" focal sequences against (Nextstrain's proximity score)
contextual_fasta1 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351282239.sequences.fasta")
contextual_fasta2 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351517420.sequences.fasta")
contextual_fasta3 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351754831.sequences.fasta")
contextual_fasta4 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351952484.sequences.fasta")
contextual_fasta5 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352142926.sequences.fasta")
contextual_fasta6 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352346092.sequences.fasta")
contextual_fasta7 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352559319.sequences.fasta")
contextual_fasta8 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352769608.sequences.fasta")
contextual_fasta9 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352966476.sequences.fasta")
contextual_fasta10 <- read.fasta("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686353166160.sequences.fasta")
contextual_fasta <- bind_rows(contextual_fasta1, contextual_fasta2, contextual_fasta3, contextual_fasta4, contextual_fasta5, contextual_fasta6, contextual_fasta7, contextual_fasta8, contextual_fasta9, contextual_fasta10)
dat2fasta(contextual_fasta, "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/combined.fasta")

contextual_metadata1 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351282239.metadata.tsv")
contextual_metadata2 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351517420.metadata.tsv")
contextual_metadata3 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351754831.metadata.tsv")
contextual_metadata4 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686351952484.metadata.tsv")
contextual_metadata5 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352142926.metadata.tsv")
contextual_metadata6 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352346092.metadata.tsv")
contextual_metadata7 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352559319.metadata.tsv")
contextual_metadata8 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352769608.metadata.tsv")
contextual_metadata9 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686352966476.metadata.tsv")
contextual_metadata10 <- read_tsv("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/1686353166160.metadata.tsv")
contextual_metadata <- bind_rows(contextual_metadata1, contextual_metadata2, contextual_metadata3, contextual_metadata4, contextual_metadata5, contextual_metadata6, contextual_metadata7, contextual_metadata8, contextual_metadata9, contextual_metadata10)

#contextual_metadata <- read.delim("C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia old/gisaid/50k dataset for contextual samples/contextual_metadata.tsv", quote = "", row.names = NULL, stringsAsFactors = FALSE) #I don't know what's up with this file, but it won't read as tsv. Have to disable quoting like this
contextual_metadata <- contextual_metadata %>% filter(strain != "strain") %>% select(-c(location)) 
contextual_metadata <- contextual_metadata %>% filter(!(country == "USA" & division == "Georgia")) 
contextual_metadata$week <- lubridate::week(contextual_metadata$date)
contextual_metadata$year <- lubridate::year(contextual_metadata$date)
contextual_metadata$week_year <- paste(contextual_metadata$year, contextual_metadata$week, sep = "_")
#contextual_metadata$location <- "OOS"
contextual_metadata$division <- "OOS"
write_tsv(contextual_metadata, "C:/Users/u6070907/Box/UGA/Manuscript - Clusters/Analysis/georgia/GISAID/global delta complete high_cov collect/extracted tar files/contextual_metadata.tsv")

#set.seed(105)
contextual_universal <- contextual_metadata %>% group_by(region, year, week) %>% slice_sample(n=37)

contextual_fasta <- read.fasta()
contextual_fasta <- contextual_fasta %>% distinct()
#contextual_fasta5000 <- contextual_fasta %>% left_join(contextual_universal, by = c("seq.name" = "strain"))

#dat2fasta(contextual_fasta5000, "C:/Users/gev25289/Desktop/xps/georgia/contextual_universal.fasta")
#export(contextual_universal, "C:/Users/gev25289/Desktop/xps/georgia/contextual_universal.tsv")

#Reference tsv
references_metadata <- read_tsv("D:/georgia/references_metadata.tsv") 
references_metadata <- references_metadata %>% rename(loc = location) %>% mutate(location = "OOS") %>% rename(div = division) %>% mutate(division = "OOS") #new name = old name syntax
#export(references_metadata, "D:/georgia/references_metadata.tsv")