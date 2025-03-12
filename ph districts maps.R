lapply(c(
  "raster", "sf", "broom", "ggplot2", "tidyverse", "leaflet", "tigris", "grid", "cowplot", "tmap", "dplyr",
  "RColorBrewer", "ggspatial", "stats", "phylotools", "DescTools", "lsr", "readxl", "gtools", "gridExtra"), library, character.only = TRUE)

options(scipen=999)
source("C:/Users/gev25289/Desktop/xps/georgia/code/functions.R")

# FIPS to county name: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/ga/home/?cid=nrcs143_013697
# Nice tutorial: https://kelseyandersen.github.io/DataVizR/mapping.html
# https://catalog.data.gov/dataset/tiger-line-shapefile-2019-state-georgia-current-county-subdivision-state-based/resource/48b4ef03-d2cc-498e-8eb6-6e5ab697b171
# 
# Household income: https://hdpulse.nimhd.nih.gov/data-portal/social/table?age=001&age_options=ageall_1&demo=00010&demo_options=income_3&race=00&race_options=race_7&sex=0&sex_options=sexboth_1&socialtopic=030&socialtopic_options=social_6&statefips=13&statefips_options=area_states
# 
# https://data.census.gov/table?g=040XX00US13,13$0500000&d=GEO%20Geography%20Information 

income <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/HDPulse_data_export.csv", skip = 4)
income <- income %>% dplyr::select("County", "Value (Dollars)") %>% mutate(County = gsub(" County", "", County))

landarea <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/land area.csv") 
landarea <- landarea %>% rename("County" = "Geographic Area Name (NAME)" ,  "Area" = "Area (Land, in square miles) (AREALAND_SQMI)") %>% mutate(County = gsub(" County, Georgia", "", County)) %>% dplyr::select ("County", "Area") #Area is in square miles

pop <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/georgia county pop.csv")
pop <- pop %>% left_join(income, by = c("county" = "County"))
pop <- pop %>% left_join(landarea, by = c("county" = "County"))

metadata_delta_highcov <- read_tsv("C:/Users/gev25289/Desktop/xps/georgia/delta_highcoverage.tsv")
metadata_delta_highcov_ag <- metadata_delta_highcov %>% group_by(location) %>% summarize(seqs=n())
metadata_delta_highcov_ag$location <- gsub("_", " ", metadata_delta_highcov_ag$location)

testing <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/ga_covid_data/archived_files/4.1.22_archive_pcr_antigen_col.csv", skip=1)
testing <- testing %>% dplyr::select(c(county, collection_dt, "ALL PCR tests performed", "Antigen Tests Performed")) %>% filter(county %!in% c("Georgia", "Unknown"))
testing <- public_health_district_county(testing)
testing$date <- mdy(testing$collection_dt)
testing <- testing %>%
  filter(date >= as.Date("2021-04-09")) %>% filter(date <= as.Date("2022-03-10")) %>%
  mutate(totaltests = `ALL PCR tests performed` + `Antigen Tests Performed`) 

# Since the number of tests > population size, I double checked the #s against the Fulton County reports:
# https://testcd.fultoncountyga.gov/-/media/COVID-19/Epidemiology-reports/Epidemiology-Reports-2022/Epidemiology-Fulton-County-COVID-19-Report_02232022.pdf
fulton <- testing %>% filter(county == "Fulton") %>%
  filter(date >= as.Date("2022-01-03")) %>% filter(date <= as.Date("2022-01-16"))

testing <- testing %>%
  group_by(public_health_district) %>%
  summarize(totaltests = sum(totaltests)) 
testing$public_health_district <- gsub("_", " ", testing$public_health_district)

weightedsubfasta <- read.fasta("C:/Users/gev25289/Desktop/xps/georgia/masked_filtered.fasta")
weightedsub_metadata <- weightedsubfasta %>% filter(grepl("USA/GA-", seq.name)) %>% dplyr::select(seq.name)
weightedsub_metadata$public_health_district <- str_extract(weightedsub_metadata$seq.name, "[^/]+$")
weightedsub_metadata$public_health_district<- gsub("_", " ", weightedsub_metadata$public_health_district)
weightedsub_metadata_ag <- weightedsub_metadata %>% group_by(public_health_district) %>% summarize(subsampled_seqs=n())

#Load the shapefile 
shp <- st_read("C:/Users/gev25289/Desktop/xps/georgia/raw data/tl_2019_13_cousub/tl_2019_13_cousub.shp")
shp <- shp %>% mutate(FIPS = paste(STATEFP, COUNTYFP, sep = ""))

ggplot() + 
  geom_sf(data = shp, size = 0.2, color = "white", fill = "cyan1") + 
  ggtitle("Map of Georgia Counties") + 
  coord_sf()

fips <- read.csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/County FIPS.csv")
shp <- shp %>% left_join(fips, by = "FIPS") %>% rename("location" = "Name")
shp <- public_health_district(shp)

county <- shp %>%
  group_by(location) %>% 
  summarize(geometry = st_union(geometry))

combine <- shp %>%
  group_by(public_health_district) %>% 
  summarize(geometry = st_union(geometry))

colors <- c("darkred","deeppink","lightpink", "firebrick1", "coral", "deeppink4",
            "darkgoldenrod1","khaki1","antiquewhite3", "darkgoldenrod4", "olivedrab4", 
            "darkseagreen","chartreuse3", "blue","cadetblue1","dodgerblue4", "cornflowerblue","cyan4")

# Map of Counties
ggplot() + 
  geom_sf(data = combine, aes(fill = public_health_district), color = "black", size = 0.6) +  # Public health districts with light fill
  geom_sf(data = county, fill = NA, color = "black", size = 0.3) +  # County borders in white
  geom_sf_label(data = county, aes(label = location), size = 2.2, fontface = "bold", label.size = 0.12, fill = alpha("white", 0.6)) +  # Label counties with better contrast
  #geom_sf_text(data = county, aes(label = location), size = 2.5, fontface = "bold") +  # Label counties
  ggtitle("") + 
  scale_fill_manual(values = colors, name = "Public Health District") +  # Legend title
  theme_void(base_size = 10) +  # Improve overall text size
  theme(
    legend.position = c(0.9, 0.8), 
    legend.title = element_text(size = 9, face = "bold"),  # Make legend title bold
    legend.text = element_text(size = 10),  # Increase legend text size
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.5, "cm"), 
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center title and bold it
    plot.margin = unit(c(-1.5,-1.5,-1,-1.5), "cm"),
    legend.margin = margin(t = 0, b = 0, l = 0, r = 0)
  )

ggsave("C:/Users/gev25289/Desktop/december clusters/figures/ph county map.pdf", width = 24, height = 24, units = "cm")

ph_levels <- c("Northwest", "North Georgia", "North", "Cobb-Douglas", "Fulton", "Clayton", "GNR", "DeKalb", "Northeast", "District 4", "North Central", "East Central", "West Central", "South Central", "Southwest", "South", "Southeast", "Coastal")

latlong <- read.delim("C:/Users/gev25289/Desktop/xps/georgia/clusters/ph districts lat long.csv", sep=",")
latlong$Location <- gsub("_", " ", latlong$Location)
latlong$Location <- str_replace(latlong$Location, "-", "-\n")
latlong$Latitude[latlong$Location == "Fulton"] <- 33.63
latlong$Longitude[latlong$Location == "Fulton"] <- -84.63
latlong$Latitude[latlong$Location == "GNR"] <- 33.95
latlong$Longitude[latlong$Location == "Cobb-\nDouglas"] <- -84.66
latlong$Longitude[latlong$Location == "Northwest"] <- -85.1
latlong$Latitude[latlong$Location == "North Georgia"] <- 34.71
latlong$Longitude[latlong$Location == "West Central"] <- -84.54
latlong$Longitude[latlong$Location == "Southwest"] <- -84.23
latlong$Longitude[latlong$Location == "South Central"] <- -83
latlong$Longitude[latlong$Location == "East Central"] <- -82.1
latlong$Longitude[latlong$Location == "North Central"] <- -83.45

combine$public_health_district <- factor(combine$public_health_district, levels = ph_levels)
points_labels <- sf::st_point_on_surface(combine)
labels_coords <- as.data.frame(sf::st_coordinates(points_labels))
labels_coords$NAME <- combine$public_health_district
#labels_coords <- labels_coords %>%
#filter(NAME %!in% c("Cobb-Douglas", "Fulton", "Clayton", "GNR", "DeKalb")) #space too small

#case data
outbreak <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/epicurve_rpt_date.csv") %>% 
  filter(measure == "county_stats") %>%
  dplyr::select(county, report_date, total_cases) #total cases = PCR confirmed + antigen positive

outbreak_ag <- public_health_district_county(outbreak) %>%
  filter(!is.na(public_health_district)) %>% mutate(report_date = as.Date(report_date, "%Y-%m-%d")) %>%
  filter(report_date >= as.Date("2021-04-09")) %>% filter(report_date <= as.Date("2022-03-10")) %>%
  group_by(public_health_district) %>%
  summarize(n = sum(total_cases)) 

outbreak_ag$public_health_district <- gsub("_", " ", outbreak_ag$public_health_district)
pop <- public_health_district_county(pop)
pop_ag <- pop %>% group_by(public_health_district) %>% summarize(pop = sum(`2020 Total Population`), income = median(`Value (Dollars)`), total_area = sum(Area))
pop_ag <- pop_ag %>% mutate(pop_density = pop/total_area)
pop_ag$public_health_district <- gsub("_", " ", pop_ag$public_health_district)
outbreak_ag <- outbreak_ag %>% left_join(pop_ag)
outbreak_ag$incidence <- outbreak_ag$n/outbreak_ag$pop * 1000
sum(outbreak_ag$n)

combine <- combine %>% left_join(outbreak_ag, by = "public_health_district")
combine <- combine %>% left_join(metadata_delta_highcov_ag, by = c("public_health_district" = "location"))
combine <- combine %>% left_join(weightedsub_metadata_ag)
combine <- combine %>% mutate(SequencingProportion = round(seqs/n * 100, digits = 1))
combine <- combine %>% left_join(testing, by = "public_health_district")
combine$testingper <- round(combine$totaltests/combine$pop * 1000, digits = 0)

# Unified settings for consistent sizing
base_theme <- theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom", 
    legend.spacing = unit(1, "cm"),
    legend.key.size = unit(0.2, "cm"),
    legend.key.width = unit(0.45, "cm"),  # Smaller width
    legend.text = element_text(size = 8),  # Smaller text
    legend.title = element_text(size = 11),
    legend.margin = margin(t = 1, b = 1, l = 1, r = 1))

# First plot
cases_plot <- combine %>%
  ggplot() +
  geom_sf(aes(fill = n), color = "black", size = 1) +
  # geom_label(
  #   data = latlong, 
  #   #segment.color = 'transparent',
  #   aes(Longitude, Latitude, label = Location), 
  #   fontface = "bold", colour = "black", size = 1
  # ) +
  labs(
    fill = "COVID-19 \n Cases",
    x = NULL, y = NULL
  ) +
  scale_fill_distiller(palette = "Reds", direction = 1,     
                       breaks = scales::pretty_breaks(n = 3)) +
  coord_sf() +
  base_theme

# Second plot
incidenceplot <- combine %>%
  ggplot() +
  geom_sf(aes(fill = incidence), color = "black", size = 1) +
  # geom_label(
  #   data = latlong, 
  #   #segment.color = 'transparent', 
  #   aes(Longitude, Latitude, label = Location), 
  #   fontface = "bold", colour = "black", size = 1
  # ) +
  labs(
    fill = "COVID-19 \n Cases \n per 1,000 Persons",
    x = NULL, y = NULL
  ) +
  scale_fill_distiller(palette = "Oranges", direction = 1,
                       breaks = scales::pretty_breaks(n = 3))  +
  coord_sf() +
  base_theme

# Third plot
seqs_plot <- combine %>%
  ggplot() +
  geom_sf(aes(fill = seqs), color = "black", size = 1) +
  # geom_label(
  #   data = latlong, 
  #   #segment.color = 'transparent',
  #   aes(Longitude, Latitude, label = Location), 
  #   fontface = "bold", colour = "black", size = 1
  # ) +
  labs(
    fill = "SARS-CoV-2 \n Delta \n Sequences",
    x = NULL, y = NULL
  ) +
  scale_fill_distiller(palette = "Greens", direction = 1,    breaks = scales::pretty_breaks(n = 3)) +
  coord_sf() +
  base_theme

# Fourth plot
seqprop_plot <- combine %>%
  ggplot() +
  geom_sf(aes(fill = SequencingProportion), color = "black", size = 1) +
  # geom_label(
  #   data = latlong, 
  #   #segment.color = 'transparent',
  #   aes(Longitude, Latitude, label = Location), 
  #   fontface = "bold", colour = "black", size = 1
  # ) +
  labs(
    fill = "Sequencing \n Proportion",
    x = NULL, y = NULL
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1,   breaks = scales::pretty_breaks(n = 3)) +
  coord_sf() +
  base_theme

# Sixth plot
testingplot <- combine %>%
  ggplot() +
  geom_sf(aes(fill = testingper), color = "black", size = 1) +
  # geom_label(
  #   data = latlong, 
  #   #segment.color = 'transparent',
  #   aes(Longitude, Latitude, label = Location), 
  #   fontface = "bold", colour = "black", size = 1
  # ) +
  labs(
    fill = "Total PCR and \n Antigen Tests \n per 1,000 Persons",
    x = NULL, y = NULL
  ) +
  scale_fill_distiller(palette = "Purples", direction = 1,   breaks = scales::pretty_breaks(n = 3)) +
  coord_sf() +
  base_theme

# Combine plots with consistent aspect ratio
combined_plot <- grid.arrange(cases_plot, incidenceplot, seqs_plot, seqprop_plot, testingplot, ncol = 5)

# Save the combined plot
ggsave("C:/Users/gev25289/Desktop/december clusters/figures/ph plots.pdf", combined_plot, width = 32, height = 8, units = "cm")

combine_df <- combine %>% as.data.frame() %>% dplyr::select(-geometry) %>%
  rename("PH District" = "public_health_district",
         "No. of COVID-19 Cases" = "n",
         "Population" = "pop",
         "Incidence per 1,000 Persons" = "incidence",
         "No. of Delta SARS-CoV-2 Sequences (Total)" = "seqs",
         "Proportion of Cases Sequenced " = "SequencingProportion",
         "No. of Delta SARS-CoV-2 Sequences (Subsampled)" = "subsampled_seqs",
         "Total PCR and Antigen Tests per 1,000 Persons" = "testingper",
         "Population Density Per Sq. Mile" = "pop_density",
         "Median Household Income" = "income",
         "Area (Sq. Mile)" = "total_area")
write_csv(combine_df, "C:/Users/gev25289/Desktop/xps/georgia/PH District Demographics.csv")

