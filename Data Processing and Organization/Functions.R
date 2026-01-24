#FUNCTIONS!! :D

`%!in%` = Negate(`%in%`)

#WHO Name
who_name <- function(data){
  data %>%
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
      
      #Break up Omicron: Omicron (B.1.1.529, BA.1, BA.1.1, BA.2, BA.3, BA.4 and BA.5 lineages)
      pangolin_lineage == "B.1.1.529" ~ "Omicron:B.1.1.529", #exact match
      pangolin_lineage == "BA.1.1" ~ "Omicron:BA.1.1", #exact match
      grepl("BA.1.1.", pangolin_lineage, fixed=TRUE) ~ "Omicron:BA.1.1", #BA.1.1.
      
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
}

#Designation of Public Health District: location
###################################################################################################################################################################
public_health_district <- function(data){
  data %>%
    mutate(
      public_health_district = case_when(
        grepl("Dade|Walker|Catoosa|Chattooga|Gordon|Floyd|Bartow|Polk|Paulding|Hara", location) ~ "Northwest",
        grepl("Whitfield|Murray|Fannin|Gilmer|Pickens|Cherokee", location) ~ "North Georgia",
        grepl("Union|Towns|Rabun|Lumpkin|Dawson|Forsyth|Hall|White|Banks|Franklin|Hart|Stephens|Habersham", location) ~ "North",
        grepl("Barrow|Jackson|Madison|Elbert|Oglethorpe|Greene|Morgan|Walton|Oconee|Clarke", location) ~ "Northeast",
        grepl("Gwinnett|Rockdale|Newton", location) ~ "GNR",
        grepl("Wilkes|Taliaferro|Warren|Glascock|Jefferson|Lincoln|McDuffie|Columbia|Richmond|Burke|Jenkins|Emanuel|Screven", location) ~ "East Central",
        grepl("Jasper|Putnam|Hancock|Washington|Monroe|Jones|Baldwin|Crawford|Bibb|Wilkinson|Peach|Houston|Twiggs", location) ~ "North Central",
        grepl("Johnson|Laurens|Bleckley|Pulaski|Wilcox|Dodge|Telfair|Wheeler|Montgomery|Treutlen", location) ~ "South Central",
        grepl("Effingham|Chatham|Bryan|Liberty|Long|McIntosh|Glynn|Camden", location) ~ "Coastal",
        grepl("Candler|Bulloch|Evans|Tattnall|Toombs|Jeff Davis|Coffee|Atkinson|Clinch|Charlton|Ware|Brantley|Pierce|Bacon|Appling|Wayne", location) ~ "Southeast",
        grepl("Turner|Ben Hill|Irwin|Tift|Berrien|Cook|Lanier|Brooks|Lowndes|Echols", location) ~ "South",
        grepl("Terrell|Lee|Worth|Colquitt|Thomas|Grady|Decatur|Seminole|Miller|Early|Calhoun|Baker|Mitchell|Dougherty", location) ~ "Southwest",
        grepl("Harris|Muscogee|Talbot|Taylor|Marion|Chattahoochee|Schley|Macon|Dooly|Crisp|Sumter|Webster|Stewart|Quitman|Randolph", location) ~ "West Central",
        location == "Clay County" | location == "Clay" ~ "West Central",
        grepl("Carroll|Heard|Troup|Meriwether|Coweta|Fayette|Henry|Spalding|Butts|Pike|Lamar|Upson", location) ~ "District 4",
        grepl("Cobb|Douglas", location) ~ "Cobb-Douglas",
        grepl("Fulton", location) ~ "Fulton",
        grepl("Clayton", location) ~ "Clayton",
        grepl("DeKalb|Dekalb|De Kalb", location) ~ "DeKalb"
      )
    )
}

#Designation of Public Health District: county
###################################################################################################################################################################
public_health_district_county <- function(data){
  data %>%
    mutate(
      public_health_district = case_when(
        grepl("Dade|Walker|Catoosa|Chattooga|Gordon|Floyd|Bartow|Polk|Paulding|Hara", county) ~ "Northwest",
        grepl("Whitfield|Murray|Fannin|Gilmer|Pickens|Cherokee", county) ~ "North_Georgia",
        grepl("Union|Towns|Rabun|Lumpkin|Dawson|Forsyth|Hall|White|Banks|Franklin|Hart|Stephens|Habersham", county) ~ "North",
        grepl("Barrow|Jackson|Madison|Elbert|Oglethorpe|Greene|Morgan|Walton|Oconee|Clarke", county) ~ "Northeast",
        grepl("Gwinnett|Rockdale|Newton", county) ~ "GNR",
        grepl("Wilkes|Taliaferro|Warren|Glascock|Jefferson|Lincoln|McDuffie|Columbia|Richmond|Burke|Jenkins|Emanuel|Screven", county) ~ "East_Central",
        grepl("Jasper|Putnam|Hancock|Washington|Monroe|Jones|Baldwin|Crawford|Bibb|Wilkinson|Peach|Houston|Twiggs", county) ~ "North_Central",
        grepl("Johnson|Laurens|Bleckley|Pulaski|Wilcox|Dodge|Telfair|Wheeler|Montgomery|Treutlen", county) ~ "South_Central",
        grepl("Effingham|Chatham|Bryan|Liberty|Long|McIntosh|Glynn|Camden", county) ~ "Coastal",
        grepl("Candler|Bulloch|Evans|Tattnall|Toombs|Jeff Davis|Coffee|Atkinson|Clinch|Charlton|Ware|Brantley|Pierce|Bacon|Appling|Wayne", county) ~ "Southeast",
        grepl("Turner|Ben Hill|Irwin|Tift|Berrien|Cook|Lanier|Brooks|Lowndes|Echols", county) ~ "South",
        grepl("Terrell|Lee|Worth|Colquitt|Thomas|Grady|Decatur|Seminole|Miller|Early|Calhoun|Baker|Mitchell|Dougherty", county) ~ "Southwest",
        grepl("Harris|Muscogee|Talbot|Taylor|Marion|Chattahoochee|Schley|Macon|Dooly|Crisp|Sumter|Webster|Stewart|Quitman|Randolph", county) ~ "West_Central",
        grepl("Carroll|Heard|Troup|Meriwether|Coweta|Fayette|Henry|Spalding|Butts|Pike|Lamar|Upson", county) ~ "District_4",
        county == "Clay" ~ "West_Central",
        grepl("Cobb|Douglas", county) ~ "Cobb-Douglas",
        grepl("Fulton", county) ~ "Fulton",
        grepl("Clayton", county) ~ "Clayton",
        grepl("DeKalb|Dekalb", county) ~ "DeKalb"
      )
    )
}

#Variant Proportions
###################################################################################################################################################################
aggregate_week <- function(data) {
  data %>%
    group_by(WHO_name, week = cut(date, "week", start.on.monday=FALSE)) %>% #Start on Sunday (epi-week)
    summarize(n_week = sum(n))
}

#Weekly Stacked Barchart
weekly_stacked_barchart <- function(data, palette){
  ggplot(data, aes(fill=WHO_name, y=n_week, x=week)) + 
    geom_bar(color="black",position="stack", stat="identity", width=1) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values = Palette, name=NULL, limits=force) + #drop=TRUE didn't work, but limits=force dropped unused levels
    theme_bw() +
    #theme(legend.position="bottom") +
    xlab("\nWeek (Starting Date of Collection)") + ylab("\nNumber of Sequences") + theme(legend.title = element_blank()) 
}

#Weekly 100% Stacked Barchart
weekly_100_stacked_barchart <- function(data){
  data <- data %>%
    mutate(percentage = scales::percent(n_week/sum(n_week)))
  
  percent_labels <- data %>%
    group_by(week)%>%
    mutate(percent=n_week/sum(n_week)) 
  
  ggplot(data = data, aes(fill=WHO_name, y=n_week, x=week)) +
    geom_bar(stat="identity", position ="fill", color='black', width=1) + 
    scale_fill_manual(values = Palette, limits=force) +
    scale_y_continuous(labels = scales::percent) + 
   # geom_text(data = percent_labels, aes(label=paste0(sprintf("%1.0f", percent*100),"%"), angle = 90),
   #           position=position_fill(vjust=0.5), colour="black", size = 3) +
    xlab("Week (Starting Date of Collection)") + ylab("Proportion") + 
    theme_minimal() +
    #theme(legend.position="bottom") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90,hjust =0 )) 
}

#Convert dataset to FASTA format
###################################################################################################################################################################
fasta_format <- function(data){
data$seq.name <- paste(data$strain, data$date, data$epi_week, data$epi_year, data$WHO_name, data$public_health_district, sep = "/")
data <- data %>% 
  select(c("seq.name", "V2")) %>% 
  rename("seq.text" = "V2")

return(data)
}

#Random Subsampling for Growing Trees
###################################################################################################################################################################
#No sampling
no_subsampling <- function(data, end_week){
  #wk <- data %>% filter(week_yr >= 14 & week_yr <= end_week) 
  wk <- data %>% filter(epi_week <= end_week)
  
  #Convert dataset to FASTA format
  wk$seq.name <- paste(wk$strain, wk$date, wk$epi_week, wk$WHO_name, wk$public_health_district, sep = "/")
  wk <- wk %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")
  
  return(wk)
}

#Weighted random sampling
weighted_subsampling <- function(data, end_week, week_total) {
  #week <- data %>% filter(week_yr >= 14 & week_yr <= end_week) 
  week <- data %>% filter(epi_week <= end_week)
  week$weight <- week$total_cases/week_total #weight
  set.seed(101)
  wk_weight <- slice_sample(week, weight_by = weight, n = 15000) #random sampling
  
  #Convert dataset to FASTA format
  wk_weight$seq.name <- paste(wk_weight$strain, wk_weight$date, wk_weight$epi_week,
                              wk_weight$WHO_name, wk_weight$public_health_district, sep = "/")
  wk_weight <- wk_weight %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")

  return(wk_weight)
}

#Stratified random sampling
stratified_subsampling <- function(data, end_week, number){
  #week <- data %>% filter(week_yr >= 14 & week_yr <= end_week) 
  week <- data %>% filter(epi_week <= end_week)
  
  set.seed(101)
  wk_strat <- week %>% group_by(public_health_district, epi_week) %>% slice_sample(n=number) #random sampling
  
  #Convert dataset to FASTA format
  wk_strat$seq.name <- paste(wk_strat$strain, wk_strat$date, wk_strat$epi_week, wk_strat$epi_year,
                             wk_strat$WHO_name, wk_strat$public_health_district, sep = "/")
  wk_strat <- wk_strat %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")
  
  return(wk_strat)
}

#Simple random sampling
simple_subsampling <- function(data, end_week){
  #week <- data %>% filter(week_yr >= 14 & week_yr <= end_week) 
  week <- data %>% filter(epi_week <= end_week)
  set.seed(101)
  wk_simple <- week %>% slice_sample(n=15000) #random sampling
  
  #Convert dataset to FASTA format
  wk_simple$seq.name <- paste(wk_simple$strain, wk_simple$date, wk_simple$epi_week, 
                              wk_simple$public_health_district, sep = "/")
  wk_simple <- wk_simple %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")
  
  return(wk_simple)
}

#####BEAST
no_beast_subsampling <- function(data){
  wk <- data 
  
  #Convert dataset to FASTA format
  wk$seq.name <- paste(wk$strain, wk$date, wk$epi_week, wk$WHO_name, wk$location, sep = "/")
  wk <- wk %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")
  
  return(wk)
}

#Simple random sampling
simple_beast_subsampling <- function(data){
  set.seed(101)
  wk_simple <- slice_sample(data, n=650) #random sampling
  
  #Convert dataset to FASTA format
  wk_simple$seq.name <- paste(wk_simple$strain, wk_simple$date, wk_simple$epi_week,wk_simple$WHO_name, wk_simple$location, sep = "/")
  wk_simple <- wk_simple %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")
  
  return(wk_simple)
}

#Weighted random sampling
weighted_beast_subsampling <- function(data) {
  set.seed(101)
  wk_weight <- slice_sample(data, weight_by = weight, n = 650) #random sampling

  #Convert dataset to FASTA format
  wk_weight$seq.name <- paste(wk_weight$strain, wk_weight$date, wk_weight$epi_week,wk_weight$WHO_name, wk_weight$location, sep = "/")
  wk_weight <- wk_weight %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")
  
  return(wk_weight)
}

#Stratified random sampling
stratified_beast_subsampling <- function(data, number){
  set.seed(101)
  wk_strat <- data %>% group_by(location, epi_week) %>% slice_sample(n=number) #random sampling
  
  #Convert dataset to FASTA format
  wk_strat$seq.name <- paste(wk_strat$strain, wk_strat$date, wk_strat$epi_week,wk_strat$WHO_name, wk_strat$location, sep = "/")
  wk_strat <- wk_strat %>% 
    select(c("seq.name", "V2")) %>% 
    rename("seq.text" = "V2")
  
  return(wk_strat)
}
