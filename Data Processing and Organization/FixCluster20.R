# Cluster 20 has some states that are empty. Skip these, so the file can be read
###########################################################################################################################
read_and_filter <- function(file_path, expected_columns = 4) {
  lines <- readLines(file_path)  # Read file as text
  
  # Filter lines with the expected number of columns
  valid_lines <- lines[sapply(lines, function(line) length(strsplit(line, "\t")[[1]]) == expected_columns)]
  
  # Convert the valid lines back to a data frame
  data <- read.table(text = paste(valid_lines, collapse = "\n"), header = TRUE)
  
  return(data)
}

c20_cleaned <- read_and_filter("/scratch/gev25289/workdir/georgia/delta/clusters/individual_dta/rep0/c20/jumptimes.txt")

#Jumps
############################################################
state <- n_distinct(c20_cleaned$state) #Number of total state counts
c20_cleaned$from_to = paste(c20_cleaned$from,c20_cleaned$to, sep=".")
c20_cleaned <- c20_cleaned %>% mutate(time = max(cluster_tips[[20]]$`numeric date`) - as.numeric(time))
c20_cleaned$year <- format(date_decimal(c20_cleaned$time), "%Y-%m")

#From, To
count_total <- c20_cleaned %>% group_by(from_to) %>% count()
count_total <- count_total %>% separate_wider_delim(from_to, ".", names = c("From", "To"))
count_total <- count_total %>% mutate(ave=n/state) %>% mutate(Cluster = 20)

#Jumps over Time
############################################################
#From, To
count <- c20_cleaned %>% group_by(from_to, year) %>% count()
count2 <- cbind(count, read.table(text = as.character(count$from_to), sep = ".")) #Forgot to do "From" and "To", so it's V1, V2
count2 <- count2 %>% mutate(ave=n/state) %>% mutate(Cluster = 20)

all_jumps_time <- bind_rows(all_jumps_time, count2)
all_jumps <- bind_rows(all_jumps, count_total)
