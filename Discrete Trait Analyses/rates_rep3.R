#rep3
######

lapply(c(
  "raster", "sf", "broom", "ggplot2", "tidyverse", "leaflet", "tigris", "grid", "cowplot", "tmap", 
  "RColorBrewer", "ggspatial", "stats", "DescTools", "lsr", "readxl", "gtools", "gridExtra"), library, character.only = TRUE)

options(scipen=999)
source("C:/Users/gev25289/Desktop/xps/georgia/code/functions.R")

####################################################################################################################
colors <- c("darkred","deeppink","lightpink", "firebrick1", "coral", "deeppink4",
            "darkgoldenrod1","khaki1","antiquewhite3", "darkgoldenrod4", "olivedrab4", 
            "darkseagreen","chartreuse3", "blue","cadetblue1","dodgerblue4", "cornflowerblue","cyan4")

ph_levels <- c("Northwest", "North Georgia", "North", "Cobb-Douglas", "Fulton", "Clayton", "GNR", "DeKalb", "Northeast", "District 4", "North Central", "East Central", "West Central", "South Central", "Southwest", "South", "Southeast", "Coastal")

#Load the shapefile 
####################################################################################################################
shp <- st_read("C:/Users/gev25289/Desktop/xps/georgia/raw data/tl_2019_13_cousub/tl_2019_13_cousub.shp")
shp <- shp %>% mutate(FIPS = paste(STATEFP, COUNTYFP, sep = ""))

fips <- read.csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/County FIPS.csv")

shp <- shp %>% left_join(fips, by = "FIPS") %>% rename("location" = "Name")
shp <- public_health_district(shp)

county <- shp %>%
  group_by(location) %>% 
  summarize(geometry = st_union(geometry))

combine <- shp %>%
  group_by(public_health_district) %>% 
  summarize(geometry = st_union(geometry))

combine$public_health_district <- factor(combine$public_health_district, levels = ph_levels)
points_labels <- sf::st_point_on_surface(combine)

center_point <- st_point(c(-81, 34)) 
circle_polygon <- st_buffer(center_point, 0.2) #set radius
circle_polygon <- st_sfc(circle_polygon) #convert circle to sfc
st_crs(circle_polygon) <- st_crs(combine) #set crs for circle to match combine shapefile

combine <- st_union(combine, circle_polygon)
plot(combine)

# Lat/Long for PH District Label
####################################################################################################################
labels_coords <- as.data.frame(sf::st_coordinates(points_labels))
labels_coords$NAME <- combine$public_health_district
labels_coords$Y <- ifelse(labels_coords$NAME == "North Georgia", 34.7, labels_coords$Y)
labels_coords$Y <- ifelse(labels_coords$NAME == "Cobb-Douglas", 34, labels_coords$Y)
labels_coords$X <- ifelse(labels_coords$NAME == "Cobb-Douglas", -84.75, labels_coords$X)
labels_coords$Y <- ifelse(labels_coords$NAME == "Fulton", 33.7, labels_coords$Y) 
labels_coords$X <- ifelse(labels_coords$NAME == "Fulton", -84.6, labels_coords$X)
labels_coords$X <- ifelse(labels_coords$NAME == "Clayton", -84.3, labels_coords$X) 
labels_coords$Y <- ifelse(labels_coords$NAME == "Clayton", 33.45, labels_coords$Y) 
labels_coords$X <- ifelse(labels_coords$NAME == "District 4", -84, labels_coords$X) 
labels_coords$Y <- ifelse(labels_coords$NAME == "District 4", 33.5, labels_coords$Y) 
labels_coords$Y <- ifelse(labels_coords$NAME == "GNR", 33.9, labels_coords$Y)
labels_coords <- labels_coords %>%
  add_row(X = -81, Y = 34, NAME = "OOS")

# Lat/Long for Arrows
####################################################################################################################
latlong <- read.delim("C:/Users/gev25289/Desktop/xps/georgia/clusters/ph districts lat long.csv", sep=",")
#latlong$Location <- gsub("_", " ", latlong$Location)
latlong$Latitude[latlong$Location == "Fulton"] <- 33.63
latlong$Longitude[latlong$Location == "Fulton"] <- -84.63
latlong$Latitude[latlong$Location == "GNR"] <- 33.95
latlong$Longitude[latlong$Location == "Cobb-Douglas"] <- -84.66
latlong$Longitude[latlong$Location == "Northwest"] <- -85.1
latlong$Latitude[latlong$Location == "North_Georgia"] <- 34.71
latlong$Longitude[latlong$Location == "West_Central"] <- -84.54
latlong$Longitude[latlong$Location == "Southwest"] <- -84.23
latlong$Longitude[latlong$Location == "South_Central"] <- -83
latlong$Longitude[latlong$Location == "East_Central"] <- -82.1
latlong$Longitude[latlong$Location == "North_Central"] <- -83.45
latlong <- latlong %>%
  add_row(Location = "OOS", Latitude = 34, Longitude = -81)

####################################################################################################################
spread3_all <- read_tsv("C:/Users/gev25289/Desktop/december clusters/rep3/spread.txt") 
spread3 <- spread3_all %>% filter(BAYES_FACTOR > 100) %>% filter(`POSTERIOR PROBABILITY` >= .5) #overwhelming support

spread <- spread3 %>% 
  left_join(latlong, by = join_by(FROM == Location)) %>% rename(Latitude.x = Latitude) %>% rename(Longitude.x = Longitude)
spread <- spread %>% 
  left_join(latlong, by = join_by(TO == Location)) %>% rename(Latitude.y = Latitude) %>% rename(Longitude.y = Longitude) %>% 
  mutate(FromTo = paste(FROM, TO, sep="."))

logfile <- read_delim("C:/Users/gev25289/Desktop/december clusters/rep3/rep3.combined.location.rates.log")
actualRates <- logfile %>% select(starts_with("Location.actualRates"))
locationRates <- logfile %>% select(starts_with("Location.rates"))
colnames(locationRates) <- gsub("Location.rates.", "" , colnames(locationRates))
colnames(actualRates) <- colnames(locationRates)
colnames(actualRates) <- paste("Location.actualRates" ,colnames(actualRates),sep=".") 
meanCol <- colMeans(actualRates)
actualRates <- actualRates %>% rbind(meanCol) #the last row is the column mean of actual rates
actualRatesMean <- actualRates[27004,]
actualRatesMean <- actualRatesMean %>%
  pivot_longer(
    cols = starts_with("Location.actualRates."),
    names_to = "FromTo",
    names_prefix = "Location.actualRates.",
    values_to = "Rate"
  ) 
spread <- spread %>% left_join(actualRatesMean, by = "FromTo")
spread$Rate <- round(spread$Rate, digits = 3)
hist(spread$Rate, main = "Distribution of Rates", col = "lightgray", border = "black")
summary(spread$Rate)

spread_rep3 <- spread

#Identify outliers to remove them before calculating quantiles
##############################################################
Q1 <- quantile(spread$Rate, .25)
Q3 <- quantile(spread$Rate, .75)
IQR <- IQR(spread$Rate)

#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers <- subset(spread, spread$Rate<(Q1 - 1.5*IQR) | spread$Rate>(Q3 + 1.5*IQR))
outliers$Rate

# Remove the outliers
rate_filtered <- spread$Rate[spread$Rate %!in% outliers$Rate]
summary(rate_filtered)

# Calculate quantiles after removing the value
quantiles <- quantile(rate_filtered, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
spread_filter <- spread %>% filter(Rate %!in% outliers$Rate)
spread_filter$Rate_quantile <- cut(spread_filter$Rate,
                                   breaks = c(0, quantiles, max(spread_filter$Rate)),
                                   labels = c("Q1", "Q2", "Q3", "Q4"),
                                   right = TRUE)
spread_filter$quantile <- cut(spread_filter$Rate,
                              breaks = c(0, quantiles, max(spread_filter$Rate)),
                              right = TRUE)

rate_outlier <- spread %>%
  filter(Rate %in% outliers$Rate) %>%
  mutate(quantile = paste("[", min(Rate), ",", max(Rate), "]", sep = "")) %>%
  mutate(quantile = as.factor(quantile)) %>%
  mutate(Rate_quantile = "outlier")

spread <- bind_rows(spread_filter, rate_outlier)
levels(spread$quantile)

# spread$quantile <- factor(spread$Rate_quantile, levels = c(levels(spread$Rate_quantile), "outlier"))
# spread$quantile[spread$Rate %in% outliers$Rate] <- "outlier"

# spread$quantile <- factor(spread$quantile, levels = c(levels(spread$quantile), "2.35"))
# spread$quantile[spread$Rate > 2] <- "2.35"

Q1 <- spread %>% filter(Rate_quantile == "Q1")
Q2 <- spread %>% filter(Rate_quantile == "Q2")
Q3 <- spread %>% filter(Rate_quantile == "Q3")
Q4 <- spread %>% filter(Rate_quantile == "Q4")
o <- spread %>% filter(Rate_quantile == "outlier")

labels_coords_edit <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/clusters/labels_coords_edit.csv")

#####################################################################################################################
color_palette <- setNames(
  c("#d0d0d0", "#4f81bd", "#f1c27d", "#f28d9d", "darkred"), 
  levels(spread$quantile)
)

generate_plot <- function(data) {
  combine %>%
    ggplot() +  
    geom_sf(fill="white", color = "black", size = 2) +
    #geom_label(data = latlong, aes(Longitude, Latitude, label = Location), fontface = "bold", colour = "black", size = 2.2) + #label ph districts
    geom_curve(data = data, aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, yend = Latitude.y, color=quantile), 
               size = 0.3,
               arrow = arrow(length = unit(0.03, "npc"), type="closed", angle=15)) +
    #ggtitle("Map of Georgia PH Districts", subtitle = "") +
    xlab("") + ylab("") + labs(color = "Location Exchange Rates") +
    theme(text = element_text(family = "sans")) +
    scale_color_manual(values=color_palette) +
    coord_sf() + 
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.1),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 8),  # Adjust the font size for the legend title
      legend.text = element_text(size = 7), # Adjust the font size for the legend values
      legend.margin = margin(t = 10, r = 10, b = 10, l = 10) #add white space after legend
    ) +
    expand_limits(x = c(min(data$Longitude.x, data$Longitude.y) - 1, max(data$Longitude.x, data$Longitude.y) + 1), 
                  y = c(min(data$Latitude.x, data$Latitude.y) - 1, max(data$Latitude.x, data$Latitude.y) + 1)) 
}

Q1_plot <- generate_plot(Q1) + ggtitle("") + theme(legend.position = "none")
Q2_plot <- generate_plot(Q2) + ggtitle("") + theme(legend.position = "none")
Q3_plot <- generate_plot(Q3) + ggtitle("") + theme(legend.position = "none")
Q4_plot <- generate_plot(Q4) + ggtitle("") + theme(legend.position = "none")
outlier_plot <- generate_plot(o) + ggtitle("") + theme(legend.position = "none")
all_plot_colors <- generate_plot(spread)

legend <- ggplotGrob(all_plot_colors)$grobs[[which(sapply(ggplotGrob(all_plot_colors)$grobs, function(x) x$name) == "guide-box")]]

combined_plot_colors <- grid.arrange(
  Q1_plot, Q2_plot, Q3_plot, Q4_plot, 
  outlier_plot, 
  ncol = 5,  # Use 5 columns for the plots
  widths = c(1, 1, 1, 1, 1),  # Adjust the widths as needed
  right = legend
)

combined_plot_colors
ggsave("C:/Users/gev25289/Desktop/december clusters/rep3/color_rates_grid.pdf", combined_plot_colors, width = 23, height = 7, units = "cm")
all_plot_colors
ggsave("C:/Users/gev25289/Desktop/december clusters/rep3/color_rates.pdf", all_plot_colors, width = 22, height = 7, units = "cm")

#THICKNESS
####################################################################################################################
arrow_sizes <- setNames(
  c(0.01, 0.25, 0.35, 0.45, 0.55), 
  levels(spread$quantile)
)

generate_plot_thick <- function(data) {
  combine %>%
    ggplot() +  
    geom_sf(fill="white", color = "black", size = 2) +
    geom_curve(data = data, aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, yend = Latitude.y, size = quantile), 
               arrow = arrow(length = unit(0.03, "npc"), type="closed", angle=15)) +
    xlab("") + ylab("") + labs(size = "Location Exchange Rate") +
    theme(text = element_text(family = "sans")) +
    scale_size_manual(values = arrow_sizes) +  # Arrow thickness based on quantile intervals
    coord_sf() + 
    theme_void() + 
    theme(
      plot.title = element_text(hjust = 0.1),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 8),  # Adjust the font size for the legend title
      legend.text = element_text(size = 7), # Adjust the font size for the legend values
      legend.margin = margin(t = 10, r = 10, b = 10, l = 10) #add white space after legend
    ) +
    expand_limits(x = c(min(spread$Longitude.x, spread$Longitude.y) - 1, max(spread$Longitude.x, spread$Longitude.y) + 1), 
                  y = c(min(spread$Latitude.x, spread$Latitude.y) - 1, max(spread$Latitude.x, spread$Latitude.y) + 1)) 
}

# Generate plots as before, but now with size mapping for arrow thickness
Q1_plot <- generate_plot_thick(Q1) + ggtitle("") + theme(legend.position = "none")
Q2_plot <- generate_plot_thick(Q2) + ggtitle("") + theme(legend.position = "none")
Q3_plot <- generate_plot_thick(Q3) + ggtitle("") + theme(legend.position = "none")
Q4_plot <- generate_plot_thick(Q4) + ggtitle("") + theme(legend.position = "none")
outlier_plot <- generate_plot_thick(o) + ggtitle("") + theme(legend.position = "none")
all_plot_thick <- generate_plot_thick(spread)

# Extract the legend for the size (quantile) mapping from the 'all' plot
legend <- ggplotGrob(all_plot_thick)$grobs[[which(sapply(ggplotGrob(all_plot_thick)$grobs, function(x) x$name) == "guide-box")]]

# Combine the plots without the legend and add the legend from 'all'
# Combine the plots without the legend and add the legend from 'all'
combined_plot_thick <- plot_grid(
  Q1_plot, Q2_plot, Q3_plot, Q4_plot, outlier_plot, 
  ncol = 5,
  rel_heights = c(1,1,1,1,1),
  rel_widths = c(1,1,1,1,1),
  labels = c("Q1", "Q2", "Q3", "Q4", "Outliers"),
  label_y = 0.8, 
  label_size = 7,  # Adjust the label size as needed
  align = 'v',      # Align horizontally
  axis = 'bt'       # Align bottom and top axes
)

combined_plot_thick <- plot_grid(
  combined_plot_thick, legend, 
  ncol = 2,
  rel_widths = c(5, 1)  # Adjust the relative widths to give space for the legend
)

combined_plot_thick
ggsave("C:/Users/gev25289/Desktop/december clusters/rep3/thickness_rates_grid.pdf", combined_plot_thick, width = 23, height = 7, units = "cm")

all_plot_thick
ggsave("C:/Users/gev25289/Desktop/december clusters/rep3/thickness_rates.pdf", all_plot_thick, width = 23, height = 7, units = "cm")

# To combine with other subsamples later
# Create a title grob
title_grob <- ggdraw() + 
  draw_label("Replicate 3", size = 10, fontface = "bold", hjust = 0.5, vjust = 3)

# Combine the title and the final plot
rep3_combined_plot_thick <- plot_grid(
  title_grob,          # Add the title grob
  combined_plot_thick,  # Your combined plot with legend
  ncol = 1,
  rel_heights = c(0.05, 1),  # Space for title and plot
  labels = NULL          # No need for additional labels
)
rep3_combined_plot_thick

rep3_all_plot_thick <- plot_grid(
  title_grob,          # Add the title grob
  all_plot_thick,  # Your combined plot with legend
  ncol = 1,
  rel_heights = c(0.1, 1),  # Space for title and plot
  labels = NULL          # No need for additional labels
)

####################################################################################################################
spread3_50 <- spread3_all %>% filter(`POSTERIOR PROBABILITY` >= .5)
spread3_50_substantial <- spread3_50 %>% filter(BAYES_FACTOR >= 3.2 & BAYES_FACTOR <= 10) 
spread3_50_strong <- spread3_50 %>% filter(BAYES_FACTOR >= 11 & BAYES_FACTOR <= 30) 
spread3_50_verystrong <- spread3_50 %>% filter(BAYES_FACTOR >= 31 & BAYES_FACTOR <= 100) 

spread_substantial <- spread3_50_substantial %>% 
  left_join(latlong, by = join_by(FROM == Location)) %>% rename(Latitude.x = Latitude) %>% rename(Longitude.x = Longitude)
spread_substantial <- spread_substantial %>% 
  left_join(latlong, by = join_by(TO == Location)) %>% rename(Latitude.y = Latitude) %>% rename(Longitude.y = Longitude) %>% 
  mutate(FromTo = paste(FROM, TO, sep="."))

spread_strong <- spread3_50_strong %>% 
  left_join(latlong, by = join_by(FROM == Location)) %>% rename(Latitude.x = Latitude) %>% rename(Longitude.x = Longitude)
spread_strong <- spread_strong %>% 
  left_join(latlong, by = join_by(TO == Location)) %>% rename(Latitude.y = Latitude) %>% rename(Longitude.y = Longitude) %>% 
  mutate(FromTo = paste(FROM, TO, sep="."))

spread_verystrong <- spread3_50_verystrong %>% 
  left_join(latlong, by = join_by(FROM == Location)) %>% rename(Latitude.x = Latitude) %>% rename(Longitude.x = Longitude)
spread_verystrong <- spread_verystrong %>% 
  left_join(latlong, by = join_by(TO == Location)) %>% rename(Latitude.y = Latitude) %>% rename(Longitude.y = Longitude) %>% 
  mutate(FromTo = paste(FROM, TO, sep="."))

generate_plot_black <- function(data) {
  combine %>%
    ggplot() +  
    geom_sf(fill="white", color = "black", size = 2) +
    #geom_label(data = latlong, aes(Longitude, Latitude, label = Location), fontface = "bold", colour = "black", size = 2.2)+ #label ph districts
    geom_curve(data = data, aes(x = Longitude.x, y = Latitude.x, xend = Longitude.y, yend = Latitude.y), 
               size = 0.1,
               arrow = arrow(length = unit(0.03, "npc"), type="closed", angle=15)) +
    #ggtitle("Map of Georgia PH Districts", subtitle = "") +
    xlab("") + ylab("") + labs(color = "Location Exchange Rates") +
    theme(text = element_text(family = "sans")) +
    #scale_color_manual(values=color_palette) +
    coord_sf() + 
    theme_void() +
    theme(
      plot.title = element_text(size = 7, hjust = 0.1),
      plot.background = element_rect(fill = "white", color = NA),
      #legend.title = element_text(size = 8),  # Adjust the font size for the legend title
      #legend.text = element_text(size = 7), # Adjust the font size for the legend values
      #legend.margin = margin(t = 10, r = 10, b = 10, l = 10) #add white space after legend
    ) +
    expand_limits(x = c(min(spread$Longitude.x, spread$Longitude.y) - 1, max(spread$Longitude.x, spread$Longitude.y) + 1), 
                  y = c(min(spread$Latitude.x, spread$Latitude.y) - 1, max(spread$Latitude.x, spread$Latitude.y) + 1)) 
}

decisive_plot <- generate_plot_black(spread) + ggtitle ("Decisive")
verystrong_plot <- generate_plot_black(spread_verystrong) + ggtitle ("Very Strong")
strong_plot <- generate_plot_black(spread_strong) + ggtitle ("Strong")
substantial_plot <- generate_plot_black(spread_substantial) + ggtitle ("Substantial")

bf_combined_plot <- plot_grid(#substantial_plot, 
  strong_plot, verystrong_plot, decisive_plot,
  ncol = 1)
ggsave("C:/Users/gev25289/Desktop/december clusters/rep3/bf_grid.pdf", bf_combined_plot, width = 3, height = 8, units = "cm")

#To combine with other subsamples later
# Create a title grob
title_grob <- ggdraw() + 
  draw_label("Replicate 3", size = 8, fontface = "bold", hjust = 0.5)

# Combine the title and the plot using plot_grid
rep3_bf_combined_plot <- plot_grid(
  title_grob,           # Add the title grob
  bf_combined_plot,     # Your main plot
  ncol = 1,
  rel_heights = c(0.1, 1),  # Adjust space for title and plot
  labels = NULL          # No need for labels
)

rm(list = setdiff(ls(), c("rep0_bf_combined_plot", "rep0_combined_plot_thick", "rep0_all_plot_thick", "spread_rep0",
                          "rep1_bf_combined_plot", "rep1_combined_plot_thick", "rep1_all_plot_thick", "spread_rep1",
                          "rep2_bf_combined_plot", "rep2_combined_plot_thick", "rep2_all_plot_thick", "spread_rep2",
                          "rep3_bf_combined_plot", "rep3_combined_plot_thick", "rep3_all_plot_thick", "spread_rep3")))
