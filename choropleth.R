# Tutorial
# https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2/

# Add on for labelling of data (undone)
# https://stackoverflow.com/questions/22038640/labeling-center-of-map-polygons-in-r-ggplot

# Possible data source for Eurobarometer
# http://data.europa.eu/euodp/en/data/dataset/S2143_88_3_STD88_ENG

install.packages(c("geojsonio", "dplyr", "broom", "ggplot2", "mapproj"))

library(geojsonio)
library(dplyr)
library(broom)
library(ggplot2)
library(mapproj)

# Obtain geojson data
spdf <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
spdf@data

# List EU countries
eu_countries_txt <- "Belgium, Greece, Lithuania, Portugal, Bulgaria, Spain, Luxembourg, Romania, Czech Republic, 
France, Hungary, Slovenia, Denmark, Croatia, Malta, Slovakia, Germany, Italy, Netherlands, 
Finland, Estonia, Cyprus, Austria, Sweden, Ireland, Latvia, Poland, United Kingdom"

eu_countries <- trimws(strsplit(eu_countries_txt, ", ")[[1]])

code_to_cty <- as.data.frame(spdf@data) %>%
  transmute(code = as.character(id), name = as.character(name))

filtered_code_to_cty <- code_to_cty[code_to_cty$name %in% eu_countries,]

# Find Centroids of EU countries country 
centroids_df <- as.data.frame(coordinates(spdf))
centroids_df$area <- area(spdf)
names(centroids_df) <- c("long_centre", "lat_centre", "area")
centroids_df$country <- as.character(code_to_cty$name)
centroids_df$code <- as.character(code_to_cty$code)
centroids_df <- centroids_df %>% filter(
  country %in% eu_countries & area > 0.8e11) # Only centroids in the region, above a minimum size is important

# Filter out Antarctica which has problems with latitude and longitude
all_countries <- unique(spdf@data$name)
filtered_countries <- all_countries[-which(all_countries == "Antarctica")] 

# Fortify the coordinates of countries
filtered_spdf <- spdf[spdf@data$name %in% filtered_countries , ]
spdf_fortified <- tidy(filtered_spdf[filtered_spdf@data$name %in% filtered_countries,], region = "name")

# Read input data from csv
data <- read.csv('poll_data.csv', stringsAsFactors = FALSE)
names(data) <- c("country", "poll.2007", "poll.2017")

adj_size <- function(x, min_size = 1, max_size = 6) {
  ((x - min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))*(max_size - min_size) + min_size
}

# Join data together
spdf_fortified2 = spdf_fortified %>%
  left_join(., data, by=c("id"="country")) %>%
  mutate(diff = poll.2017 - poll.2007) %>% 
  left_join(centroids_df, by = c("id" = "country")) %>%
  mutate(font_size = ifelse(is.na(adj_size(area)), 0, adj_size(area)))



# Plot diff with fancy colours
ggplot(spdf_fortified2) +
  geom_polygon(aes(fill = diff, x = long, y = lat, group = group) , 
               size=0, alpha=0.9) +
  geom_text(aes(label = code, x = long_centre, y = lat_centre, size = font_size), show.legend = FALSE) + 
  theme_void() +
  scale_fill_gradient2(breaks=c(-0.4,-0.2,0,0.2), 
                       name="Change in Trust"
  ) +
  labs(
    title = "Change of Trust in EU"
    #subtitle = "Number of restaurant per city district", 
  ) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(1.2,0.5)
  ) +
  coord_quickmap(xlim = c(-20, 40), ylim = c(30, 72)) 
    #coord_cartesian(xlim = c(-10, 10), ylim = c(35, 75), expand = FALSE)
  #coord_cartesian()

# Plot for 2017
ggplot() +
  geom_polygon(data = spdf_fortified2, aes(fill = poll.2017 , x = long, y = lat, group = group)) +
  #theme_void() +
  #coord_map()

# Plot for 2007
ggplot() +
  geom_polygon(data = spdf_fortified2, aes(fill = poll.2007 , x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()


