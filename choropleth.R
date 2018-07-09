
# https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2/

# Possible data souce
# http://data.europa.eu/euodp/en/data/dataset/S2143_88_3_STD88_ENG

install.packages(c("geojsonio", "dplyr", "broom", "ggplot2", "mapproj"))

library(geojsonio)
library(dplyr)
library(broom)
library(ggplot2)
library(mapproj)

spdf <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")

spdf@data

countries_txt <- "Belgium, Greece, Lithuania, Portugal, Bulgaria, Spain, Luxembourg, Romania, Czech Republic, 
France, Hungary, Slovenia, Denmark, Croatia, Malta, Slovakia, Germany, Italy, Netherlands, 
Finland, Estonia, Cyprus, Austria, Sweden, Ireland, Latvia, Poland, United Kingdom"

countries <- trimws(strsplit(countries_txt, ", ")[[1]])

filtered_spdf <- spdf[ spdf@data$name  %in% countries , ]

spdf_fortified <- tidy(filtered_spdf, region = "name")

# Test code
#ggplot() +
#  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group)) +
#  theme_void() +
#  coord_map()

# Read input from csv
data <- read.csv('poll_data.csv', stringsAsFactors = FALSE)
names(data) <- c("country", "poll.2007", "poll.2017")

# Join data together
spdf_fortified2 = spdf_fortified %>%
  left_join(., data, by=c("id"="country")) %>%
  mutate(diff = poll.2017 - poll.2007)

# Plot diff with fancy colours
ggplot() +
  geom_polygon(data = spdf_fortified2, aes(fill = diff, x = long, y = lat, group = group) , 
               size=0, alpha=0.9) +
  theme_void() +
  scale_fill_gradient2(breaks=c(-0.4,-0.2,0,0.1), 
                       name="Change in Trust"
  ) +
  
  labs(
    title = "Trust in EU"#,
    #subtitle = "Number of restaurant per city district", 
  ) +
  theme(
    text = element_text(color = "#22211d"), 
    #plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(1.3,0.5)
  ) +
  coord_map()

# Plot for 2017
ggplot() +
  geom_polygon(data = spdf_fortified2, aes(fill = poll.2017 , x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

# Plot for 2007
ggplot() +
  geom_polygon(data = spdf_fortified2, aes(fill = poll.2007 , x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()


