
## BIKETOWN data viz for 2018 Cascadia R Conference cRaggy graphics show & tell

# Pierrette Lo ~ pierrette_lo@hotmail.com ~ github.com/lopierra


library(tidyverse)
library(lubridate)
library(ggmap)


# Download, unzip, and read in data with correct formatting for columns (need `format` for date/time or it doesn't work)

url <- "https://s3.amazonaws.com/biketown-tripdata-public/BiketownPublicTripData201804.zip"

download.file(url, dest = "dataset.zip", mode = "wb")

unzip("dataset.zip")

public_trip_data <- list.files(path = "PublicTripData",
                               pattern = "*.csv",
                               full.names = T) %>%
  map_df(~ read_csv(., col_types = list(
    RouteID = col_integer(),
    PaymentPlan = col_character(),
    StartHub = col_character(),
    StartLatitude = col_double(),
    StartLongitude = col_double(),
    StartDate = col_date(format = "%m/%d/%Y"),
    StartTime = col_time(format = "%R"),
    EndHub = col_character(),
    EndLatitude = col_double(),
    EndLongitude = col_double(),
    EndDate = col_date(format = "%m/%d/%Y"),
    EndTime = col_time(format = "%R"),
    TripType = col_character(),
    BikeID = col_integer(),
    BikeName = col_character(),
    Distance_Miles = col_double(),
    Duration = col_time(format = "%T"),
    RentalAccessPath = col_character(),
    MultipleRental = col_logical()
  )))


# Subset trips where a bike was not returned to a hub
# Get unique coordinate sets and number of rides ending at each
# Filter out 4477 rides where end lat/long was NA

nohub_end <- public_trip_data %>% 
  filter(is.na(EndHub)) %>% 
  group_by(EndLatitude, EndLongitude) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(EndLatitude)) %>% 
  head(200)


# Get all unique end hub locations

hub_locations <- public_trip_data %>% 
  filter(!is.na(EndHub)) %>% 
  distinct(EndHub, .keep_all = TRUE) %>% 
  select(EndHub, EndLatitude, EndLongitude)


# Portland center coordinates were obtained from Google
# Use to get map of Portland

portland <- c(lon = -122.6587, lat = 45.5222)
portland_map <- get_map(location = portland, zoom = 13, scale = 1, source = "stamen", maptype = "toner-lite")


# Plot rides ending outside hubs and hub locations on map of Portland 

ggmap(portland_map) +
  geom_point(aes(EndLongitude, EndLatitude, size = n), 
             data = nohub_end, 
             alpha = 0.5, 
             color = "#005147") +
  geom_point(aes(EndLongitude, EndLatitude), 
             data = hub_locations, 
             color = "#FF6600", 
             shape = 8, 
             size = 1.5) +
  scale_size_continuous(name = "Ride count", 
                        range = c(2, 10), 
                        breaks = c(250, 500, 750, 1000), 
                        labels = c(250, 500, 750, 1000)) +
  ggtitle("Locations and numbers of rides ending outside a BIKETOWN hub", 
          subtitle = "\u2731 indicates hub locations\n") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "#FF6600", hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.95, 0.08)) +
  ylab(NULL) +
  xlab(NULL) +
  labs(caption = "Created by Pierrette Lo\nCode: https://github.com/lopierra/biketown")

ggsave("nohub.png", height = 11, width = 8.5, units = "in", dpi = 600) 


## Future directions: 
# Join with data from Google Maps & Google Places using package `googleways`
# Join with Portland Maps Open Data (https://gis-pdx.opendata.arcgis.com/) using packages `tmap` and `sp`








