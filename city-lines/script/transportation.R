# Author: Jeremy Dong
# Date: 7/26/2017

# Credits: Jonathan Bouchet - City Lines Kaggle Visualization

library(readr)
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(gridExtra)

# Accessing Coordinates

# Function 1: Use Regex to extract Long
# @location - coordinates vector (character)
get_long <- function(location) {
  return(as.numeric(str_extract(location, "-?\\d+\\.?\\d*")))
}

# Function 2: Use Regex to extract Lat
# @location - coordinates vector (character)
get_lat <- function(location) {
  return(as.numeric(str_trim(str_extract(location, "\\s-?\\d+\\.?\\d*"))))
}

# Accessing City + Country info

# Function 1: finds city name using city_id
# @city_id - city id element (integer)
find_city_id <- function(city_id) {
  return(cities$name[which(cities$city_id == city_id)])
}

# Function 2: finds country name using city_id
# @city_id - city id element (integer)
find_country_id <- function(city_id) {
  return(cities$country[which(cities$city_id == city_id)])
}

# Function 3: finds city name using line_id
# @line_id - line id element (integer)
find_city_line <- function(line_id) {
  return(station_lines$city[which(station_lines$line_id == line_id)[1]])
}

# Function 4: finds country name using line_id
# @line_id - line id element (integer)
find_country_line <- function(line_id) {
  return(station_lines$country[which(station_lines$line_id == line_id)[1]])
}

# Function 5: finds line associated with the line_id
# @line_id - line id element (integer)
find_line <- function(line_id) {
  return(station_lines$line[which(station_lines$line_id == line_id)[1]])
}


# Cities + city_id, long, lat
cities <- read_csv("cities.csv", col_types = "icc--c-") %>%
  mutate(long = get_long(coords),
         lat = get_lat(coords),
         coords = NULL) %>%
  rename("city_id" = id)

# plot cities
ggplot(cities, aes(x = long, y = lat, color = country)) + 
  geom_point() + 
  ggtitle("Countries") + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())



# Lines + city, country
lines <- read_csv("r/kaggle/city-lines/data/lines.csv", col_types = "iic--i") %>%
  mutate(city = map_chr(city_id, find_city_id),
         country = map_chr(city_id, find_country_id),
         city_id = NULL) %>%
  rename("line_id" = id,
         "line" = "name")



# Stations                                                                                                                                  # Stations + long, lat
stations <- read_csv("r/kaggle/city-lines/data/stations.csv", col_types = "iicc---") %>%
  mutate(long = get_long(geometry),
         lat = get_lat(geometry),
         geometry = NULL) %>%
  rename("station_id" = id,
         "station" = name)


# Combine Stations + Lines
station_lines <- left_join(stations, lines, by = "line_id") 



# Tracks
tracks <- read_csv("r/kaggle/city-lines/data/tracks.csv", col_types = "iic---i") %>%
  mutate(line = map_chr(tracks$line_id, find_line))

# Regex -> tracks$geometry
# Step 1: extract coords
# Step 2: get (lon, lat) 

# Function 1: get coords from "LINESTRING .... "
# @track - string of coordinate (character)
get_coords <- function(track) {
  return(unlist(str_extract_all(track, "-?\\d+\\.?\\d*\\s-?\\d+\\.?\\d*")))
}

# Function 2: add id, lon, lat, length to vectors
# @id - id of the line (integer)
# @track - string of coordinates (character)
# @dist - length of the track (integer)
add_tracks <- function(id, line_name, track, dist) {
  coords <- get_coords(track)
  longitude <- get_long(coords)
  latitude <- get_lat(coords)
  l <- length(line_id)
  for (i in seq_along(coords)) {
    l <- l + 1
    line_id[l] <<- id
    line[l] <<- line_name
    long[l] <<- longitude[i]
    lat[l] <<- latitude[i]
    length[l] <<- dist
  }
}

# Function 3: iterate through the rows in tracks and add_tracks for each row
# @tracks - tracks df
build_tracks <- function(tracks) {
  for (i in seq_len(nrow(tracks))) {
    add_tracks(tracks$line_id[i], tracks$line[i], tracks$geometry[i], tracks$length[i])
  }
}


# Optimization: inserting values into vectors and then creating a data.frame  
line_id <- integer()
line <- character()
long <- double()
lat <- double()
length <- integer()

build_tracks(tracks)

# Track Coords 
track_coords <- data.frame(line_id, line, long, lat, length) %>%
  mutate(city = map_chr(line_id, find_city_line),
         country = map_chr(line_id, find_country_line))

# df of station coords
sl <- data.frame(line_id = station_lines$line_id, line = station_lines$line, 
                 long = station_lines$long, lat = station_lines$lat,
                 city = station_lines$city, country = station_lines$country)

# df of track coords
tc <- data.frame(line_id = track_coords$line_id, line = track_coords$line, 
                 long = track_coords$long, lat = track_coords$lat,
                 city = track_coords$city, country = track_coords$country)

# Tracks Stations - combines stations and tracks data                 
tracks_stations <- rbind(sl, tc) %>%
  mutate(is_station = c(rep(1, nrow(sl)), rep(0, nrow(tc))),
         line = as.character(line),
         city = as.character(city),
         country = as.character(country))

cities_transportation <- list()
unique_cities <- unique(tracks_stations$city[is.na(tracks_stations$city) == FALSE])

# Builds a graph of each city's transportation system
for (i in seq_along(unique_cities)) {
  city_name <- unique_cities[i]
  city <- tracks_stations %>%
    filter(city == city_name)
  
  cities_transportation[[i]] <- ggplot(city, aes(x = long, y = lat, color = as.factor(line))) +
    geom_point(size = 0.2) +
    ggtitle(city_name) +
    theme_fivethirtyeight() + 
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
          axis.text = element_blank(), axis.title = element_blank())
}

# VISUALIZATION Of EVERY CITY'S TRANSPORTATION LINES
transportation_systems <- do.call(grid.arrange, c(cities_transportation, ncol=5))

# save visualization to a file
pdf("transportation-systems.pdf", width = 15, height = 20)
print(transportation_systems)
dev.off()

# fix london plot


