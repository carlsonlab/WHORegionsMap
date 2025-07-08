
library(tidyverse)
library(sf)
library(rnaturalearth)

# 1. Download data

who <- read_csv("Source/WHO-COVID-19-global-daily-data.csv")

who <- who %>% 
  select(Country, Country_code, WHO_region) %>%
  distinct()

map <- rnaturalearth::ne_download(type = "map_units",
                                  category = "cultural",
                                  destdir = "Source",
                                  scale = 10)

# 2. Match any WHO countries not in the shapefile

who %>%
  filter(Country_code %in% setdiff(who$Country_code, map$ISO_A2_EH))

# Ignore international conveyances (this is case data from early in the Covid-19 pandemic)
# Namibia is throwing an error because... "NA"

who$Country_code[who$Country=="Namibia"] <- "NAMIBIA"
map$ISO_A2_EH[map$SOVEREIGNT=="Namibia"] <- "NAMIBIA"

who <- who %>% 
  rename("ISO_A2_EH" = "Country_code")

map <- map %>% 
  select(SOVEREIGNT, GEOUNIT, ISO_A2_EH) %>%
  left_join(who, by = "ISO_A2_EH") 

# 2. Categorize any countries not assigned to a WHO region

map %>%
  filter(is.na(WHO_region)) %>% View()

euro <- c('Dhekelia Sovereign Base Area', 'Northern Cyprus', 'Cyprus No Mans Area', 'Akrotiri Sovereign Base Area', 'Aland', 'Svalbard')
map$WHO_region[map$GEOUNIT %in% euro] <- "EUR"

emro <- c('UNDOF', 'Somaliland', 'Western Sahara', 'Bir Tawil')
map$WHO_region[map$GEOUNIT %in% emro] <- "EMR"

searo <- c('Korean Demilitarized Zone (north)')
map$WHO_region[map$GEOUNIT %in% searo] <- "SEAR"

wpro <- c('Korean Demilitarized Zone (south)', 'Hong Kong S.A.R.', 'Macao S.A.R')
map$WHO_region[map$GEOUNIT %in% wpro] <- "WPR"

amro <- c('US Naval Base Guantanamo Bay', 'Southern Patagonian Ice Field', 'Bajo Nuevo Bank (Petrel Is.)', 'Serranilla Bank', 'Scarborough Reef')
map$WHO_region[map$GEOUNIT %in% amro] <- "AMR"

map %>%
  filter(is.na(WHO_region)) %>% View()

# 3. Plot the results for a spotcheck

ggplot(data = map) +
  geom_sf(aes(fill = WHO_region))

sf::write_sf(map, 'Output/WHORegions.shp')
