###################
# CREATE PUDS MAP #
# Mao Hu          #
# 10/27/2016      #
###################

# load libraries
library(maptools)
library(ggplot2)
library(ggmap)
library(rgdal)
library(readr)
library(broom)
library(dplyr)
library(leaflet)
library(htmlwidgets)

# load data
# assume that current working directory has the appropriate data file
directory <- getwd()
puds <- readOGR(dsn=paste0(directory, "/data/Planned_Unit_Development_PUDs"), layer="Planned_Unit_Development_PUDs")

# convert the shapefile to a data frame
puds_data <- as(puds, "data.frame")

#--- leaflet maps ----

# a function formatting the HTML for the popups
formatPopup <- function(name, zoning, ward){
  paste(sep="<br/>",
   paste0("<b>", name, "</b>"),
   paste0("Zoning: ", zoning),
   ward
  )
}

# edit the PUDS data to classify PUD zoning types
puds_data <- mutate(puds_data, 
                    zoning_type = ifelse(grepl(',', PUD_ZONING), "Other/Multiple Types", 
                                         ifelse(substring(PUD_ZONING,1,1)=='C', "Commercial",
                                          ifelse(substring(PUD_ZONING,1,1)=='R', "Residential",
                                           "Other/Multiple Types")
                                          ) )
                    ) 
# order this factor
puds_data$zoning_type <- factor(puds_data$zoning_type, levels=c("Commercial", "Residential", "Other/Multiple Types"))

# define a palette
pal <- colorFactor(palette=c("red", "blue", "green"),
                   domain = c("Commercial", "Residential", "Other/Multiple Types")
                   )

# attribution string, not used
attribution = "by Mao Hu, using the <a href='https://rstudio.github.io/leaflet/'>leaflet</a> package for R </br> Data from <a href='http://opendata.dc.gov/datasets/bdfca0a8c4174bfab5fd1898c8860cc8_9'>DC Open Data</a>."

# create the map
map <- leaflet(data = puds) %>% 
  setView(lat=38.889, lng=-77.009, zoom=12) %>%
  #addTiles(options = tileOptions(opacity = 0.75)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  #addProviderTiles("OpenMapSurfer.Grayscale") %>%
  #addControl(html=attribution, position="bottomleft") %>%
  addPolygons(stroke = TRUE, weight=2, color="black", 
              fillOpacity = 0.5, 
              fillColor = ~pal(puds_data$zoning_type),
              popup = formatPopup(puds_data$PUD_NAME, puds_data$PUD_ZONING, puds_data$WARD)
              ) %>%
  addLegend("bottomright", pal=pal, values = puds_data$zoning_type, title="Zoning Type of Planned Unit Development", opacity=0.75)

# display the map
map

# save to HTML
saveWidget(map, file="PUDS_map_final.html")

