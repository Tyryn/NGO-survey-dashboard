# Dashboard

## Remember to remove province bits

# Libraries ####
library(shiny)
library(googlesheets)
library(tidyverse)
library(stringr)
library(shinydashboard)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(leaflet)
library(readxl)
library(reshape)
library(scales)
library(timevis)

library(sf)

library(dplyr)
library(raster)



# 1. Move none to the end of the maps
# 2. Add data source and describe what the data is. 
# 3. Remove tick labels 
# 4. Change organization to organisation
# 5. Change title to "Organisation focus areas"
# 6. Change to lollipop chart
# 7. Remove the word cloud
# 8. Send megan the colour scheme. 
# 9. Make map box longer and add in survey info there.
# 10. Include Firdale Consulting in header. Try on the right, with logo. 
# 11. Hoverplot the lollipop plot with the organisation names and website hyperlink.

survey_data <-
  gs_key("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k") %>%
  gs_read(ws = "Survey")

## Get separate geo dataframes, separated by province
SouthAfricanCities <-
  read_excel("SouthAfricanCities.xls")

# Create function for "not in"
`%nin%` = Negate(`%in%`)


myfunc <- function(v1) {
  deparse(substitute(v1))
}


# Clean data ####

# Break up questions with multiple possible answers into chunk and then merge again
# Each question gets its own column, answers comma separated
number_data_vector <-
  as.character(seq(from = 0, to = 2000)) # Setting limit to 2000
survey_data_cut <-
  survey_data[,-c(1:5)] # Done to prevent year or name fields being counted


# Abbreviated dataset that is the first four variables (they always fill the same cells) ####
first_answers <- survey_data[, 1:4]
first_answers$obs <- as.character(1:nrow(survey_data))
#first_answers$obs <- paste("", first_answers$obs)
first_answers <- first_answers %>%
  dplyr::select(obs, everything())
names(first_answers) <-
  c("obs", "ngo_or_donor", "name", "established", "website")


#
# First part of abbreviating datasets ####
#
# Field of operation
field_data_vector <-
  c(
    "Advocacy & Awareness",
    "Culture & Society",
    "Democracy & Civic Rights",
    "Disability & Handicap",
    "Displaced Population & Refugees",
    "Education",
    "Environment",
    "Family Care",
    "Health",
    "Human Rights",
    "Labor",
    "Law & Legal Affairs",
    "Rural Development",
    "Science & Technology",
    "Youth Empowerment",
    "Other"
  )

field_data <- lapply(field_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# Description of organisations falling under "other"
other_description <-
  sapply(survey_data_cut, function(x)
    grep("other_description", x, value = TRUE))
other_description_data_vector <-
  unlist(other_description, use.names = FALSE)

other_description_data <-
  lapply(other_description_data_vector, function(z) {
    Filter(function(x)
      any(x == z), survey_data)
  })

# Region of operation
region_data_vector <-
  c("South Africa", "Southern Africa", "Rest of world")
region_data <- lapply(region_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# # Province of operation
# province_data_vector <-
#   c(
#     "Western Cape",
#     "Northern Cape",
#     "Eastern Cape",
#     "Free State",
#     "North West",
#     "KwaZulu Natal",
#     "Gauteng",
#     "Limpopo",
#     "Mpumalanga"
#   )
# province_data <- lapply(province_data_vector, function(z) {
#   Filter(function(x)
#     any(x == z), survey_data)
# })

# City/municipality of operation
municipality <-
  sapply(survey_data_cut, function(x)
    grep("municipality_", x, value = TRUE))
municipality_data_vector <- unlist(municipality, use.names = FALSE)

municipality_data <- lapply(municipality_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# # Numbers data (Employees and volunteers)
# number_data <- lapply(number_data_vector, function(z) {
#   Filter(function(x)
#     any(x == z), survey_data_cut)
# })

# # Age data
# age <-
#   sapply(survey_data_cut, function(x)
#     grep("age_", x, value = TRUE))
# age_data_vector <- unlist(age, use.names = FALSE)
# age_data <- lapply(age_data_vector, function(z) {
#   Filter(function(x)
#     any(x == z), survey_data)
# })

# # Gender data
# gender_data_vector <- c("male", "female", "na")
# gender_data <- lapply(gender_data_vector, function(z) {
#   Filter(function(x)
#     any(x == z), survey_data_cut)
# })

# Service data
service <-
  sapply(survey_data_cut, function(x)
    grep("service_", x, value = TRUE))
service_data_vector <- unlist(service, use.names = FALSE)

service_data <- lapply(service_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# Priorities data
priorities_data_vector <-
  c(
    "Improving our monitoring & evaluation",
    "Growing our team",
    "Upskilling our team",
    "Securing new equipment/venues",
    "Expanding our organization's scope",
    "Building relationship with funders",
    "Recruiting volunteers"
  )
priorities_data <- lapply(priorities_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})

# M&E data
evaluated_data_vector <- c("Agree", "Neutral", "Disagree")
evaluated_data <- lapply(evaluated_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
pain <-
  sapply(survey_data_cut, function(x)
    grep("pain_", x, value = TRUE))
pain_data_vector <- unlist(pain, use.names = FALSE)

pain_data <- lapply(pain_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

##### Now looping
dataList <-
  list(
    field_data,
    other_description_data,
    region_data,
    # province_data,
    municipality_data,
    # number_data,
    # age_data,
    # gender_data,
    service_data,
    priorities_data,
    evaluated_data,
    pain_data
  )
vectorList <-
  list(
    field_data_vector,
    other_description_data_vector,
    region_data_vector,
    # province_data_vector,
    municipality_data_vector,
    # number_data_vector,
    # age_data_vector,
    # gender_data_vector,
    service_data_vector,
    priorities_data_vector,
    evaluated_data_vector,
    pain_data_vector
  )


list <- mapply(function(z, y) {
  z <-
    lapply(z, cbind, obs = 1:nrow(survey_data_cut))
  z <- z %>%
    reduce(left_join, by = "obs")
  
  z <-
    z[!duplicated(as.list(z))]
  z <- z %>%
    dplyr::select(obs, everything())
  
  shouldChange <- z
  shouldChange[, -1] <-
    sapply(z[,-1], function(x)
      x %nin% y) # Identify cells that need to be made blank
  
  var_list <- names(z)
  var_list <- var_list[-1]    # Remove obs from the list
  
  for (val in var_list) {
    z[, val][shouldChange[, val]] <- NA
  }
  
  # Shifting non-NA cells to the left
  z <-
    as.data.frame(t(apply(z, 1, function(x) {
      return(c(x[!is.na(x)], x[is.na(x)]))
    })))
  emptycols <- sapply(z, function (k)
    all(is.na(k)))
  z <- z[!emptycols]
}, dataList, vectorList)

## Unlist
field_data <- list[[1]]
other_description_data <- list[[2]]
region_data <- list[[3]]
#province_data <- list[[3]]
municipality_data <- list[[4]]
# number_data <- list[[5]]
# age_data <- list[[6]]
# gender_data <- list[[7]]
service_data <- list[[5]]
priorities_data <- list[[6]]
evaluated_data <- list[[7]]
pain_data <- list[[8]]

#### Naming columns
## Field data
names(field_data)[0:ncol(field_data)] <-
  paste("Field", 0:ncol(field_data), sep = "_") # Rename each variable field_stub
names(field_data)[1] <- "obs" # Rename back to obs

## Other description data
names(other_description_data)[1:2] <-
  c("obs", "Other org description")
# Remove the prefix
other_description_data <-
  as.data.frame(lapply(other_description_data, function(x)
    gsub("other_description_", "", x)))


## Region data
names(region_data)[0:ncol(region_data)] <-
  paste("Region", 0:ncol(region_data), sep = "_") # Rename each variable field_stub
names(region_data)[1] <- "obs" # Rename back to obs

# ## Province data
# names(province_data)[0:ncol(province_data)] <-
#   paste("Province", 0:ncol(province_data), sep = "_") # Rename each variable field_stub
# names(province_data)[1] <- "obs" # Rename back to obs

## Municipality data
names(municipality_data)[0:ncol(municipality_data)] <-
  paste("Municipality", 0:ncol(municipality_data), sep = "_") # Rename each variable field_stub
names(municipality_data)[1] <- "obs" # Rename back to obs
# Remove the prefix
municipality_data <-
  as.data.frame(lapply(municipality_data, function(x)
    gsub("municipality_", "", x)))
#
# ## Number data (employees)
# names(number_data)[1:4] <-
#   c("obs",
#     "perm_emp",
#     "temp_emp",
#     "volunteers")
#
# ## Age data
# names(age_data)[1:3] <- c("obs", "min_age", "max_age")
# # Remove the prefix
# age_data <-
#   as.data.frame(lapply(age_data, function(x)
#     gsub("age_", "", x)))
#
# ## Gender data
# names(gender_data)[0:ncol(gender_data)] <-
#   paste("Gender", 0:ncol(gender_data), sep = "_") # Rename each variable field_stub
# names(gender_data)[1] <- "obs" # Rename back to obs

## Service data
names(service_data)[1:2] <- c("obs", "Service")
# Remove the prefix
service_data <-
  as.data.frame(lapply(service_data, function(x)
    gsub("service_", "", x)))

## Priorities data
names(priorities_data)[0:ncol(priorities_data)] <-
  paste("Priority", 0:ncol(priorities_data), sep = "_") # Rename each variable field_stub
names(priorities_data)[1] <- "obs" # Rename back to obs

## Evaluated data
names(evaluated_data)[1:2] <- c("obs", "evaluated")

## Pain data
names(pain_data)[0:ncol(pain_data)] <-
  paste("Pain", 0:ncol(pain_data), sep = "_") # Rename each variable field_stub
names(pain_data)[1] <- "obs" # Rename back to obs
# Remove the prefix
pain_data <-
  as.data.frame(lapply(pain_data, function(x)
    gsub("pain_", "", x)))

### ### ### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
# Merge dataframes into single cleaned dataframe to be used in dashboard ####

dataframes <-
  list(
    first_answers,
    field_data,
    other_description_data,
    region_data,
    # province_data,
    municipality_data,
    # number_data,
    # gender_data,
    # age_data,
    service_data,
    priorities_data,
    evaluated_data,
    pain_data
  )

dataframes <- lapply(dataframes, function(x) {
  x['obs'] = lapply(x['obs'], as.character)
  x                                         # The return is required otherwise it just return the new vector.
})

# Remove whitespace before obs - dataframes after 'first_answers' weirdly have that
dataframes <- lapply(dataframes, function(x) {
  x$obs <- gsub('\\s+', '', x$obs)
  x
})

clean_data <- dataframes %>%
  reduce(left_join, by = "obs")


### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
# Merge in coordinates of place names ####
### ###### ###### ###### ###### ###### ###### ###### ### ###### ###

# Clean SA data so it is only Accent city and coordinates
sa_data <- SouthAfricanCities[, c(2, 4, 5)] %>%
  dplyr::filter(!Latitude == -30.783403)  %>% # Duplicate Pretoria, removing the incorrect coordinate
  distinct(AccentCity, .keep_all = TRUE)


# Remove duplicates
municipalities <- clean_data %>%
  dplyr::select(grep("Municipality", names(clean_data)))

for (i in seq(municipalities)) {
  assign(paste0("municipalities_", i), municipalities[, i])
}

municipList <-
  lapply(ls(pattern = "municipalities_"), function(x)
    get(x))

obs <- 1:nrow(municipalities)
municipCoord <- lapply(municipList, function(x)
  cbind(x, obs))

municipCoord <- lapply(municipCoord, function(x) {
  merge(x,
        sa_data,
        by.x = 1,
        by.y = 1,
        all.x = TRUE)
})


# Need to sort duplicate towns. FOR NOW JUST RANDOMLY DELETING A DUPLICATE :/
# province_merged <- lapply(municipCoord, function(x) {
#   merge(x, province_data, by="obs")
# })


cols <- as.list(as.character(1:ncol(municipalities)))

dfs <-
  Map(function(x, n)
    setNames(x, c(names(x)[c(1, 2)], c(
      paste0("Latitude_", n),
      paste0("Longitude_", n)
    ))),
    municipCoord, cols)

municipCoord <- dfs %>%
  reduce(merge, by = "obs")

municipCoord <-
  distinct(municipCoord, obs, .keep_all = TRUE)   # Just randomly deleting duplicates

municipalities_colnames <- colnames(municipalities)

# Merge back into clean dataset with coordinates
clean_data <-
  clean_data[, -which(names(clean_data) %in% municipalities_colnames)]
clean_data <-
  merge(clean_data, municipCoord, by = "obs", .keep_all = TRUE)


#### Reshape data long
## Slice into x number of dataframes that will be renamed then appended
colnames <- c("obs", "Municipality", "Latitude", "Longitude")

splitMunicip <-
  municipCoord[, grepl("Municipality", names(municipCoord))]
numMunicip <- c(1:ncol(splitMunicip)) %>%
  as.character()

splitMunicip <- lapply(numMunicip, function(x) {
  municipCoord[c(1, grep(x, colnames(municipCoord)))]
})
splitMunicip <- lapply(splitMunicip, setNames, colnames)

coordDF <- bind_rows(splitMunicip)
coordDF <-
  coordDF[rowSums(is.na(coordDF)) == 0,]     # Remove empty rows

### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
# Learner:educator chloropleth data preparation ####
### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
# Shapefile
# bounds <-
#   st_read(
#     "Local_Municipalities_2016.shp"
#   )
# # 
# # Get the municipality average learner:teacher ratio
# school_data <-
#   read.csv(
#     "2018 Masterlist Ordinary schools National Masterlist.csv"
#   )
# school_data <- school_data %>%
#   mutate(Learners_2018 = na_if(Learners_2018, 0)) %>%
#   mutate(Educator_2018 = na_if(Educator_2018, 0)) %>%
#   mutate(ratio = Learners_2018 / Educator_2018) %>%
#   group_by(LMunName) %>%
#   mutate(ave_ratio = median(ratio, na.rm = TRUE)) %>%   # Note that this is the median
#   select(LMunName, ave_ratio) %>%
#   na.omit() %>%
#   distinct(LMunName, .keep_all = TRUE)
# 
# school_data$LMunName <- tolower(school_data$LMunName)
# school_data$LMunName <-
#   gsub("local municipality", "", school_data$LMunName)
# school_data$LMunName <-
#   gsub("metropolitan municipality", "", school_data$LMunName)
# school_data$LMunName <-
#   gsub("municipality", "", school_data$LMunName)
# school_data$LMunName <- trimws(school_data$LMunName)
# colnames(school_data) <- c("Municipality", "Ratio")
# 
# bounds$MUNICNAME <- tolower(bounds$MUNICNAME)
# bounds$MUNICNAME <- gsub("municipality", "", bounds$MUNICNAME)
# bounds$MUNICNAME <- trimws(bounds$MUNICNAME)
# colnames(bounds)[6] <- "Municipality"
# 
# 
# # Merge the two datasets
# mapdata <- left_join(bounds, school_data, by = "Municipality")
# 
# # Create bins
# bins <-
#   c(6, 10, 11, 15, 16, 20, 21, 25, 26, 30, 31, 35, 36, 40)
# pal <- colorBin("YlOrRd", domain = mapdata$Ratio, bins = bins)
# 
# # Add labels
# mapdata$Municipality <-
#   gsub("(?<=\\b)([a-z])",
#        "\\U\\1",
#        tolower(mapdata$Municipality),
#        perl = TRUE)
# mapdata$Ratio <- round(mapdata$Ratio, 1)
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g learners per educator",
#   mapdata$Municipality,
#   mapdata$Ratio
# ) %>% lapply(htmltools::HTML)
# 
# mapdata <- rmapshaper::ms_simplify(mapdata, keep = 0.05, keep_shapes = TRUE)
# 
# # Create the map
# chloro <- leaflet() %>%
#   addTiles() %>%
#   setView(lng = 26.154898,
#           lat = -29.087217,
#           zoom = 6) %>%
#   addPolygons(
#     data = mapdata,
#     weight = 1,
#     fillColor = ~ pal(Ratio),
#     color = "white",
#     fillOpacity = 0.7,
#     # highlight = highlightOptions(
#     #   weight = 5,
#     #   color = "#666",
#     #   dashArray = "",
#     #   fillOpacity = 0.7,
#     #   bringToFront = TRUE),
#     label = labels,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto"
#     )
#   ) %>%
#   addLegend(
#     pal = pal,
#     values = mapdata$Ratio,
#     opacity = 0.7,
#     position = "topright"
#   ) 

### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
  # Community data chloropleth ####
### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
community_cleaned <- read.csv("community_cleaned.csv", header = TRUE) 

# Shape file
bounds <-
  sf::st_read(
    "Local_Municipalities_2016.shp"
  )
bounds$MUNICNAME <- tolower(bounds$MUNICNAME)
bounds$MUNICNAME <- gsub("municipality", "", bounds$MUNICNAME)
bounds$MUNICNAME <- trimws(bounds$MUNICNAME)
colnames(bounds)[6] <- "Municipality"

# Merge datasets
community_map <- left_join(bounds, community_cleaned, by = "Municipality")

# _Crime ####
# Create bins
pal <- colorBin("YlOrRd", domain = community_map$Crime)

# Add labels
community_map$Municipality <-
  gsub("(?<=\\b)([a-z])",
       "\\U\\1",
       tolower(community_map$Municipality),
       perl = TRUE)
labels <- sprintf(
  "<strong>%s</strong><br/>%g / 1000 a victim of crime in 2015",
  community_map$Municipality,
  community_map$Crime
) %>% lapply(htmltools::HTML)

community_map <- rmapshaper::ms_simplify(community_map, keep = 0.05, keep_shapes = TRUE)

# Create the map
crime_chloro <- leaflet() %>%
  addTiles() %>%
  setView(lng = 26.154898,
          lat = -29.087217,
          zoom = 6) %>%
  addPolygons(
    data = community_map,
    weight = 1,
    fillColor = ~ pal(Crime),
    color = "white",
    fillOpacity = 0.7,
    # highlight = highlightOptions(
    #   weight = 5,
    #   color = "#666",
    #   dashArray = "",
    #   fillOpacity = 0.7,
    #   bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = community_map$Crime,
    opacity = 0.7,
    position = "topright"
  )


# _Hospital  ####
pal <- colorBin("Greens", domain = community_map$RateHospital)

# Add labels
community_map$Municipality <-
  gsub("(?<=\\b)([a-z])",
       "\\U\\1",
       tolower(community_map$Municipality),
       perl = TRUE)
labels <- sprintf(
  "<strong>%s</strong><br/>%g average hospital rating (out of 4)",
  community_map$Municipality,
  community_map$RateHospital
) %>% lapply(htmltools::HTML)

# Create the map
RateHospital_chloro <- leaflet() %>%
  addTiles() %>%
  setView(lng = 26.154898,
          lat = -29.087217,
          zoom = 6) %>%
  addPolygons(
    data = community_map,
    weight = 1,
    fillColor = ~ pal(RateHospital),
    color = "white",
    fillOpacity = 0.7,
    # highlight = highlightOptions(
    #   weight = 5,
    #   color = "#666",
    #   dashArray = "",
    #   fillOpacity = 0.7,
    #   bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = community_map$RateHospital,
    opacity = 0.7,
    position = "topright"
  )


# _Toilet ####
pal <- colorBin("Greens", domain = community_map$RateToilet)

# Add labels
community_map$Municipality <-
  gsub("(?<=\\b)([a-z])",
       "\\U\\1",
       tolower(community_map$Municipality),
       perl = TRUE)
labels <- sprintf(
  "<strong>%s</strong><br/>%g average toilet rating (out of 4)",
  community_map$Municipality,
  community_map$RateToilet
) %>% lapply(htmltools::HTML)

# Create the map
RateToilet_chloro <- leaflet() %>%
  addTiles() %>%
  setView(lng = 26.154898,
          lat = -29.087217,
          zoom = 6) %>%
  addPolygons(
    data = community_map,
    weight = 1,
    fillColor = ~ pal(RateToilet),
    color = "white",
    fillOpacity = 0.7,
    # highlight = highlightOptions(
    #   weight = 5,
    #   color = "#666",
    #   dashArray = "",
    #   fillOpacity = 0.7,
    #   bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = community_map$RateToilet,
    opacity = 0.7,
    position = "topright"
  )

# _Water ####
pal <- colorBin("Greens", domain = community_map$RateWater)

# Add labels
community_map$Municipality <-
  gsub("(?<=\\b)([a-z])",
       "\\U\\1",
       tolower(community_map$Municipality),
       perl = TRUE)
labels <- sprintf(
  "<strong>%s</strong><br/>%g average water rating (out of 4)",
  community_map$Municipality,
  community_map$RateWater
) %>% lapply(htmltools::HTML)

# Create the map
RateWater_chloro <- leaflet() %>%
  addTiles() %>%
  setView(lng = 26.154898,
          lat = -29.087217,
          zoom = 6) %>%
  addPolygons(
    data = community_map,
    weight = 1,
    fillColor = ~ pal(RateWater),
    color = "white",
    fillOpacity = 0.7,
    # highlight = highlightOptions(
    #   weight = 5,
    #   color = "#666",
    #   dashArray = "",
    #   fillOpacity = 0.7,
    #   bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = community_map$RateWater,
    opacity = 0.7,
    position = "topright"
  )

RateWater_chloro

# _Police ####
pal <- colorBin("Greens", domain = community_map$RatePolice)

# Add labels
community_map$Municipality <-
  gsub("(?<=\\b)([a-z])",
       "\\U\\1",
       tolower(community_map$Municipality),
       perl = TRUE)
labels <- sprintf(
  "<strong>%s</strong><br/>%g average police rating (out of 4)",
  community_map$Municipality,
  community_map$RatePolice
) %>% lapply(htmltools::HTML)

# Create the map
RatePolice_chloro <- leaflet() %>%
  addTiles() %>%
  setView(lng = 26.154898,
          lat = -29.087217,
          zoom = 6) %>%
  addPolygons(
    data = community_map,
    weight = 1,
    fillColor = ~ pal(RatePolice),
    color = "white",
    fillOpacity = 0.7,
    # highlight = highlightOptions(
    #   weight = 5,
    #   color = "#666",
    #   dashArray = "",
    #   fillOpacity = 0.7,
    #   bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = community_map$RatePolice,
    opacity = 0.7,
    position = "topright"
  )

RatePolice_chloro


# _School ####
pal <- colorBin("Greens", domain = community_map$RateSchool)

# Add labels
community_map$Municipality <-
  gsub("(?<=\\b)([a-z])",
       "\\U\\1",
       tolower(community_map$Municipality),
       perl = TRUE)
labels <- sprintf(
  "<strong>%s</strong><br/>%g average rating of nearest school (out of 4)",
  community_map$Municipality,
  community_map$RateSchool
) %>% lapply(htmltools::HTML)

# Create the map
RateSchool_chloro <- leaflet() %>%
  addTiles() %>%
  setView(lng = 26.154898,
          lat = -29.087217,
          zoom = 6) %>%
  addPolygons(
    data = community_map,
    weight = 1,
    fillColor = ~ pal(RateSchool),
    color = "white",
    fillOpacity = 0.7,
    # highlight = highlightOptions(
    #   weight = 5,
    #   color = "#666",
    #   dashArray = "",
    #   fillOpacity = 0.7,
    #   bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = community_map$RateSchool,
    opacity = 0.7,
    position = "topright"
  )

#RateSchool_chloro

### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
# Dashboard ####
### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###

# Create vector containing all Fields in the dataset
fields_unique <- as.vector(as.matrix(field_data[, -c(1)]))
fields <- unique(fields_unique)
fields <- fields[!is.na(fields)]


# Define UI for application ####
#636466

ui <- dashboardPage(skin = "blue",
  dashboardHeader(
    title = "South African Development Landscape",titleWidth = 450), 
  dashboardSidebar(disable = TRUE),
  dashboardBody(fluidRow(
    tags$style(HTML(".box.box-solid.box-primary>.box-header {
                                color:#FFFFFF;
                    background-color:#636466;}

                    .box.box-solid.box-primary{
                    border-bottom-color:#636466;
                    border-left-color:#636466;
                    border-right-color:#636466;
                    border-top-color:#636466;
                    }
                    .skin-blue .main-header .logo {
                      background-color: #6E876C;
                    }
                    
                    /* logo when hovered */
                      .skin-blue .main-header .logo:hover {
                        background-color: #636466;
                      }
                    
                    /* navbar (rest of the header) */
                      .skin-blue .main-header .navbar {
                        background-color: #636466;
                      }        
                    
                    /* main sidebar */
                      .skin-blue .main-sidebar {
                        background-color: #636466;
                      }
                    
                    /* active selected tab in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                        background-color: #ff0000;
                      }
                    
                    /* other links in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                        background-color: #00ff00;
                          color: #000000;
                      }
                    
                    /* other links in the sidebarmenu when hovered */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                        background-color: #636466;
                      }
                    /* toggle button when hovered  */                    
                      .skin-blue .main-header .navbar .sidebar-toggle:hover{
                        background-color: #636466;
                      }
                    "
                    )),
    box(
      title = "Survey respondent locations",
      status = "primary",
      solidHeader = TRUE,
      width = 12, height = 510,
      collapsible = F,
          leafletOutput(outputId = "map"),
          absolutePanel(
            bottom = 60,
            left = 10,
            box(
              width = 12,
              collapsible = FALSE,
              radioButtons(
                "map_type",
                "See service delivery satifaction, by municipality",
                choices =  c("Access to running water" = "water","Access to sanitation/toilets"=
                               "toilet","Quality of nearest hospital"="hospital","Quality of nearest school"="school",
                             "Quality of local police"=
                               "police", "None"="basic"),
                selected = "basic"
              )
            )
          ), p("Source: Community Survey 2016 (StatsSA)"), 
      p("Survey respondents rated delivery of each service: 1 - No delivery; 
        2 - Poor; 3 - Average; 4 - Good")
    )
  ),
  fluidRow(
    # box(
    #   title = "Word cloud of service provided",
    #   status = "primary",
    #   solidHeader = TRUE,
    #   collapsible = TRUE,
    #   plotOutput("wordcloud", width = "100%")
    # ),
    box(title = "Organisation focus areas", status = "primary", solidHeader = TRUE,
        collapsible = F, width = 12, plotOutput("focus_plot"), 
        absolutePanel(top = 60, right = 20, radioButtons(
          "organization",
          "Type of organisation",
          choices =  c("NGO"="ngo", "Donor"="donor", "Social impact investor"="sia"),
          selected = c("NGO"="ngo")))))
))




# Define server logic ####
server <- function(input, output) {

  # Map
  selectedData <- reactive({
    selectedData <- clean_data 
  })
  
  info_data <- reactive({
    selectedData()[, c(1, 3:5)]
  })
  
  coordDF_1 <- reactive({
    merge(coordDF, info_data())
  })
  
  info <- reactive({
    paste(
      sep = "<br/>",
      paste0(
        "<b><a href='",
        coordDF_1()$website,
        "'>",
        coordDF_1()$name,
        "</a></b>"
      ),
      paste("Est.", coordDF_1()$established)
    )
  })
  
  observeEvent(input$map_type, {
    if(input$map_type=="basic"){
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          lng = as.numeric(coordDF_1()$Longitude),
          lat = as.numeric(coordDF_1()$Latitude),
          popup = info()
        )
    })
    }
  })

  
  
  
  
  
  
  
  # # # Chloropleth ####
  selectedData_2 <- reactive({
    selectedData_2 <- clean_data
  })

  info_data_2 <- reactive({
    selectedData_2()[, c(1, 3:5)]
  })

  coordDF_2 <- reactive({
    merge(coordDF, info_data_2())
  })

  info_2 <- reactive({
    paste(
      sep = "<br/>",
      paste0(
        "<b><a href='",
        coordDF_2()$website,
        "'>",
        coordDF_2()$name,
        "</a></b>"
      ),
      paste("Est.", coordDF_2()$established)
    )
  })

  observeEvent(input$map_type,{
    if(input$map_type=="crime"){
  output$map <- renderLeaflet({
  crime_chloro %>%
    addMarkers(
        lng = as.numeric(coordDF_2()$Longitude),
        lat = as.numeric(coordDF_2()$Latitude),
        popup = info_2()
      )
  })
    }
    if(input$map_type=="hospital"){
      output$map <- renderLeaflet({
        RateHospital_chloro %>%
          addMarkers(
            lng = as.numeric(coordDF_2()$Longitude),
            lat = as.numeric(coordDF_2()$Latitude),
            popup = info_2()
          )
      })
    }
    if(input$map_type=="toilet"){
      output$map <- renderLeaflet({
        RateToilet_chloro %>%
          addMarkers(
            lng = as.numeric(coordDF_2()$Longitude),
            lat = as.numeric(coordDF_2()$Latitude),
            popup = info_2()
          )
      })
    }
    if(input$map_type=="water"){
      output$map <- renderLeaflet({
        RateWater_chloro %>%
          addMarkers(
            lng = as.numeric(coordDF_2()$Longitude),
            lat = as.numeric(coordDF_2()$Latitude),
            popup = info_2()
          )
      })
    }
    if(input$map_type=="police"){
      output$map <- renderLeaflet({
        RatePolice_chloro %>%
          addMarkers(
            lng = as.numeric(coordDF_2()$Longitude),
            lat = as.numeric(coordDF_2()$Latitude),
            popup = info_2()
          )
      })
    }
    if(input$map_type=="school"){
      output$map <- renderLeaflet({
        RateSchool_chloro %>%
          addMarkers(
            lng = as.numeric(coordDF_2()$Longitude),
            lat = as.numeric(coordDF_2()$Latitude),
            popup = info_2()
          )
      })
    }
  })

  # Wordcloud ####
  output$wordcloud <- renderPlot({
    docs <- Corpus(VectorSource(clean_data$Service))
    
    # Cleaning the text
    docs <-
      tm_map(docs, content_transformer(tolower))    # To lower case
    # docs <- tm_map(docs, removeNumber)  # Remove numbers
    docs <-
      tm_map(docs, removeWords, stopwords("english")) # Remove common English words
    docs <-
      tm_map(docs, removeWords, c("help", "assist"))  # Remove words common and useless words to this context
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    
    # Build term-document matrix
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    head(d, 10)
    
    # Build word cloud
    wordcloud(
      words = d$word,
      freq = d$freq,
      min.freq = 1,
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8, "RdBu")
    )
  })
  
# Focus area plot ####
  # Filter data
  selectedData_3 <- reactive({
    clean_data %>%
      filter(ngo_or_donor==input$organization)
  })

  
  # Shape data 
  focus_data <- reactive({
    focus_data <- selectedData_3() %>%
      dplyr::select(obs, grep("Field", names(selectedData_3()))) %>%
      gather(., Column, Focus, 2:ncol(.)) %>%
      na.omit() %>%
      dplyr::select(Focus) 
    
    focus_data <- as.data.frame(table(focus_data$Focus))
    focus_data <-  mutate(focus_data, pct = focus_data$Freq)
    focus_data
  })
  
  
  output$focus_plot <- renderPlot({
    validate(need(nrow(selectedData_3()) > 0, "No data for this selection"))
    
    max_prop <- round((max(focus_data()$pct) + 15)) 
    
    
    plot <- ggplot(focus_data(), aes(x=Var1, y=pct)) +
      geom_segment(aes(x=Var1, xend=Var1, y=0, yend=pct), color="#6E876C") +
      geom_point(color="#6E876C") + theme_light() + scale_x_discrete("") +
      coord_flip() + scale_y_continuous("Number of organisations", limits = c(0,5)) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    
    
    
   # plot <-  ggplot(focus_data(), aes(Var1, pct)) + geom_bar(stat = 'identity', fill="#6E876C") +
   #    scale_y_continuous("Number of organisations", limits = c(0,5)) + 
   #    scale_x_discrete("") +
   #    geom_text(
   #      aes(label = pct),
   #      vjust = 1.6,
   #      color = "black",
   #      size = 3.5
   #    ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   #              panel.background = element_blank(), axis.text.x = element_text(angle = 45),
   #              axis.text.y = element_blank())
   plot
  })
}



# Run the application
shinyApp(ui = ui, server = server)

