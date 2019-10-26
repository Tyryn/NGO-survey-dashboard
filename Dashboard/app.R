# Dashboard

library(shiny)
library(googlesheets)
library(tidyverse)
library(stringr)

survey_data <-
  gs_key("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k") %>%
  gs_read(ws = "Survey")


# Clean data

# Break up questions with multiple possible answers into chunk and then merge again
# Each question gets its own column, answers comma separated


# NEED TO FIGURE OUT WAY TO KEEP CLIENTS AN ID 
###################################################################################
# Abbreviated dataset that is the first four variables (they always fill the same cells)
first_answers <- survey_data[, 1:3]
first_answers$obs <- 1:nrow(survey_data)

####################################################################################
# Abbreviated dataset containing only field of operation information
fields <-
  c(
    "advocacy",
    "agriculture",
    "business",
    "child_educ",
    "youth_empowerment",
    "citizenship",
    "communication",
    "conflict_resolution",
    "peace",
    "ict",
    "culture_society",
    "democracy_civic",
    "rural_dev",
    "disability",
    "refugees",
    "education",
    "environment",
    "fam_care",
    "womens_rights",
    "governance",
    "health",
    "human_rights",
    "charity",
    "labor",
    "law_legal",
    "migrant_workers",
    "relief",
    "reconstruction",
    "rehabilitation",
    "research_studies",
    "science",
    "social_media",
    "technology",
    "transparency",
    "training",
    "building"
  )

field_data <- lapply(fields, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
# field <- Filter(function(x)
#   ncol(x)[1] > 0, field_data)

field_data <- lapply(field_data, cbind, obs = 1:nrow(survey_data)) # Add "obs" identifier

field_data <- field_data %>%
  reduce(left_join, by = "obs")

field_data <-
  field_data[!duplicated(as.list(field_data))]  # Remove redundant columns

field_data <- field_data %>%
  select(obs, everything())      # Want to move obs to front because want loop to not apply to obs

`%nin%` = Negate(`%in%`)  # Create function for "not in"
shouldChange <- field_data
shouldChange[,-1] <-
  sapply(field_data[, -1], function(x)
    x %nin% fields) # Identify cells that need to be made blank

var_list <- names(field_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  field_data[, val][shouldChange[, val]] <- ""
}

names(field_data)[0:ncol(field_data)] <-
  paste("Field", 0:ncol(field_data), sep = "_") # Rename each variable field_stub
names(field_data)[1] <- "obs" # Rename back to obs

#############################################################################
# Abbreviated dataset containing only region of operation information
# Same method as before
regions <- c("south_africa", "southern_africa", "row")
region_data <- lapply(regions, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
region_data <- lapply(region_data, cbind, obs = 1:nrow(survey_data))
region_data <- region_data %>%
  reduce(left_join, by = "obs")
region_data <-
  region_data[!duplicated(as.list(region_data))]
region_data <- region_data %>%
  select(obs, everything())  
shouldChange <- region_data
shouldChange[,-1] <-
  sapply(region_data[, -1], function(x)
    x %nin% regions) # Identify cells that need to be made blank
var_list <- names(region_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  region_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left and remove empty column
region_data <- as.data.frame(t(apply(region_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
emptycols <- sapply(region_data, function (k) all(is.na(k)))
region_data <- region_data[!emptycols]

# Naming columns
names(region_data)[0:ncol(region_data)] <-
  paste("Region", 0:ncol(region_data), sep = "_") # Rename each variable field_stub
names(region_data)[1] <- "obs" # Rename back to obs

###############################################################################
# Abbreviated dataset containing only province information
provinces <- c("Western Cape", "Northern Cape", "Eastern Cape", "Free State", "North West",
              "KwaZulu Natal", "Gauteng", "Limpopo", "Mpumalanga")

province_data <- lapply(provinces, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
province_data <- lapply(province_data, cbind, obs = 1:nrow(survey_data))
province_data <- province_data %>%
  reduce(left_join, by = "obs")
province_data <-
  province_data[!duplicated(as.list(province_data))]
province_data <- province_data %>%
  select(obs, everything())  
shouldChange <- province_data
shouldChange[,-1] <-
  sapply(province_data[, -1], function(x)
    x %nin% provinces) # Identify cells that need to be made blank
var_list <- names(province_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  province_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
province_data <- as.data.frame(t(apply(province_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
emptycols <- sapply(province_data, function (k) all(is.na(k)))
province_data <- province_data[!emptycols]

# Naming columns
names(province_data)[0:ncol(province_data)] <-
  paste("Province", 0:ncol(province_data), sep = "_") # Rename each variable field_stub
names(province_data)[1] <- "obs" # Rename back to obs


###############################################################################
# Abbreviated dataset containing only city/municipality information

# Need to figure this one out!







# Define UI for application
ui <- fluidPage()

# Define server logic
server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)
