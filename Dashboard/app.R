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
  numbers <- as.character(seq(from = 0, to = 2000)) # Setting limit to 2000
  survey_data_cut <- survey_data[, -c(1:5)] # Done to prevent year or name fields being counted
  
  
  # Look at looping 
  
  ###################################################################################
  # Abbreviated dataset that is the first four variables (they always fill the same cells)
  first_answers <- survey_data[, 1:3]
  first_answers$obs <- 1:nrow(survey_data)
  first_answers <- first_answers %>%
    select(obs, everything())   
  names(first_answers) <- c("obs", "ngo_or_donor", "name", "established")
  
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
    field_data[, val][shouldChange[, val]] <- NA
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
  
  # Added the prefix "municipality_" to all of these entries. Use the same logic except with a pattern match
  municipality <- sapply(survey_data_cut, function(x) grep("municipality_", x, value = TRUE))
  municipality <- unlist(municipality, use.names = FALSE)
  
  municipality_data <- lapply(municipality, function(z) {
    Filter(function(x)
      any(x == z), survey_data)
  })
  municipality_data <- lapply(municipality_data, cbind, obs = 1:nrow(survey_data))
  municipality_data <- municipality_data %>%
    reduce(left_join, by = "obs")
  municipality_data <-
    municipality_data[!duplicated(as.list(municipality_data))]
  municipality_data <- municipality_data %>%
    select(obs, everything())  
  shouldChange <- municipality_data
  shouldChange[,-1] <-
    sapply(municipality_data[, -1], function(x)
      x %nin% municipality) # Identify cells that need to be made blank
  var_list <- names(municipality_data)
  var_list <- var_list[-1]    # Remove obs from the list
  
  for (val in var_list) {
    municipality_data[, val][shouldChange[, val]] <- NA
  }
  
  # Shifting non-NA cells to the left
  municipality_data <- as.data.frame(t(apply(municipality_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
  emptycols <- sapply(municipality_data, function (k) all(is.na(k)))
  municipality_data <- municipality_data[!emptycols]
  
  # Naming columns
  names(municipality_data)[0:ncol(municipality_data)] <-
    paste("Municipality", 0:ncol(municipality_data), sep = "_") # Rename each variable field_stub
  names(municipality_data)[1] <- "obs" # Rename back to obs

# Remove the prefix
municipality_data<- as.data.frame(lapply(municipality_data, function(x) gsub("municipality_", "", x)))


###############################################################################
# Abbreviated dataset containing only permanent employees, temp employees, 
# volunteers and targetted age range

number_data <- lapply(numbers, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
number_data <- lapply(number_data, cbind, obs = 1:nrow(survey_data_cut))
number_data <- number_data %>%
  reduce(left_join, by = "obs")
number_data <-
  number_data[!duplicated(as.list(number_data))]
number_data <- number_data %>%
  select(obs, everything())  
shouldChange <- number_data
shouldChange[,-1] <-
  sapply(number_data[, -1], function(x)
    x %nin% numbers) # Identify cells that need to be made blank

var_list <- names(number_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  number_data[, val][shouldChange[, val]] <- NA
}
# Shifting non-NA cells to the left
number_data <- as.data.frame(t(apply(number_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
emptycols <- sapply(number_data, function (k) all(is.na(k)))
number_data <- number_data[!emptycols]

# Name employee columns
names(number_data)[1:6] <- c("obs", "perm_emp", "temp_emp", "volunteers", "age_min",
                             "age_max")

#####################################################################################
# Abbreviated dataset containing only gender information

genders <- c("male", "female", "na")

gender_data <- lapply(genders, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
gender_data <- lapply(gender_data, cbind, obs = 1:nrow(survey_data_cut))
gender_data <- gender_data %>%
  reduce(left_join, by = "obs")
gender_data <-
  gender_data[!duplicated(as.list(gender_data))]
gender_data <- gender_data %>%
  select(obs, everything())  
shouldChange <- gender_data
shouldChange[,-1] <-
  sapply(gender_data[, -1], function(x)
    x %nin% genders) # Identify cells that need to be made blank

var_list <- names(gender_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  gender_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
gender_data <- as.data.frame(t(apply(gender_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
emptycols <- sapply(gender_data, function (k) all(is.na(k)))
gender_data <- gender_data[!emptycols]

# Naming columns
names(gender_data)[0:ncol(gender_data)] <-
  paste("Gender", 0:ncol(gender_data), sep = "_") # Rename each variable field_stub
names(gender_data)[1] <- "obs" # Rename back to obs


#####################################################################################
# Abbreviated dataset containing only service data

# Doing it the same way as the municipality data

service <- sapply(survey_data_cut, function(x) grep("service_", x, value = TRUE))
service <- unlist(service, use.names = FALSE)

service_data <- lapply(service, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
service_data <- lapply(service_data, cbind, obs = 1:nrow(survey_data))
service_data <- service_data %>%
  reduce(left_join, by = "obs")
service_data <-
  service_data[!duplicated(as.list(service_data))]
service_data <- service_data %>%
  select(obs, everything())  
shouldChange <- service_data
shouldChange[,-1] <-
  sapply(service_data[, -1], function(x)
    x %nin% service) # Identify cells that need to be made blank
var_list <- names(service_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  service_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
service_data <- as.data.frame(t(apply(service_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
emptycols <- sapply(service_data, function (k) all(is.na(k)))
service_data <- service_data[!emptycols]

# Naming columns
names(service_data)[1:2] <- c("obs", "Service")

# Remove the prefix
service_data<- as.data.frame(lapply(service_data, function(x) gsub("service_", "", x)))


#####################################################################################
# Abbreviated dataset containing only priorities information

priorities <-
  c(
    "Improving our monitoring & evaluation" = "monitoring",
    "Growing our team" = "team_growth",
    "Upskilling our team" = "upskilling",
    "Securing new equipment/venues" = "facilities",
    "Expanding our organization's scope" = "scope",
    "Building relationship with funders" = "funders",
    "Recruiting volunteers" = "volunteers"
  )

priorities_data <- lapply(priorities, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
priorities_data <- lapply(priorities_data, cbind, obs = 1:nrow(survey_data_cut))
priorities_data <- priorities_data %>%
  reduce(left_join, by = "obs")
priorities_data <-
  priorities_data[!duplicated(as.list(priorities_data))]
priorities_data <- priorities_data %>%
  select(obs, everything())  
shouldChange <- priorities_data
shouldChange[,-1] <-
  sapply(priorities_data[, -1], function(x)
    x %nin% priorities) # Identify cells that need to be made blank

var_list <- names(priorities_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  priorities_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
priorities_data <- as.data.frame(t(apply(priorities_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
emptycols <- sapply(priorities_data, function (k) all(is.na(k)))
priorities_data <- priorities_data[!emptycols]

# Naming columns
names(priorities_data)[0:ncol(priorities_data)] <-
  paste("Priority", 0:ncol(priorities_data), sep = "_") # Rename each variable field_stub
names(priorities_data)[1] <- "obs" # Rename back to obs


#####################################################################################
# Abbreviated dataset containing only evaluated information

evaluated <- c("Yes", "No")

evaluated_data <- lapply(evaluated, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
evaluated_data <- lapply(evaluated_data, cbind, obs = 1:nrow(survey_data_cut))
evaluated_data <- evaluated_data %>%
  reduce(left_join, by = "obs")
evaluated_data <-
  evaluated_data[!duplicated(as.list(evaluated_data))]
evaluated_data <- evaluated_data %>%
  select(obs, everything())  
shouldChange <- evaluated_data
shouldChange[,-1] <-
  sapply(evaluated_data[, -1], function(x)
    x %nin% evaluated) # Identify cells that need to be made blank

var_list <- names(evaluated_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  evaluated_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
evaluated_data <- as.data.frame(t(apply(evaluated_data,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
emptycols <- sapply(evaluated_data, function (k) all(is.na(k)))
evaluated_data <- evaluated_data[!emptycols]

# Naming columns
names(evaluated_data)[1:2] <- c("obs", "evaluated")



#######################################################################################
# Merge dataframes into single cleaned dataframe to be used in dashboard

dataframes <- list(first_answers, field_data, region_data, province_data, municipality_data,
                   number_data, gender_data, service_data, priorities_data, evaluated_data)

dataframes = lapply(dataframes, function(x){
 x['obs'] = lapply(x['obs'], factor)
 x
})
clean_data <- dataframes %>%
  reduce(left_join, by = "obs")















#######################################################################################


# Define UI for application
ui <- fluidPage()

# Define server logic
server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)
