# Dashboard

library(shiny)
library(googlesheets)
library(tidyverse)
library(stringr)

survey_data <- gs_key("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k") %>%
  gs_read(ws = "Survey")


# Clean data

# Break up questions with multiple possible answers into chunk and then merge again 
# Each question gets its own column, answers comma separated


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
  Filter(function(x) any(x==z), survey_data)
})            
field <- Filter(function(x) ncol(x)[1] > 0, field_data)

field_data <- lapply(field_data, cbind, obs = 1:nrow(survey_data))

field_data <- field_data %>%
  reduce(left_join, by = "obs")

field_data <- field_data[!duplicated(as.list(field_data))]  # Remove redundant columns


## NEED TO FIGURE OUT WAY TO REPLACE ALL NON-FIELD ENTRIES WITH BLANKS
lapply(fields, function(z){
  field_data[field_data != z] <- ""
}
  )












# Define UI for application 
ui <- fluidPage(
  
)

# Define server logic 
server <- function(input, output) {
  
  
  
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

