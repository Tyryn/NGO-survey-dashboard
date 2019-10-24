# Survey

library(shiny)
library(shinydashboard)
library("googlesheets")
library("DT")
library(readxl)

# Get shiny token to access google drive
# shiny_token <- gs_auth()
# saveRDS(shiny_token, "shiny_app_token.rds")


## Google Sheet has been created. This is the sh
sheetkey <- "1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k"
Data <- gs_key(sheetkey)

city_names <- read_excel("SouthAfricanCities.xls")
fields <-
  c(
    "Advocacy & Awareness" = "advocacy",
    "Agriculture" = "agriculture",
    "Business & Economic Policy" = "business",
    "Child Education" = "child_educ",
    "Youth Empowerment" = "youth_empowerment",
    " Citizenship" = "citizenship",
    "Communication" = "communication",
    "Conflict Resolution" = "conflict_resolution",
    "Peace Building" = "peace",
    "ICT" = "ict",
    "Culture & Society" = "culture_society",
    "Democracy & Civic Rights" = "democracy_civic",
    "Rural Development" = "rural_dev",
    "Disability & Handicap" = "disability",
    "Displaced Population & Refugees" = "refugees",
    "Education" = "education",
    "Environment" = "environment",
    "Family Care" = "fam_care",
    "Women’s Rights" = "womens_rights",
    "Governance" = "governance",
    "Health" = "health",
    "Human Rights" = "human_rights",
    "Charity/Philanthropy" = "charity",
    "Labor" = "labor",
    "Law & Legal Affairs" = "law_legal",
    "Migrant Workers" = "migrant_workers",
    "Relief" = "relief",
    "Reconstruction" = "reconstruction",
    "Rehabilitation" = "rehabilitation",
    "Research & Studies" = "research_studies",
    "Science" = "science",
    "Social Media" = "social_media",
    "Technology" = "technology",
    "Transparency" = "transparency",
    "Training & Capacity" = "training",
    "Building" = "building"
  )

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

# UI
ui <- fluidPage(fluidRow(column(12, offset = 5, titlePanel("Survey"))),
                fluidRow(column(
                  12,
                  fluidRow(column(
                    8,
                    offset = 2,
                    wellPanel(
                      fluidRow(selectInput(
                        "ngo_or_donor",
                        "1. NGO or Donor?",
                        choices = c("NGO" = "ngo", "Donor" = "donor")
                      )),
                      fluidRow(textInput("name", "2. Name of organization")),
                      fluidRow(
                        selectInput(
                          "established",
                          "3. Year established",
                          choices = 1900:as.numeric(format(Sys.Date(), "%Y")),
                          selected = 2010
                        )
                      ),
                      fluidRow(
                        conditionalPanel(
                          condition = "input.ngo_or_donor == 'ngo'",
                          selectInput(
                            "field",
                            "4. Field of work",
                            choices = fields,
                            multiple = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        conditionalPanel(
                          condition = "input.ngo_or_donor == 'ngo'",
                          checkboxGroupInput(
                            "country", "5. Regions of operation", 
                            choices = c("South Africa"="south_africa", "Southern Africa"="southern_africa",
                                        "Rest of world"="row"),
                            selected = "south_africa"
                          )
                        )
                      ),
                      fluidRow(
                        conditionalPanel(
                          condition = "input.ngo_or_donor == 'ngo' && input.country && 
                          input.country.indexOf('south_africa') > -1",  ## Necessary javascript expression
                          selectInput(
                            "province",
                            "6. South African provinces where your organization operates",
                            c("All provinces" = "",
                              city_names$ProvinceName),
                            multiple = TRUE
                          )
                        )
                      ),
                      fluidRow(
                        conditionalPanel(
                          condition = "input.ngo_or_donor == 'ngo' && input.country && 
                          input.country.indexOf('south_africa') > -1",
                          selectInput(
                            "municipality",
                            "7. (Nearest) South African village, town, or city where your organization operates",
                            city_names$AccentCity
                          ),
                          multiple = TRUE
                        )
                      ),
                      fluidRow(
                        conditionalPanel(
                          condition = "input.ngo_or_donor == 'ngo' && input.country && 
                          input.country.indexOf('south_africa') > -1",
                          numericInput("perm_employees", "8. Number of permanent employees", 10, min = 0),
                          numericInput("temp_employees", "9. Number of temporary employees", 10, min = 0),
                          numericInput("volunteers", "10. Number of volunteers", 10, min = 0),
                          sliderInput(
                            "target_age",
                            "11. Targetted age range",
                            min = 0,
                            max = 100,
                            value = c(0, 20)
                          ),
                          selectInput(
                            "target_gender",
                            "12. Targetted gender",
                            choices = c("Male" = "male", "Female" = "female"),
                            multiple = TRUE
                          ),
                          textInput(
                            "service",
                            "13. Please provide a description of what your organization does"
                          ),
                          selectInput("priorities", "14. Next year, we are prioritising:", choices = priorities),
                          radioButtons(
                            "evaluated",
                            "15. Has your organization ever been evaluated?",
                            choices = c("Yes", "No")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.ngo_or_donor == 'ngo' && input.country && 
                          input.country.indexOf('south_africa') > -1",
                          selectInput(
                            "municipality",
                            "7. (Nearest) South African village, town, or city where your organization operates",
                            city_names$AccentCity
                          ),
                          multiple = TRUE
                        )
                    ),
                    fluidRow(
                      conditionalPanel(
                        condition = "input.ngo_or_donor == 'ngo' && input.country && 
                          input.country.indexOf('south_africa') < 0",
                        numericInput("perm_employees", "6. Number of permanent employees", 10, min = 0),
                        numericInput("temp_employees", "7. Number of temporary employees", 10, min = 0),
                        numericInput("volunteers", "8. Number of volunteers", 10, min = 0),
                        sliderInput(
                          "target_age",
                          "9. Targetted age range",
                          min = 0,
                          max = 100,
                          value = c(0, 20)
                        ),
                        selectInput(
                          "target_gender",
                          "10. Targetted gender",
                          choices = c("Male" = "male", "Female" = "female"),
                          multiple = TRUE
                        ),
                        textInput(
                          "service",
                          "11. Please provide a description of what your organization does"
                        ),
                        selectInput("priorities", "12. Next year, we are prioritising:", choices = priorities),
                        radioButtons(
                          "evaluated",
                          "13. Has your organization ever been evaluated?",
                          choices = c("Yes", "No")
                        )
                      )
                      )
                    )
                  )),
                  fluidRow(column(
                    4, offset = 3, actionButton("submit", "Submit")
                  ),
                  column(
                    4, img(
                      src = 'firdale_logo.png',
                      height = '100px',
                      width = '100px'
                    )
                  ))
                )))


# Server 
server <- function(input, output) {
  results <- reactive(
    c(
      input$ngo_or_donor,
      input$name,
      input$established,
      input$field,
      input$country,
      input$province,
      input$municipality,
      input$perm_employees,
      input$temp_employees,
      input$volunteers,
      input$target_age,
      input$target_gender,
      input$service,
      input$priorities,
      input$evaluated,
      Sys.time()
    )
  )
  observeEvent(input$submit, {
    Data <- Data %>%
      gs_add_row(ws = "Survey", input = results())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

