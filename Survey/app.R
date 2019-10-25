## Still to do:
# 1. Create message that tells user that submission successful.
#     Done
# 2. Conditional panel that matches city to province
# 3. Prevent submission without answering all questions
#     Done
# 4. Form resets or something when submitted.
#     Done


# Survey

library(shiny)
library(shinydashboard)
library("googlesheets")
library("DT")
library(readxl)
library(shinyjs)



# Get shiny token to access google drive
# shiny_token <- gs_auth()
# saveRDS(shiny_token, "shiny_app_token.rds")


## Google Sheet has been created. This is the sheet that it will alter with each submission
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

# Javascript code so that page refreshes when submit button is pressed
jscode <- "shinyjs.refresh = function() { history.go(0); }"

# Code for mandatory fields
fieldsMandatory <- c("name")

labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}
appCSS <- ".mandatory_star { color: red; }"

# UI
ui <-
  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    fluidRow(column(12, offset = 5, titlePanel("Survey"))),
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
          fluidRow(textInput(
            "name", labelMandatory("2. Name of organization"), ""
          )),
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
                choices = c("Select from the list" = "", fields),
                multiple = TRUE
              )
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.ngo_or_donor == 'ngo'",
              checkboxGroupInput(
                "country",
                "5. Regions of operation",
                choices = c(
                  "South Africa" = "south_africa",
                  "Southern Africa" = "southern_africa",
                  "Rest of world" = "row"
                ),
                selected = "south_africa"
              )
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.ngo_or_donor == 'ngo' && input.country &&
              input.country.indexOf('south_africa') > -1",
              ## Necessary javascript expression
              selectInput(
                "province",
                "6. South African provinces where your organization operates",
                c("Select provinces of operation" = "",
                  city_names$ProvinceName),
                multiple = TRUE
              ),
              selectInput(
                "municipality",
                "7. (Nearest) South African village, town, or city where your organization operates",
                c("Select places of operation" = "", city_names$AccentCity),
                multiple = TRUE
              )
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.ngo_or_donor == 'ngo' && input.country &&
              input.country.indexOf('south_africa') > -1",
              numericInput("perm_employees", "8. Number of permanent employees", 0, min = 0),
              numericInput("temp_employees", "9. Number of temporary employees", 0, min = 0),
              numericInput("volunteers", "10. Number of volunteers", 0, min = 0),
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
                choices = c("Male" = "male", "Female" = "female", "Not applicable" = "na"),
                multiple = TRUE
              ),
              textInput(
                "service",
                "13. Please provide a description of what your organization does"
              ),
              selectInput(
                "priorities",
                "14. Next year, we are prioritising:",
                choices = c("Select from the list" = "", priorities),
                multiple = TRUE
              ),
              radioButtons(
                "evaluated",
                "15. Has your organization ever been evaluated?",
                choices = c("Yes", "No")
              )
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.ngo_or_donor == 'ngo' && input.country &&
              input.country.indexOf('south_africa') < 0",
              numericInput("perm_employees", "6. Number of permanent employees", 0, min = 0),
              numericInput("temp_employees", "7. Number of temporary employees", 0, min = 0),
              numericInput("volunteers", "8. Number of volunteers", 0, min = 0),
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
                choices = c("Male" = "male", "Female" = "female", "Not applicable" = "na"),
                multiple = TRUE
              ),
              textInput(
                "service",
                "11. Please provide a description of what your organization does"
              ),
              selectInput(
                "priorities",
                "12. Next year, we are prioritising:",
                choices = c("Select from the list:", priorities),
                multiple = TRUE
              ),
              radioButtons(
                "evaluated",
                "13. Has your organization ever been evaluated?",
                choices = c("Yes", "No")
              )
            )
          )
      )
      )),
      fluidRow(
        useShinyjs(),
        extendShinyjs(text = jscode),
        column(
          4,
          offset = 3,
          actionButton("submit", "Submit", class = "btn-primary")
        ),
        column(4, img(
          src = 'firdale_logo.png',
          height = '100px',
          width = '100px'
        ))
      )
  ))
  )


# Server
server <- function(input, output, session) {
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
  observeEvent(input$submit, {
    showNotification("Response successfully submitted", type = "message")
  })
  observeEvent(input$submit, {
    js$refresh()
    
  })
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
