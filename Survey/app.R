## Still to do:
# 1. Create message that tells user that submission successful.
#     Done
# 2. Conditional panel that matches city to province
#     Done
# 3. Prevent submission without answering all questions
#     Done
# 4. Form resets or something when submitted.
#     Done
# 5. Need to sort out something for when age is inapplicable
#   Done
# 6. Need to change survey so that instead of province and city (and age) disappearing,
#   the province, city and age widgets cannot be filled conditional on other answers.
#   Done
# 7. Need to prevent na and genders from being filled


# 1. Link submit button to dashboard url
# 2. Write blurb introducing survey, also asking if its alright if the data should 

# Survey

library(shiny)
library(shinydashboard)
library("googlesheets")
library("DT")
library(readxl)
library(shinyjs)
library(shinyWidgets)
library(V8)

# Get shiny token to access google drive
# shiny_token <- gs_auth()
# saveRDS(shiny_token, "shiny_app_token.rds")


## Google Sheet has been created. This is the sheet that it will alter with each submission
sheetkey <- "1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k"
Data <- gs_key(sheetkey)


## Get separate geo dataframes, separated by province
SouthAfricanCities <- read_excel("SouthAfricanCities.xls")

fields <-
  c(
    "Advocacy & Awareness",
    "Culture & Society",
    "Democracy & Civic Rights",
    "Disability",
    "Displaced Population & Refugees",
    "Education",
    "Environment",
    "Family Care",
    "Health",
    "Human Rights",
    "Labour",
    "Law & Legal Affairs",
    "Rural Development",
    "Science & Technology",
    "Youth Empowerment",
    "Other"
  )

priorities <-
  c(
    "Improving our monitoring & evaluation",
    "Growing our team",
    "Upskilling our team",
    "Securing new equipment/venues",
    "Expanding our organisation's scope",
    "Building relationship with funders",
    "Recruiting volunteers",
    "Securing funding",
    "Other"
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
  #fluidPage(setBackgroundColor("#ADD8E6"),
  fluidPage(setBackgroundColor("#6E876C"),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    fluidRow(column(12, offset = 5, 
                    h1(id="big-heading", ""),
                    tags$style(HTML("#big-heading{color: white;}")))),
    fluidRow(column(12,
                    fluidRow(
                      column(
                        8,
                        offset = 2,
                        wellPanel(
                          fluidRow(selectInput(
                            "ngo_or_donor",
                            "1. NGO, donor, or social impact investor?",
                            choices = c("NGO" = "ngo", "Donor" = "donor", "Social impact investor"=
                                          "sia", "Other"="other")
                          )),
                          fluidRow(textInput(
                            "name", labelMandatory("2. Name of organisation"), ""
                          )),
                          fluidRow(
                            selectInput(
                              "established",
                              "3. Year established",
                              choices = 1900:as.numeric(format(Sys.Date(), "%Y")),
                              selected = 2010
                            )
                          ),
                          fluidRow(textInput(
                            "website", "4. Organisation's website"
                          )),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              selectInput(
                                "field",
                                "5. Field of work",
                                choices = c("Select up to 5 from the list" = "", fields),
                                multiple = TRUE
                              )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'donor' || input.ngo_or_donor == 'sia'",
                              selectInput(
                                "field_2",
                                "5. Main fields of focus",
                                choices = c("Select from the list" = "", fields),
                                multiple = TRUE
                              )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'other'",
                              textInput(
                                "other_description",
                                "5. Please describe your organisation"
                              )
                            )
                          ),
                          fluidRow(
                              checkboxGroupInput(
                                "country",
                                "6. Regions of operation",
                                choices = c("South Africa",
                                            "Southern Africa",
                                            "Rest of world"),
                                selected = "South Africa"
                              )
                          ),
                          conditionalPanel(
                            condition = "input.country.indexOf('South Africa')>-1",
                            selectInput(
                              "municipality",
                              "6a. South African villages, towns, or cities where your organisation mainly operates",
                              c("Select up to 10" = "", SouthAfricanCities$AccentCity),
                              multiple = TRUE
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              textInput(
                                "service",
                                "7. Please provide a description of what your organization does"
                              )
                            )),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              selectInput(
                                "priorities",
                                "8. Next year, your organization is prioritising:",
                                choices = c("Select any that apply" = "", priorities),
                                multiple = TRUE
                              )
                            )),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              radioButtons(
                                "evaluated",
                                "9. Your organisation has an effective M&E system in place",
                                choices = c("Agree", "Neutral", "Disagree")
                              )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              selectInput(
                                "pain",
                                "10. What are your organisation's main M&E pain points?",
                                choices = c("Collecting data", "Establishing a framework", "Finding experts", "Other"),
                                multiple = TRUE
                              )
                            )
                          )
                        )
                      )
                    ))),
    fluidRow(
      useShinyjs(),
      extendShinyjs(text = jscode),
      column(
        4,
        offset = 3,
        actionButton("submit", "Submit", class = "btn-primary", onclick = "window.open('https://firdaleconsulting.shinyapps.io/NGO_dashboard/', '_blank')"
      )),
      column(4, img(
        src = 'firdale_logo.png',
        height = '100px',
        width = '100px'
      ))
    )
  )

# Server
server <- function(input, output, session) {
  results <- reactive(
    c(
      input$ngo_or_donor,
      input$name,
      input$established,
      input$website,
      input$field,
      input$field_2,
      paste0("other_description", sep = "_", input$other_description),
      input$country,
      paste0("municipality", sep = "_", input$municipality),
      paste0("service", sep = "_", input$service),
      input$priorities,
      input$evaluated,
      paste0("pain", sep = "_", input$pain),
      Sys.time()
    )
  )
  observeEvent(input$submit, {
    Data <- Data %>%
      gs_add_row(ws = "Survey", input = results())
  })
  observeEvent(input$submit, {
    showNotification("Response successfully submitted. Thanks for filling out our form!", type = "message")
  })
  observeEvent(input$submit, {
    delay(5000, js$refresh())   ## Delay so that the message stays for 5 secs, then refreshes.
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
