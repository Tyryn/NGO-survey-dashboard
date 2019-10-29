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


## Get separate geo dataframes, separated by province
SouthAfricanCities <- read_excel("SouthAfricanCities.xls")

fields <-
  c(
    "Advocacy & Awareness",
    "Agriculture",
    "Business & Economic Policy",
    "Child Education",
    "Youth Empowerment",
    " Citizenship",
    "Communication",
    "Conflict Resolution",
    "Peace Building",
    "ICT",
    "Culture & Society",
    "Democracy & Civic Rights",
    "Rural Development",
    "Disability & Handicap",
    "Displaced Population & Refugees",
    "Education",
    "Environment",
    "Family Care",
    "Women’s Rights",
    "Governance",
    "Health",
    "Human Rights",
    "Charity/Philanthropy",
    "Labor",
    "Law & Legal Affairs",
    "Migrant Workers",
    "Relief",
    "Reconstruction",
    "Rehabilitation",
    "Research & Studies",
    "Science",
    "Social Media",
    "Technology",
    "Transparency",
    "Training & Capacity",
    "Building"
  )

priorities <-
  c(
    "Improving our monitoring & evaluation",
    "Growing our team",
    "Upskilling our team",
    "Securing new equipment/venues",
    "Expanding our organization's scope",
    "Building relationship with funders",
    "Recruiting volunteers"
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
    fluidRow(column(12,
                    fluidRow(
                      column(
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
                          fluidRow(textInput(
                            "website", labelMandatory("4. Organization's website"), ""
                          )),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              selectInput(
                                "field",
                                "5. Field of work",
                                choices = c("Select from the list" = "", fields),
                                multiple = TRUE
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
                          ## Necessary javascript expression
                          selectInput(
                            "province",
                            "6a. South African provinces where your organization operates",
                            c(
                              "Select provinces of operation" = "",
                              SouthAfricanCities$ProvinceName
                            ),
                            multiple = TRUE
                          ),
                          selectInput(
                            "municipality",
                            "6b. (Nearest) South African village, town, or city where your organization operates",
                            c("Select places of operation" = "", SouthAfricanCities$AccentCity),
                            multiple = TRUE
                          ),
                          fluidRow(
                            numericInput(
                              "perm_employees",
                              "7. Number of permanent employees",
                              0,
                              min = 0,
                              max = 2000
                            ),
                            numericInput(
                              "temp_employees",
                              "8. Number of temporary employees",
                              0,
                              min = 0,
                              max = 2000
                            ),
                            numericInput(
                              "volunteers",
                              "9. Number of volunteers",
                              0,
                              min = 0,
                              max = 2000
                            ),
                            checkboxGroupInput(
                              "target_gender",
                              "10. Targetted gender(s)",
                              choices = c(
                                "Male" = "male",
                                "Female" = "female",
                                "Not applicable" = "na"
                              )
                            ),
                            sliderInput(
                              "target_age",
                              "11. Targetted age range",
                              min = 0,
                              max = 100,
                              value = c(0, 20)
                            ),
                            textInput(
                              "service",
                              "12. Please provide a description of what your organization does"
                            ),
                            selectInput(
                              "priorities",
                              "13. Next year, your organization is prioritising:",
                              choices = c("Select from the list" = "", priorities),
                              multiple = TRUE
                            ),
                            radioButtons(
                              "evaluated",
                              "14. Has your organization ever been evaluated?",
                              choices = c("Yes", "No")
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
        actionButton("submit", "Submit", class = "btn-primary")
      ),
      column(4, img(
        src = 'firdale_logo.png',
        height = '100px',
        width = '100px'
      ))
    )
  )

# Server
server <- function(input, output, session) {
  observeEvent(input$province, {
    ## Update possible towns depending on province selected
    updateSelectInput(session,
                      'municipality',
                      choices = unique(SouthAfricanCities$AccentCity[SouthAfricanCities$ProvinceName ==
                                                                       input$province]))
  })
  # observeEvent(input$target_gender=="na", {
  #   updateCheckboxGroupInput(session, "target_gender",
  #                            selected = )
  #
  # })
  #
  
  
  observe({
    shinyjs::toggleState("province", input$country == "South Africa")
    shinyjs::toggleState("municipality", input$country == "South Africa")
  })
  observe({
    shinyjs::toggleState("target_age",
                         input$target_gender == "male" || input$target_gender == "female")
  })
  
  results <- reactive(
    c(
      input$ngo_or_donor,
      input$name,
      input$established,
      input$website,
      input$field,
      input$country,
      input$province,
      paste0("municipality", sep = "_", input$municipality),
      input$perm_employees,
      input$temp_employees,
      input$volunteers,
      paste0("age", sep = "_", input$target_age),
      input$target_gender,
      paste0("service", sep = "_", input$service),
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
