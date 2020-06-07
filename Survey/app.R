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
library(googlesheets)
library(DT)
library(readxl)
library(shinyjs)
library(shinyWidgets)
library(V8)
library(googlesheets4)




# Authorise the app ####

sheets_deauth()
# set values in options
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "tyryn.carnegie@gmail.com"
)
# run sheets auth

sheets_auth(path = "client_secret_691536632541-7r4t1u45ntoqhrkddtcmfrr2udblpsen.apps.googleusercontent.com.json")



## Google Sheet has been created. This is the sheet that it will alter with each submission
# sheetkey <- "1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k"
Data <- sheets_read("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k")


## Get separate geo dataframes, separated by province
SouthAfricanCities <- read_excel("SouthAfricanCities.xlsx") %>%
  dplyr::arrange(City)



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
fieldsMandatory <- c("name", "field")

labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}
appCSS <- ".mandatory_star { color: red; }"

# UI ####
ui <-
  #fluidPage(setBackgroundColor("#ADD8E6"),
  fluidPage(setBackgroundColor("#6E876C"),
            headerPanel(""),
            tags$style(HTML("
             .box.box-solid.box-primary>.box-header {

                            }
                            
                            .box.box-solid.box-primary{
                            
                            background:#f542b3
                            }
              #submit{margin-bottom:20px}
                            ")),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    fluidRow(column(12,
                    fluidRow(
                      column(
                        8,
                        offset = 2,
                        wellPanel(
                          fluidRow(selectInput(
                            "ngo_or_donor",
                            "NGO, donor, or social impact investor?",
                            choices = c("NGO" = "ngo", "Donor" = "donor", "Social impact investor"=
                                          "sia", "Other"="other")
                          )),
                          fluidRow(textInput(
                            "name", labelMandatory("Name of organisation"), ""
                          )),
                          fluidRow(
                            selectInput(
                              "established",
                              "Year established",
                              choices = 1900:as.numeric(format(Sys.Date(), "%Y")),
                              selected = 2010
                            )
                          ),
                          fluidRow(textInput(
                            "website", "Organisation's website"
                          )),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo' || input.ngo_or_donor == 'donor' || input.ngo_or_donor == 'sia'
                              || input.ngo_or_donor == 'other'",
                              selectizeInput(
                                "field",
                                labelMandatory("Field of work"),
                                choices = c("Select up to 5" = "", fields),
                                multiple = TRUE, options = list(plugins= list('remove_button'))                              )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'other'",
                              textInput(
                                "other_description",
                                "Please describe your organisation"
                              )
                            )
                          ),
                          fluidRow(
                              checkboxGroupInput(
                                "country",
                                "Regions of operation",
                                choices = c("South Africa",
                                            "Southern Africa",
                                            "Rest of world"),
                                selected = "South Africa"
                              )
                          ),
                          conditionalPanel(
                            condition = "input.country.indexOf('South Africa')>-1",
                            selectizeInput(
                              "municipality",
                              "South African villages, towns, or cities where your organisation mainly operates",
                              c("Search and select up to 10" = "", SouthAfricanCities$AccentCity),
                              multiple = TRUE, options = list(plugins= list('remove_button'))
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              textAreaInput(height = "100px",
                                "service",
                                "Please provide a description of what your organisation does"
                              )
                            )),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              selectizeInput(
                                "priorities",
                                "Next year, your organisation is prioritising",
                                choices = c("Select any that apply" = "", priorities),
                                multiple = TRUE, options = list(plugins= list('remove_button'))
                              )
                            )),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              radioButtons(
                                "evaluated",
                                "Your organisation has an effective M&E system in place",
                                choices = c("Agree", "Neutral", "Disagree")
                              )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.ngo_or_donor == 'ngo'",
                              selectInput(
                                "pain",
                                "What are your organisation's main M&E pain points?",
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
        offset = 5,
        actionButton("submit", "Submit responses", class = "btn-primary" 
      ))
    )
  )

# Server ####
server <- function(input, output, session) {
  
  # The pop-up at start up
  startup_modal <- modalDialog(
    footer = modalButton(label = "Continue to survey"),
                fluidRow(column(12, offset = 5, 
                                h1(id="big-heading", ""),
                                tags$style(HTML("#big-heading{color: white;}")))),
                fluidRow(column(12,
                                fluidRow(
                                  column(
                                    8,
                                    offset = 2,
                                    fluidRow(column(8, offset = 1, h3("Firdale Consulting")),
                                             column(3, img(
                                               src = 'firdale_logo_3.svg',
                                               height = "50px" 
                                             ))))),
                                fluidRow(hr()),
                                fluidRow(column(10, offset = 1,
                                                p("We want to know more about the NGOs, donors, and
                                                  social impact investors in South Africa. We also want these
                                                  organisations to connect with and know more about each other."),
                                                p("Your responses to the survey will be added to a dashboard that
                                                  provides a landscape of development organisations in South
                                                  Africa. This dashboard only uses
                                                  information regarding your organisation's region of operation
                                                  and its field of work. We will not share information about your organisation's
                                                  priorities and M&E."),
                                                p("For any questions, contact us at megan@firdaleconsulting.com."))))))
  
  
  showModal(startup_modal)
  
  
  results <- reactive(
    data.frame(t(unlist(c(
      input$ngo_or_donor,
      input$name,
      input$established,
      input$website,
      input$field,
      # input$field_2,
      paste0("other_description", sep = "_", input$other_description),
      input$country,
      paste0("municipality", sep = "_", input$municipality),
      paste0("service", sep = "_", input$service),
      input$priorities,
      input$evaluated,
      paste0("pain", sep = "_", input$pain),
      Sys.time()
    ))))
  )
  observeEvent(input$submit, {
    sheet_append("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k", data = results())
  })
  observeEvent(input$submit, {
    showModal(modalDialog(footer = actionButton("hyperlink", "Go to dashboard",  onclick = "window.open('https://firdaleconsulting.shinyapps.io/NGO_dashboard/', '_blank')"),
      fluidRow(column(
      12, offset = 5,
      h1(id = "big-heading", ""),
      tags$style(HTML("#big-heading{color: white;}"))
    )),
    fluidRow(column(
      12,
      fluidRow(column(8,
                      offset = 2,
                      fluidRow(
                        column(8, offset = 1, h3("Firdale Consulting")),
                        column(3, img(src = 'firdale_logo_3.svg',
                                      height = "50px"))
                      ))),
      fluidRow(hr()),
      p("Thank you for filling in the survey! Your response has been successfully recorded.")
    ))))
  })
  
  
  # observeEvent(input$submit, {
  #   showNotification("Response successfully submitted. Thanks for filling out our form! You will now be taken to the dashboard.", 
  #                    type = "message")
  # })
  # observeEvent(input$submit, {
  #   delay(5000, js$refresh())   ## Delay so that the message stays for 5 secs, then refreshes.
  # })
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
