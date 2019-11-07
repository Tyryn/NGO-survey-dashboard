# Dashboard

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

survey_data <-
  gs_key("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k") %>%
  gs_read(ws = "Survey")

## Get separate geo dataframes, separated by province
SouthAfricanCities <- read_excel("SouthAfricanCities.xls")

# Create function for "not in"
`%nin%` = Negate(`%in%`)  


myfunc <- function(v1) {
  deparse(substitute(v1))
}


# Clean data

# Break up questions with multiple possible answers into chunk and then merge again
# Each question gets its own column, answers comma separated
number_data_vector <-
  as.character(seq(from = 0, to = 2000)) # Setting limit to 2000
survey_data_cut <-
  survey_data[, -c(1:5)] # Done to prevent year or name fields being counted

###################################################################################
# Abbreviated dataset that is the first four variables (they always fill the same cells)
first_answers <- survey_data[, 1:4]
first_answers$obs <- 1:nrow(survey_data)
first_answers <- first_answers %>%
  select(obs, everything())
names(first_answers) <-
  c("obs", "ngo_or_donor", "name", "established", "website")


###################################################################################
# First part of abbreviating datasets
##################################################################################
# Field of operation
field_data_vector <-
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

field_data <- lapply(field_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# Region of operation
region_data_vector <- c("South Africa", "Southern Africa", "Rest of world")
region_data <- lapply(region_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# Province of operation
province_data_vector <-
  c(
    "Western Cape",
    "Northern Cape",
    "Eastern Cape",
    "Free State",
    "North West",
    "KwaZulu Natal",
    "Gauteng",
    "Limpopo",
    "Mpumalanga"
  )
province_data <- lapply(province_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# City/municipality of operation
municipality <-
  sapply(survey_data_cut, function(x)
    grep("municipality_", x, value = TRUE))
municipality_data_vector <- unlist(municipality, use.names = FALSE)

municipality_data <- lapply(municipality_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# Numbers data (Employees and volunteers)
number_data <- lapply(number_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})

# Age data
age <-
  sapply(survey_data_cut, function(x)
    grep("age_", x, value = TRUE))
age_data_vector <- unlist(age, use.names = FALSE)
age_data <- lapply(age_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})

# Gender data
gender_data_vector <- c("male", "female", "na")
gender_data <- lapply(gender_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})

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

# Whether been evaluated data
evaluated_data_vector <- c("Yes", "No")
evaluated_data <- lapply(evaluated_data_vector, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})


#### Now looping 
dataList <- list(field_data, region_data, province_data, municipality_data, number_data, age_data,
                 gender_data, service_data, priorities_data, evaluated_data)
vectorList <- list(field_data_vector, region_data_vector, province_data_vector,
                   municipality_data_vector, number_data_vector, age_data_vector,
                   gender_data_vector, service_data_vector, priorities_data_vector, evaluated_data_vector)


list <- mapply(function(z, y) {
  z <-
    lapply(z, cbind, obs = 1:nrow(survey_data_cut))
  
  z <- z %>%
    reduce(left_join, by = "obs")
  
  z <-
    z[!duplicated(as.list(z))]
  z <- z %>%
    select(obs, everything())
  
  shouldChange <- z
  shouldChange[,-1] <-
    sapply(z[, -1], function(x)
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
region_data <- list[[2]]
province_data <- list[[3]]
municipality_data <- list[[4]]
number_data <- list[[5]]
age_data <- list[[6]]
gender_data <- list[[7]]
service_data <- list[[8]]
priorities_data <- list[[9]]
evaluated_data <- list[[10]]


#### Naming columns
## Field data
names(field_data)[0:ncol(field_data)] <-
  paste("Field", 0:ncol(field_data), sep = "_") # Rename each variable field_stub
names(field_data)[1] <- "obs" # Rename back to obs

## Region data
names(region_data)[0:ncol(region_data)] <-
  paste("Region", 0:ncol(region_data), sep = "_") # Rename each variable field_stub
names(region_data)[1] <- "obs" # Rename back to obs

## Province data
names(province_data)[0:ncol(province_data)] <-
  paste("Province", 0:ncol(province_data), sep = "_") # Rename each variable field_stub
names(province_data)[1] <- "obs" # Rename back to obs

## Municipality data
names(municipality_data)[0:ncol(municipality_data)] <-
  paste("Municipality", 0:ncol(municipality_data), sep = "_") # Rename each variable field_stub
names(municipality_data)[1] <- "obs" # Rename back to obs
# Remove the prefix
municipality_data <-
  as.data.frame(lapply(municipality_data, function(x)
    gsub("municipality_", "", x)))

## Number data (employees)
names(number_data)[1:4] <-
  c("obs",
    "perm_emp",
    "temp_emp",
    "volunteers")

## Age data
names(age_data)[1:3] <- c("obs", "min_age", "max_age")
# Remove the prefix
age_data <-
  as.data.frame(lapply(age_data, function(x)
    gsub("age_", "", x)))

## Gender data
names(gender_data)[0:ncol(gender_data)] <-
  paste("Gender", 0:ncol(gender_data), sep = "_") # Rename each variable field_stub
names(gender_data)[1] <- "obs" # Rename back to obs

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


#######################################################################################
# Merge dataframes into single cleaned dataframe to be used in dashboard

dataframes <-
  list(
    first_answers,
    field_data,
    region_data,
    province_data,
    municipality_data,
    number_data,
    gender_data,
    age_data,
    service_data,
    priorities_data,
    evaluated_data
  )

dataframes = lapply(dataframes, function(x) {
  x['obs'] = lapply(x['obs'], factor)
  x
})
clean_data <- dataframes %>%
  reduce(left_join, by = "obs")



#######################################################################################

# For Priorities graph, create dataset that gives the total proportions of each category

priorities_prop <- priorities_data[,-c(1)]
priorities_prop$all <- ""
priorities_long <-
  gather(priorities_prop, all, priorities_prop[, 1:ncol(priorities_prop)]) %>%
  select(-starts_with("all"))
names(priorities_long)[1] <- "All priorities"


priorities_table <- table(priorities_long)
priorities_prop <- as.data.frame(prop.table(priorities_table))

ggplot(priorities_prop, aes(x = priorities_long, y = Freq)) +
  geom_bar(stat = "identity", fill = " steelblue") + theme_classic() +
  geom_text(aes(label = round(Freq, 2)),
            vjust = 1.6,
            color = " white",
            size = 3.5)







#######################################################################################


#######################################################################################
# Dashboard


# Create vector containing all Fields in the dataset
fields_unique <- as.vector(as.matrix(field_data[,-c(1)]))
fields <- unique(fields_unique)
fields <- fields[!is.na(fields)]


# Define UI for application


ui <- dashboardPage(
  dashboardHeader(title = "Name pending"),
  dashboardSidebar(fluidRow(
    status = "primary",
    checkboxGroupInput(
      "field_of_work",
      "Field of work",
      choices =  fields,
      selected = fields
    )
  )),
  dashboardBody(column(
    12,
    box(
      title = "Priorities",
      status = "primary",
      solidHeader = TRUE,
      plotOutput("priorities_plot", width = "100%", height = "400px")
    ),
    box(
      title = "Word cloud of service provided",
      status = "primary",
      solidHeader = TRUE,
      plotOutput("wordcloud", width = "100%", height = "400px")
    )
  ))
)






# Define server logic
server <- function(input, output) {
  # Dataset reactive to checkbox input
  
  selectedData <- reactive({
    selectedData <- clean_data %>%
      filter_all(any_vars(str_detect(
        ., paste(input$field_of_work, collapse = "|")
      )))
  })
  
  output$priorities_plot <- renderPlot({
    priorities_prop <-
      selectedData()[, c(grep("Priority", names(selectedData()))), drop = F]
    
    priorities_prop$all <- ""
    priorities_long <-
      gather(priorities_prop, all, priorities_prop[, 1:ncol(priorities_prop)]) %>%
      select(-starts_with("all"))
    names(priorities_long)[1] <- "All priorities"
    
    
    priorities_table <- table(priorities_long)
    priorities_prop <- as.data.frame(prop.table(priorities_table))
    
    
    input$priorities_plot
    
    ggplot(priorities_prop, aes(x = priorities_long, y = Freq)) +
      geom_bar(stat = "identity", fill = " steelblue") +
      geom_text(
        aes(label = round(Freq, 2)),
        vjust = 1.6,
        color = " white",
        size = 3.5
      ) +
      scale_x_discrete(
        labels = function(labels) {
          sapply(seq_along(labels), function(i)
            paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        }
      ) + xlab("Priorities") + ylab("Proportion of NGOs that have as priority")
    
  })
  
  output$wordcloud <- renderPlot({
    docs <- Corpus(VectorSource(selectedData()$Service))
    
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
}

# Run the application
shinyApp(ui = ui, server = server)
