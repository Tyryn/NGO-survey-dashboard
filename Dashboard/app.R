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



survey_data <-
  gs_key("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k") %>%
  gs_read(ws = "Survey")

# Clean data

# Break up questions with multiple possible answers into chunk and then merge again
# Each question gets its own column, answers comma separated
numbers <-
  as.character(seq(from = 0, to = 2000)) # Setting limit to 2000
survey_data_cut <-
  survey_data[,-c(1:5)] # Done to prevent year or name fields being counted


# Look at looping

###################################################################################
# Abbreviated dataset that is the first four variables (they always fill the same cells)
first_answers <- survey_data[, 1:4]
first_answers$obs <- 1:nrow(survey_data)
first_answers <- first_answers %>%
  select(obs, everything())
names(first_answers) <-
  c("obs", "ngo_or_donor", "name", "established", "website")

####################################################################################
# Abbreviated dataset containing only field of operation information
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

field_data <- lapply(fields, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
# field <- Filter(function(x)
#   ncol(x)[1] > 0, field_data)

field_data <-
  lapply(field_data, cbind, obs = 1:nrow(survey_data)) # Add "obs" identifier

field_data <- field_data %>%
  reduce(left_join, by = "obs")

field_data <-
  field_data[!duplicated(as.list(field_data))]  # Remove redundant columns

field_data <- field_data %>%
  select(obs, everything())      # Want to move obs to front because want loop to not apply to obs

`%nin%` = Negate(`%in%`)  # Create function for "not in"
shouldChange <- field_data
shouldChange[, -1] <-
  sapply(field_data[,-1], function(x)
    x %nin% fields) # Identify cells that need to be made blank

var_list <- names(field_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  field_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left and remove empty column
field_data <-
  as.data.frame(t(apply(field_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(field_data, function (k)
  all(is.na(k)))
field_data <- field_data[!emptycols]

# Naming columns
names(field_data)[0:ncol(field_data)] <-
  paste("Field", 0:ncol(field_data), sep = "_") # Rename each variable field_stub
names(field_data)[1] <- "obs" # Rename back to obs






#############################################################################
# Abbreviated dataset containing only region of operation information
# Same method as before
regions <- c("South Africa", "Southern Africa", "Rest of world")
region_data <- lapply(regions, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
region_data <-
  lapply(region_data, cbind, obs = 1:nrow(survey_data))
region_data <- region_data %>%
  reduce(left_join, by = "obs")
region_data <-
  region_data[!duplicated(as.list(region_data))]
region_data <- region_data %>%
  select(obs, everything())
shouldChange <- region_data
shouldChange[, -1] <-
  sapply(region_data[,-1], function(x)
    x %nin% regions) # Identify cells that need to be made blank
var_list <- names(region_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  region_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left and remove empty column
region_data <-
  as.data.frame(t(apply(region_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(region_data, function (k)
  all(is.na(k)))
region_data <- region_data[!emptycols]

# Naming columns
names(region_data)[0:ncol(region_data)] <-
  paste("Region", 0:ncol(region_data), sep = "_") # Rename each variable field_stub
names(region_data)[1] <- "obs" # Rename back to obs

###############################################################################
# Abbreviated dataset containing only province information
provinces <-
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

province_data <- lapply(provinces, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
province_data <-
  lapply(province_data, cbind, obs = 1:nrow(survey_data))
province_data <- province_data %>%
  reduce(left_join, by = "obs")
province_data <-
  province_data[!duplicated(as.list(province_data))]
province_data <- province_data %>%
  select(obs, everything())
shouldChange <- province_data
shouldChange[, -1] <-
  sapply(province_data[,-1], function(x)
    x %nin% provinces) # Identify cells that need to be made blank
var_list <- names(province_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  province_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
province_data <-
  as.data.frame(t(apply(province_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(province_data, function (k)
  all(is.na(k)))
province_data <- province_data[!emptycols]

# Naming columns
names(province_data)[0:ncol(province_data)] <-
  paste("Province", 0:ncol(province_data), sep = "_") # Rename each variable field_stub
names(province_data)[1] <- "obs" # Rename back to obs


###############################################################################
# Abbreviated dataset containing only city/municipality information

# Added the prefix "municipality_" to all of these entries. Use the same logic except with a pattern match
municipality <-
  sapply(survey_data_cut, function(x)
    grep("municipality_", x, value = TRUE))
municipality <- unlist(municipality, use.names = FALSE)

municipality_data <- lapply(municipality, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
municipality_data <-
  lapply(municipality_data, cbind, obs = 1:nrow(survey_data))
municipality_data <- municipality_data %>%
  reduce(left_join, by = "obs")
municipality_data <-
  municipality_data[!duplicated(as.list(municipality_data))]
municipality_data <- municipality_data %>%
  select(obs, everything())
shouldChange <- municipality_data
shouldChange[, -1] <-
  sapply(municipality_data[,-1], function(x)
    x %nin% municipality) # Identify cells that need to be made blank
var_list <- names(municipality_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  municipality_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
municipality_data <-
  as.data.frame(t(apply(municipality_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(municipality_data, function (k)
  all(is.na(k)))
municipality_data <- municipality_data[!emptycols]

# Naming columns
names(municipality_data)[0:ncol(municipality_data)] <-
  paste("Municipality", 0:ncol(municipality_data), sep = "_") # Rename each variable field_stub
names(municipality_data)[1] <- "obs" # Rename back to obs

# Remove the prefix
municipality_data <-
  as.data.frame(lapply(municipality_data, function(x)
    gsub("municipality_", "", x)))


###############################################################################
# Abbreviated dataset containing only permanent employees, temp employees,
# volunteers and targetted age range

number_data <- lapply(numbers, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
number_data <-
  lapply(number_data, cbind, obs = 1:nrow(survey_data_cut))
number_data <- number_data %>%
  reduce(left_join, by = "obs")
number_data <-
  number_data[!duplicated(as.list(number_data))]
number_data <- number_data %>%
  select(obs, everything())
shouldChange <- number_data
shouldChange[, -1] <-
  sapply(number_data[,-1], function(x)
    x %nin% numbers) # Identify cells that need to be made blank

var_list <- names(number_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  number_data[, val][shouldChange[, val]] <- NA
}
# Shifting non-NA cells to the left
number_data <-
  as.data.frame(t(apply(number_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(number_data, function (k)
  all(is.na(k)))
number_data <- number_data[!emptycols]

# Name employee columns
names(number_data)[1:4] <-
  c("obs",
    "perm_emp",
    "temp_emp",
    "volunteers")


#####################################################################################
# Abbreviated dataset containing only age information

# Same method as municipality data

age <-
  sapply(survey_data_cut, function(x)
    grep("age_", x, value = TRUE))
age <- unlist(age, use.names = FALSE)

age_data <- lapply(age, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
age_data <-
  lapply(age_data, cbind, obs = 1:nrow(survey_data))
age_data <- age_data %>%
  reduce(left_join, by = "obs")
age_data <-
  age_data[!duplicated(as.list(age_data))]
age_data <- age_data %>%
  select(obs, everything())
shouldChange <- age_data
shouldChange[, -1] <-
  sapply(age_data[,-1], function(x)
    x %nin% age) # Identify cells that need to be made blank
var_list <- names(age_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  age_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
age_data <-
  as.data.frame(t(apply(age_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(age_data, function (k)
  all(is.na(k)))
age_data <- age_data[!emptycols]

# Naming columns
names(age_data)[1:3] <- c("obs", "min_age", "max_age")

# Remove the prefix
age_data <-
  as.data.frame(lapply(age_data, function(x)
    gsub("age_", "", x)))
















#####################################################################################
# Abbreviated dataset containing only gender information

genders <- c("male", "female", "na")

gender_data <- lapply(genders, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
gender_data <-
  lapply(gender_data, cbind, obs = 1:nrow(survey_data_cut))
gender_data <- gender_data %>%
  reduce(left_join, by = "obs")
gender_data <-
  gender_data[!duplicated(as.list(gender_data))]
gender_data <- gender_data %>%
  select(obs, everything())
shouldChange <- gender_data
shouldChange[, -1] <-
  sapply(gender_data[,-1], function(x)
    x %nin% genders) # Identify cells that need to be made blank

var_list <- names(gender_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  gender_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
gender_data <-
  as.data.frame(t(apply(gender_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(gender_data, function (k)
  all(is.na(k)))
gender_data <- gender_data[!emptycols]

# Naming columns
names(gender_data)[0:ncol(gender_data)] <-
  paste("Gender", 0:ncol(gender_data), sep = "_") # Rename each variable field_stub
names(gender_data)[1] <- "obs" # Rename back to obs


#####################################################################################
# Abbreviated dataset containing only service data

# Doing it the same way as the municipality data

service <-
  sapply(survey_data_cut, function(x)
    grep("service_", x, value = TRUE))
service <- unlist(service, use.names = FALSE)

service_data <- lapply(service, function(z) {
  Filter(function(x)
    any(x == z), survey_data)
})
service_data <-
  lapply(service_data, cbind, obs = 1:nrow(survey_data))
service_data <- service_data %>%
  reduce(left_join, by = "obs")
service_data <-
  service_data[!duplicated(as.list(service_data))]
service_data <- service_data %>%
  select(obs, everything())
shouldChange <- service_data
shouldChange[, -1] <-
  sapply(service_data[,-1], function(x)
    x %nin% service) # Identify cells that need to be made blank
var_list <- names(service_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  service_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
service_data <-
  as.data.frame(t(apply(service_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(service_data, function (k)
  all(is.na(k)))
service_data <- service_data[!emptycols]

# Naming columns
names(service_data)[1:2] <- c("obs", "Service")

# Remove the prefix
service_data <-
  as.data.frame(lapply(service_data, function(x)
    gsub("service_", "", x)))


#####################################################################################
# Abbreviated dataset containing only priorities information

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

priorities_data <- lapply(priorities, function(z) {
  Filter(function(x)
    any(x == z), survey_data_cut)
})
priorities_data <-
  lapply(priorities_data, cbind, obs = 1:nrow(survey_data_cut))
priorities_data <- priorities_data %>%
  reduce(left_join, by = "obs")
priorities_data <-
  priorities_data[!duplicated(as.list(priorities_data))]
priorities_data <- priorities_data %>%
  select(obs, everything())
shouldChange <- priorities_data
shouldChange[, -1] <-
  sapply(priorities_data[,-1], function(x)
    x %nin% priorities) # Identify cells that need to be made blank

var_list <- names(priorities_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  priorities_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
priorities_data <-
  as.data.frame(t(apply(priorities_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(priorities_data, function (k)
  all(is.na(k)))
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
evaluated_data <-
  lapply(evaluated_data, cbind, obs = 1:nrow(survey_data_cut))
evaluated_data <- evaluated_data %>%
  reduce(left_join, by = "obs")
evaluated_data <-
  evaluated_data[!duplicated(as.list(evaluated_data))]
evaluated_data <- evaluated_data %>%
  select(obs, everything())
shouldChange <- evaluated_data
shouldChange[, -1] <-
  sapply(evaluated_data[,-1], function(x)
    x %nin% evaluated) # Identify cells that need to be made blank

var_list <- names(evaluated_data)
var_list <- var_list[-1]    # Remove obs from the list

for (val in var_list) {
  evaluated_data[, val][shouldChange[, val]] <- NA
}

# Shifting non-NA cells to the left
evaluated_data <-
  as.data.frame(t(apply(evaluated_data, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))
emptycols <- sapply(evaluated_data, function (k)
  all(is.na(k)))
evaluated_data <- evaluated_data[!emptycols]

# Naming columns
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

priorities_prop <- priorities_data[, -c(1)]
priorities_prop$all <- ""
priorities_long <- gather(priorities_prop, all, priorities_prop[, 1:ncol(priorities_prop)]) %>%
  select(-starts_with("all"))
names(priorities_long)[1] <- "All priorities"


priorities_table <- table(priorities_long)
priorities_prop <- as.data.frame(prop.table(priorities_table))

ggplot(priorities_prop, aes(x=priorities_long, y=Freq)) + 
  geom_bar(stat = "identity", fill = " steelblue") + theme_classic() +
  geom_text(aes(label=round(Freq, 2)), vjust=1.6, color=" white", size=3.5)







#######################################################################################


#######################################################################################
# Dashboard


# Create vector containing all Fields in the dataset
fields_unique <- as.vector(as.matrix(field_data[, -c(1)]))
fields <- unique(fields_unique)
fields <- fields[!is.na(fields)]


# Define UI for application


ui <- dashboardPage(
  dashboardHeader(title = "Name pending"),
  dashboardSidebar(
      fluidRow(status = "primary",
        checkboxGroupInput("field_of_work", "Field of work", choices =  fields,
                           selected = fields
        )
      )
  ),
  dashboardBody(column(12, box(title = "Priorities", status = "primary", solidHeader = TRUE,
                       plotOutput("priorities_plot", width = "100%", height = "400px")
  ),
  box(title = "Word cloud of service provided", status = "primary", solidHeader = TRUE,
      plotOutput("wordcloud", width = "100%", height = "400px")
  )
  )
)
)






# Define server logic
server <- function(input, output) {
  
  # Dataset reactive to checkbox input
  
  selectedData <- reactive({
    selectedData <- clean_data %>%
      filter_all(any_vars(str_detect(., paste(input$field_of_work, collapse="|"))))
  })
  
  output$priorities_plot <- renderPlot({
    
    priorities_prop <- selectedData()[, c(grep("Priority", names(selectedData()))), drop=F]

    priorities_prop$all <- ""
    priorities_long <- gather(priorities_prop, all, priorities_prop[, 1:ncol(priorities_prop)]) %>%
      select(-starts_with("all"))
    names(priorities_long)[1] <- "All priorities"


    priorities_table <- table(priorities_long)
    priorities_prop <- as.data.frame(prop.table(priorities_table))


    input$priorities_plot
    
    ggplot(priorities_prop, aes(x=priorities_long, y=Freq)) + 
      geom_bar(stat = "identity", fill = " steelblue") +
      geom_text(aes(label=round(Freq, 2)), vjust=1.6, color=" white", size=3.5) +
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2==0, '', '\n'), labels[i]))
      }) + xlab("Priorities") + ylab("Proportion of NGOs that have as priority")
    
  })
  
  output$wordcloud <- renderPlot({
    docs <- Corpus(VectorSource(selectedData()$Service))
    
    # Cleaning the text
    docs <- tm_map(docs, content_transformer(tolower))    # To lower case
   # docs <- tm_map(docs, removeNumber)  # Remove numbers
    docs <- tm_map(docs, removeWords, stopwords("english")) # Remove common English words
    docs <- tm_map(docs, removeWords, c("help", "assist"))  # Remove words common and useless words to this context
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    
    # Build term-document matrix
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq=v)
    head(d,10)
    
    # Build word cloud
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "RdBu"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
