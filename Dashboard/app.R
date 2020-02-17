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
library(reshape)
library(scales)
library(timevis)

survey_data <-
  gs_key("1bnWcFKSQZo5aMOd_9BdjQIt_W4iMjWvzMODOoepLq6k") %>%
  gs_read(ws = "Survey")

## Get separate geo dataframes, separated by province
SouthAfricanCities <- read_excel("~/Firdale Consulting/NGO-survey-dashboard/Dashboard/SouthAfricanCities.xls")

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
  survey_data[,-c(1:5)] # Done to prevent year or name fields being counted

###################################################################################
# Abbreviated dataset that is the first four variables (they always fill the same cells)
first_answers <- survey_data[, 1:4]
first_answers$obs <- as.character(1:nrow(survey_data))
#first_answers$obs <- paste("", first_answers$obs)
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
region_data_vector <-
  c("South Africa", "Southern Africa", "Rest of world")
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
dataList <-
  list(
    field_data,
    region_data,
    province_data,
    municipality_data,
    number_data,
    age_data,
    gender_data,
    service_data,
    priorities_data,
    evaluated_data
  )
vectorList <-
  list(
    field_data_vector,
    region_data_vector,
    province_data_vector,
    municipality_data_vector,
    number_data_vector,
    age_data_vector,
    gender_data_vector,
    service_data_vector,
    priorities_data_vector,
    evaluated_data_vector
  )


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
  shouldChange[, -1] <-
    sapply(z[,-1], function(x)
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

dataframes <- lapply(dataframes, function(x) {
  x['obs'] = lapply(x['obs'], as.character)
  x                                         # The return is required otherwise it just return the new vector.
})

# Remove whitespace before obs - dataframes after 'first_answers' weirdly have that
dataframes <- lapply(dataframes, function(x) {
  x$obs <- gsub('\\s+', '', x$obs)
  x
})

clean_data <- dataframes %>%
  reduce(left_join, by = "obs")


###################################################################################
# Merge in coordinates of place names

# Clean SA data so it is only Accent city and coordinates
sa_data <- SouthAfricanCities[, c(2, 4, 5)]

municipalities <- clean_data %>%
  select(Municipality_1:perm_emp)

municipalities <- municipalities[1:(length(municipalities)-1)]

for (i in seq(municipalities)) {
  assign(paste0("municipalities_", i), municipalities[, i])
}

municipList <- lapply(ls(pattern = "municipalities_"), function(x) get(x)) 

obs <- 1:nrow(municipalities)
municipCoord <- lapply(municipList, function(x) cbind(x, obs))

municipCoord <- lapply(municipCoord, function(x) {
  merge(x, sa_data, by.x = 1, by.y = 1, all.x = TRUE)
})


# Need to sort duplicate towns. FOR NOW JUST RANDOMLY DELETING A DUPLICATE :/
# province_merged <- lapply(municipCoord, function(x) {
#   merge(x, province_data, by="obs")
# })


cols <- as.list(as.character(1:ncol(municipalities)))

dfs <- Map(function(x, n) setNames(x, c(names(x)[c(1,2)], c(paste0("Latitude_", n),
                                                               paste0("Longitude_", n)))),
                    municipCoord, cols)

municipCoord <- dfs %>%
  reduce(merge, by = "obs")

municipCoord <- distinct(municipCoord, obs, .keep_all = TRUE)   # Just randomly deleting duplicates

municipalities_colnames <- colnames(municipalities)

# Merge back into clean dataset with coordinates
clean_data <- clean_data[, -which(names(clean_data) %in% municipalities_colnames)] 
clean_data <- merge(clean_data, municipCoord, by = "obs", .keep_all=TRUE)


#### Reshape data long
## Slice into x number of dataframes that will be renamed then appended
colnames <- c("obs", "Municipality", "Latitude", "Longitude")

splitMunicip <- municipCoord[, grepl("Municipality", names(municipCoord))]
numMunicip <- c(1:ncol(splitMunicip)) %>%
  as.character()

splitMunicip <- lapply(numMunicip, function(x) {
  municipCoord[c(1, grep(x, colnames(municipCoord)))]
})
splitMunicip <- lapply(splitMunicip, setNames, colnames)

coordDF <- bind_rows(splitMunicip)
coordDF <-  coordDF[rowSums(is.na(coordDF)) == 0,]     # Remove empty rows

#######################################################################################

# For Priorities graph, create dataset that gives the total proportions of each category

# priorities_data <- clean_data
# 
# 
# priorities_long <- melt(priorities_data, id="obs")
# priorities_long <- na.omit(priorities_long) %>%
#   dplyr::rename(., `All priorities` = value) %>%
#   select(3)
# 
# priorities_table <- table(priorities_long)
# priorities_prop <- as.data.frame(prop.table(priorities_table))
# 
# 
# x <- ggplot(priorities_prop, aes(x = priorities_long, y = Freq)) +
#   geom_bar(stat = "identity", fill = " steelblue") + theme_classic() +
#   geom_text(aes(label = round(Freq, 2)),
#             vjust = 1.6,
#             color = " white",
#             size = 3.5)
# x

#######################################################################################
# Timeline data preparation
# 
# # Create new dataframe
# time_df <- clean_data
# 
# # Create new date variable
# time_df$established_1 <- as.Date(ISOdate(time_df$established, 1, 1))
# 
# ## To distinguish between NGOs and Donors
# status_levels <- c("ngo", "donor")
# status_colors <- c("#0070C0", "#00B050")
# 
# time_df$status <- factor(time_df$ngo_or_donor, levels=status_levels, ordered=TRUE)
# 
# ## This is for positioning the vertical lines
# positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
# directions <- c(1, -1)
# 
# line_pos <- data.frame(
#   "established"=unique(time_df$established),
#   "position"=rep(positions, length.out=length(unique(time_df$established))),
#   "direction"=rep(directions, length.out=length(unique(time_df$established)))
# )
# 
# time_df <- merge(x=time_df, y=line_pos, by="established", all = TRUE)
# timeline_df <- time_df[with(time_df, order(established, status)),]
# 
# ## This is to offset the lollipops for orgs that are established on the same month
# text_offset <- 0.05
# time_df$year_count <- ave(time_df$established==time_df$established, time_df$established, FUN = cumsum)
# time_df$text_position <- (time_df$month_count * text_offset * time_df$direction) + time_df$position
# 
# ## This is to create a buffer two years on either side of the earliest and most recently established orgs
# year_buffer <- 2
# year_date_range <- seq(min(time_df$established) - year_buffer, max(time_df$established) + year_buffer, by=1)
# 
# year_df <- data.frame(year_date_range)
# 
# #### Create plot
# 
# timeline_plot <- ggplot(time_df, aes(x=established,y=0, col=status, label=name))
# 
# timeline_plot<-timeline_plot+labs(col="NGO or Donor")
# 
# timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
# 
# timeline_plot<-timeline_plot+theme_classic()
# 
# timeline_plot <- timeline_plot+geom_hline(yintercept=0, 
#                                           color = "black", size=0.3)
# 
# # Plot vertical segment lines for milestones
# 
# timeline_plot<-timeline_plot+geom_segment(data=time_df[time_df$year_count == 1,], aes(y=position,yend=0,xend=established, color='black', size=0.2))
# 
# 
# 
# 
# 
# timeline_plot

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
  dashboardSidebar(fluidRow(
    status = "primary",
    checkboxGroupInput(
      "field_of_work",
      "Field of work",
      choices =  fields,
      selected = fields
    )
  )),
  dashboardBody(fluidRow(box(width = 12,
      title = "Map",
      status = "primary",
      solidHeader = TRUE,
      leafletOutput(outputId = "map")
  )), 
    fluidRow(
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
    ),
    box(width = 12,
        title = "Timeline",
        status = "primary",
        solidHeader = TRUE,
        timevisOutput("timeline"))
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
  
  # Map
  info_data <- reactive({
    selectedData()[, c(1, 3:5)]
  })
  
  coordDF_1 <- reactive({
    merge(coordDF, info_data())
  })
  
  info <- reactive({
    paste(sep = "<br/>",
                paste0("<b><a href='",coordDF_1()$website, "'>", coordDF_1()$name, "</a></b>"),
                paste("Est.", coordDF_1()$established)
)
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = as.numeric(coordDF_1()$Longitude), lat = as.numeric(coordDF_1()$Latitude), 
                 popup = info())
  })
  
  # Histogram
  output$priorities_plot <- renderPlot({
    priorities_prop <- selectedData()[, c(grep("Priority|obs", names(selectedData()))), drop = F]
    
    priorities_long <- melt(priorities_prop, id="obs")
    priorities_long <- na.omit(priorities_long) %>%
      dplyr::rename(., `All priorities` = value) %>%
      select(3) 
    
    max_prop <- max(priorities_prop$Freq*100)
    max_prop <- round(max_prop+30, -1)
    
    priorities_prop <- priorities_long %>%
      group_by(`All priorities`) %>%
      summarize(count = n()) %>%
      mutate(pct = round(((count/sum(count))*100)),1)
    
    max_prop <- max(priorities_prop$pct)
    max_prop <- round(max_prop+20, -1)
    
    priorities_plot <- ggplot(priorities_prop, aes(`All priorities`, pct)) + geom_bar(stat = 'identity', fill="darkblue") +
      scale_y_continuous("Proportion of NGOs that have as a priority (%)", limits = c(0,max_prop)) + 
      scale_x_discrete(
        labels = function(labels) {
          sapply(seq_along(labels), function(i)
            paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        }) +
      geom_text(
        aes(label = paste0(pct, "%")),
        vjust = 1.6,
        color = "white",
        size = 3.5
      ) + xlab("Priorities")
    priorities_plot
  })
  
  
  # Wordcloud
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
  
  # Timeline
  output$timeline <- renderTimevis({
    time_df <- select(selectedData(), name, established) %>%
      dplyr::rename(content=name, start=established)
    timeline <- timevis(time_df)
    timeline
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
