library(shiny)
library(data.table)
library(tidyverse)
library(plotly)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(openxlsx)
library(shinycssloaders)
library(stringr)
library(sp)
library(leaflet.extras)
library(htmltools)
library(ggplot2)
library(shinycssloaders)
library(R6)
library(stringr)

treesDF <- as.data.table(readRDS("./data/trees.rds"))
tree_agg <- as.data.table(readRDS("./data/tree_agg.rds"))

accidentsDF <- as.data.table(readRDS("./data/accidentsCleaned.rds"))
petitionsDF <- as.data.table(readRDS("./data/petitions.rds"))
bikesDF <- as.data.table(readRDS("./data/bikes.rds"))
wifiDF <- as.data.table(readRDS("./data/wifi.rds"))

source("classes.R")
source("texts.R")

accidents <- myModule$new(id = "Accidents")
bikes <- myModule$new(id = "Bike stations")
trees <- myModule$new(id = "Trees")
petitions <- myModule$new(id = "Petitions")
wifi <- myModule$new(id = "Wi-Fi")

accidents$icons <- awesomeIcons(icon = 'glyphicon-alert', iconColor = "#000000", library = 'glyphicon', markerColor = "orange")
bikes$icons <- awesomeIcons(icon ='glyphicon-upload', iconColor = "#000000", library ='glyphicon', markerColor = "beige")
petitions$icons <- awesomeIcons(icon ='glyphicon-envelope', iconColor = "#000000", library = 'glyphicon', markerColor = "blue")
wifi$icons <- awesomeIcons(icon ='glyphicon-stats', iconColor = "#000000", library = 'glyphicon', markerColor = "blue")
trees$icons <- awesomeIcons(icon ='glyphicon-tree-conifer', iconColor = "#000000", library = "glyphicon", markerColor = "green")

accidents$text <- text_accidents
bikes$text <- text_bikes
trees$text <- text_trees
petitions$text <- text_petitions
wifi$text <- text_wifi 

ui <- dashboardPage(
    skin = "yellow",
    dashboardHeader(
      title = "Barcelona"
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Homepage", tabName = "home", icon = icon(name = "home")),
        
        menuItem("Transport", tabName = "Transport", icon = icon(name = "bus"),
                 menuSubItem(
                   "Accidents",
                   tabName = 'accidents', icon = icon(name = "car-crash")),
                 menuSubItem(
                   "Bike stations",
                   tabName = 'bikes', icon = icon(name = "biking"))
        ),
        menuItem("Environment", tabName = "Environment", icon = icon(name = "leaf"),
                 menuSubItem(
                   "Trees",
                   tabName = 'trees', icon = icon(name = "tree"))
                 # menuSubItem(
                 #   "Air quality stations",
                 #   tabName = 'air', icon = icon(name = "weight"))
                 ),
        
        menuItem("City and services", tabName = "services", icon = icon(name = "building"),
                 menuSubItem(
                   "Complaints & suggestions",
                   tabName = 'petitions', icon = icon(name = "envelope-open")),
                 menuSubItem(
                   "Wi-fi hotspots",
                   tabName = 'wifi', icon = icon(name = "wifi"))
                 )
      )
    ),
    dashboardBody(
      tags$head(
      includeCSS("./www/my_css.css")
      ),
      
      tabItems(
        tabItem(
          tabName = "home",
          fluidRow(valueBox("1.62 m", "Total population of the city of Barcelona in 2019 (Eurostat)",
                            icon = icon("male"), color = "yellow"),
                   valueBox("101.9 km²", "Total area of the city of Barcelona", icon = icon("map"), color = "red"),
                   valueBox("9.47 m", "Number of tourists staying in hotels in Barcelona in 2019 (Statista.com)", icon = icon("hiking"), color = "aqua"),
                   valueBox(format(length(unique(petitionsDF$ID)), big.mark = " "), "Number petitions and complaints received in 2021 (till March 31)", icon = icon("envelope-open"), color = "blue"),
                   valueBox(format(nrow(treesDF), big.mark = " "), "Number of trees registered in Barcelona (2019)", icon = icon("tree"), color = "green"), 
                   valueBox(length(unique(bikesDF$id)), "Number of bike stations in Barcelona (2019)", icon = icon("biking"), color = "olive"), 
                   valueBox(length(unique(wifiDF$register_id)), "Number of wi-fi hotspots in Barcelona (2019)", icon = icon("wifi"), color = "light-blue"), 
                   valueBox(format(nrow(accidentsDF), big.mark = " "), "Number of accidents registered in 2020", icon = icon("car"), color = "orange"),
                   valueBox("75%", "of residents speak Catalan language actively (Barcelona.de)", icon = icon("comments"), color = "purple"))
        ),
        tabItem(
        tabName = "trees",
          fluidRow(
            trees$ui_part(),
            box(title = "Filter",status = "warning", solidHeader = TRUE,
                pickerInput(inputId = "tree_type", 
                                  label = "Tree specie", 
                                  choices = tree_agg$nom_catala,
                                  #selected = unique(tree_agg$nom_catala), 
                                  multiple = T,
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Deselect all", 
                                                 `select-all-text` = "Select all", 
                                                 `none-selected-text` = "Select tree specie",
                                                 `selected-text-format` = paste0("count > ", 5))), 
                pickerInput("trees_district", 
                            label = "District", 
                            choices = sort(unique(treesDF$District)), 
                            selected = unique(treesDF$District),
                            multiple = T,
                            options = list(`actions-box` = TRUE, 
                                           `deselect-all-text` = "Deselect all", 
                                           `select-all-text` = "Select all", 
                                           `none-selected-text` = "Select district",
                                           `selected-text-format` = paste0("count > ", 5))),
                width = 3)
          )
        ),
        tabItem(
          tabName = "accidents",
            fluidRow(
              accidents$ui_part(),
                     box(title = "Filter",status = "warning", solidHeader = TRUE,
                       pickerInput(inputId = "day_of_week", 
                                    label = "Day of a week", 
                                    choices = c("Monday", "Tuesday", "Wednesday", 
                                                "Thursday", "Friday", "Saturday", 
                                                "Sunday"), 
                                    selected = unique(accidentsDF$day_name), 
                                    multiple = T,
                                    options = list(`actions-box` = TRUE, 
                                                   `deselect-all-text` = "Deselect all", 
                                                   `select-all-text` = "Select all", 
                                                   `none-selected-text` = "Select day",
                                                   `selected-text-format` = paste0("count > ", 3))),
                    pickerInput("accidents_district", 
                                   label = "District", 
                                   choices = sort(unique(accidentsDF$district_name)), 
                                   selected = unique(accidentsDF$district_name),
                                   multiple = T,
                                   options = list(`actions-box` = TRUE, 
                                                  `deselect-all-text` = "Deselect all", 
                                                  `select-all-text` = "Select all", 
                                                  `none-selected-text` = "Select district",
                                                  `selected-text-format` = paste0("count > ", 5))),
                    pickerInput("accidents_month", 
                                   label = "Month", 
                                   choices = levels(factor(unique(accidentsDF$month_name), level=month.name)), 
                                   selected = levels(factor(unique(accidentsDF$month_name), level=month.name)),
                                   multiple = T,
                                   options = list(`actions-box` = TRUE, 
                                                  `deselect-all-text` = "Deselect all", 
                                                  `select-all-text` = "Select all", 
                                                  `none-selected-text` = "Select month",
                                                  `selected-text-format` = paste0("count > ", 5))),
                  selectInput("accidents_fatality", 
                                   label = "Result of accident", 
                                   choices = c("All accidents" = "all",
                                               "Accidents with fatalities" = "dead", 
                                               "Accidents with no fatalities" = "no_dead")), 
                  width = 3
              
              )
        
              
            )
            
        ),
        tabItem(
          tabName = "petitions",
            fluidRow(
              petitions$ui_part(),
              box(title = "Filter", status = "warning", solidHeader = TRUE,
                     pickerInput("petitions_district",
                                          label = "District",
                                          choices = sort(unique(petitionsDF$district)),
                                          selected = unique(petitionsDF$district),
                                          multiple = T,
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "Deselect all",
                                                         `select-all-text` = "Select all",
                                                         `none-selected-text` = "Select district",
                                                         `selected-text-format` = paste0("count > ", 5))),
                     pickerInput("petitions_type",
                                          label = "Type of inquiry",
                                          choices = sort(unique(petitionsDF$Type)),
                                          selected = unique(petitionsDF$Type),
                                          multiple = T,
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "Deselect all",
                                                         `select-all-text` = "Select all",
                                                         `none-selected-text` = "Select district",
                                                         `selected-text-format` = paste0("count > ", 5))),
                     pickerInput("petitions_subject",
                                          label = "Subject of inquiry",
                                          choices = sort(unique(petitionsDF$Subject)),
                                          selected = unique(petitionsDF$Subject),
                                          multiple = T,
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "Deselect all",
                                                         `select-all-text` = "Select all",
                                                         `none-selected-text` = "Select district",
                                                         `selected-text-format` = paste0("count > ", 5))),
                     pickerInput("petitions_month",
                                          label = "Month",
                                          choices = levels(factor(unique(petitionsDF$month_open), level=month.name)),
                                          selected = levels(factor(unique(petitionsDF$month_open), level=month.name)),
                                          multiple = T,
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "Deselect all",
                                                         `select-all-text` = "Select all",
                                                         `none-selected-text` = "Select month",
                                                         `selected-text-format` = paste0("count > ", 5))),

              width = 3)

            )
        ),
        tabItem(
          tabName = "bikes",
            fluidRow(
              bikes$ui_part(),
              box(title = "Filter", status = "warning", solidHeader = TRUE,
                     pickerInput("bike_type",
                                          label = "Type of bicycle",
                                          choices = sort(unique(bikesDF$type)),
                                          selected = unique(bikesDF$type),
                                          multiple = T,
                                          options = list(`actions-box` = TRUE,
                                                         `deselect-all-text` = "Deselect all",
                                                         `select-all-text` = "Select all",
                                                         `none-selected-text` = "Select bicycle type")),
                  sliderInput("slider_bikes", label = "Slots available", min = min(bikesDF$slots_total), max = max(bikesDF$slots_total),
                              value = c(min(bikesDF$slots_total), max(bikesDF$slots_total)), step = 1),
                  width = 3)

          )
        ),
        tabItem(
          tabName = "wifi",
            fluidRow(
              wifi$ui_part(),
              box(title = "Filter", status = "warning", solidHeader = TRUE,
                  pickerInput("wifi_district", 
                              label = "District", 
                              choices = sort(unique(wifiDF$addresses_district_name)), 
                              selected = unique(wifiDF$addresses_district_name),
                              multiple = T,
                              options = list(`actions-box` = TRUE, 
                                             `deselect-all-text` = "Deselect all", 
                                             `select-all-text` = "Select all", 
                                             `none-selected-text` = "Select district",
                                             `selected-text-format` = paste0("count > ", 5))),
                  width = 3)

              )
          )
        )

    )
  )
                 


server <- function(input, output, session) {
  
  ##########DATA########
  accidents$data <- reactive({
    
    if(input$accidents_fatality == "dead"){
      accidentsDF <- accidentsDF[dead >= 1]
    }else if(input$accidents_fatality == "no_dead"){
      accidentsDF <- accidentsDF[dead < 1]
    }
    
    accidentsDF <- accidentsDF[district_name %in% input$accidents_district & 
                month_name %in% input$accidents_month & 
                day_name %in% input$day_of_week,]
    

    accidentsDF
    
  })
  
  
  petitions$data <- reactive({
    
    
    petitionsDF[!(is.na(lat)) & !(is.na(lon)) & district %in% input$petitions_district & 
                month_open %in% input$petitions_month & 
                Type %in% input$petitions_type &
                Subject %in% input$petitions_subject,]
  })
  
  
  
  bikes$data <- reactive({bikesDF[type %in% input$bike_type &
                                 dplyr::between(slots_total, left = input$slider_bikes[1], right = input$slider_bikes[2]),]})
  
  
  trees$data <- reactive({treesDF[nom_catala %in% input$tree_type &
                                  District %in% input$trees_district,]})
  
  wifi$data <- reactive({wifiDF[addresses_district_name %in% input$wifi_district,]})
  
  ######Plots and maps#####
  
  ##Accidents
  
  accidents$render_map()
  
  accidents$render_plot1(
    sum_var = "victims",
    grouped_var = "month_name",
    xlab = "Month",
    ylab = "No. of Accidents",
    title = "Monthly accidents"
  )
  
  accidents$render_plot2(
    sum_var = "victims",
    grouped_var = "day_name",
    xlab = "Day",
    ylab = "No. of Accidents",
    title = "Accidents by weekday"
  )

  ##Bikes
  
  bikes$render_map(makeCluster = F)
  
  bikes$render_plot1(
    sum_var = "id",
    grouped_var = "type",
    xlab = "Type",
    ylab = "No. of bike stations",
    title = "Bike stations by type",
    FUN = n_distinct
  )
  
  ##Trees

  trees$render_heatmap()
  
  trees$render_plot1(
    sum_var = "codi",
    grouped_var = "District",
    xlab = "District",
    ylab = "No. of Trees",
    title = "Trees by district",
    FUN = n_distinct
  )
  
  trees$render_plot2(
    sum_var = "codi",
    grouped_var = "Tree specie",
    xlab = "Tree specie",
    ylab = "No. of Trees",
    title = "Trees by specie",
    FUN = n_distinct
  )
  
  ##Petitions
  
  petitions$render_map()
  
  petitions$render_plot1(
    sum_var = "ID",
    grouped_var = "Area",
    xlab = "Area",
    ylab = "No. of complaints",
    title = "Complaints by field",
    FUN = n_distinct
  )
  
  petitions$render_plot2(
    sum_var = "ID",
    grouped_var = "support",
    xlab = "Submitted via",
    ylab = "No. of complaints",
    title = "Channels for submitting complaints",
    FUN = n_distinct
  )
  
  ##Wifi
  
  wifi$render_map(makeCluster = F)
  
  wifi$render_plot1(
    sum_var = "register_id",
    grouped_var = "addresses_district_name",
    xlab = "District",
    ylab = "No. of Wi-fi Hotspots",
    title = "Wi-fi Hotspots by district",
    FUN = n_distinct
  )
  
  



}

shinyApp(ui, server)
