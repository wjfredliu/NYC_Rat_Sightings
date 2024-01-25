library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(lubridate)
library(tidyverse)
library(highcharter)
library(leaflet.extras)
library(sf)
library(httr)
library(jsonlite)

# Load dataset
rat_sightings <- read.csv("data/Rat_Sightings_20231121.csv")

# Extract years from the data and sort them
rat_sightings <- rat_sightings %>%
  mutate(Date = as.Date(`Created.Date`, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% 3:5 ~ "Spring",
      Month %in% 6:8 ~ "Summer",
      Month %in% 9:11 ~ "Fall",
      Month %in% c(12, 1, 2) ~ "Winter",
      TRUE ~ "Unknown"
    )
  )

years <- sort(unique(rat_sightings$Year))

ui <- fluidPage(
  theme = shinytheme("flatly"),  # Enhanced UI theme
  navbarPage("NYC Rat Sightings", id = "navbar",
             tabPanel("Map",
                      fluidRow(
                        column(3,
                               selectInput("borough", "Borough", choices = c("All", unique(rat_sightings$Borough)), selected = 'MANHATTAN'),
                               selectInput("year_map", "Year", choices = years, selected = max(years)),
                               helpText("Explore rat sightings in NYC on the map. Select a borough and year to view specific data.")
                        ),
                        column(9, leafletOutput("map"))
                      )
             ),
             tabPanel("About",
                      h4("Project Inspiration"),
                      p("A loved one lives in New York City and is extremely afraid of rats. As a fun project, I decided 
                        to visualize rat sightings throughout the years to help them see areas where rats frequent.
                        Please note this data was last updated in November 2023")
             ),
             tabPanel("Trends",
                      fluidRow(
                        column(3,
                               sliderInput("year_slider", "Year",
                                           min = min(years), max = max(years),
                                           value = max(years), step = 1,
                                           sep = "", pre = " "),
                               helpText("Look at yearly rat sightings in NYC over time.")
                        ),
                        column(9, highchartOutput("rat_trend"))
                      )
             ),
             tabPanel("Insights",
                      h4("Key Insights"),
                      p("Interactive summaries and analysis of rat sighting trends. In this data, you can filter and get a breakdown 
                        of the rat sightings over the years and their specific boroughs"),
                      br(),
                      selectInput("year_insights", "Select Year", choices = years, selected = max(years)),
                      selectInput("borough_insights", "Select Borough", 
                                  choices = c("All", unique(rat_sightings$Borough)), 
                                  selected = "All"),
                      fluidRow(
                        column(4, strong("Total Sightings:"), textOutput("totalSightings")),
                        column(4, strong("Average Sightings per Month:"), textOutput("averageSightings")),
                        column(4, strong("Most Active Month:"), textOutput("mostActiveMonth"))
                      ),
                      br(), 
                      br(),
                      p("Consider prioritizing pest control measures in areas with high rat sightings. The summer months seem to 
                        have the most rat sightings, which checks out since I swore I always see way more rats in the summer. I'm glad I have data to back that up now.
                        Rodents are apparently more active during the summer and spring since they're
                        actively breeding and looking for food.")
             )
  )
)

# Define the server
server <- function(input, output, session) {
  # Load GeoJSON data
  nyc_boroughs <- st_read("data/new-york-city-boroughs.geojson") %>%
    st_make_valid()  # Fix invalid geometries
  label_points <- st_point_on_surface(nyc_boroughs)
  
  # Convert label points to a dataframe
  label_points_df <- st_coordinates(label_points) %>% 
    as.data.frame() %>%
    mutate(name = nyc_boroughs$name)  # Adding names for labels
  
  # Filter data for the map
  filteredMapData <- reactive({
    data <- rat_sightings %>%
      mutate(
        Date = as.Date(`Created.Date`, format = "%m/%d/%Y %I:%M:%S %p"),
        Year = year(Date),
        Longitude = as.numeric(Longitude),
        Latitude = as.numeric(Latitude)
      ) %>%
      filter(
        !is.na(Longitude), !is.na(Latitude),  # Remove NA values
        Longitude >= -180, Longitude <= 180,  # Ensure valid longitude range
        Latitude >= -90, Latitude <= 90       # Ensure valid latitude range
      )
    
    if(input$borough != "All") {
      data <- data %>% filter(Borough == input$borough)
    }
    
    data <- data %>% filter(Year == input$year_map)
    data
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -74.0060, lat = 40.7128, zoom = 10)
  })
  
  observe({
    data <- filteredMapData()  # filter data
    
    leafletProxy("map", data = data) %>%
      clearGroup(group = "heat") %>%
      addTiles() %>%
      addHeatmap(
        lng = ~Longitude, lat = ~Latitude, 
        intensity = ~1,
        blur = 25, max = 0.8, radius = 20,
        gradient = c("0.4" = "blue", "0.65" = "green", "1" = "red"),
        group = "heat"
      ) %>%
      addPolygons(data = nyc_boroughs, fillColor = NA, color = "#444444", weight = 2, opacity = 1, group = "boundaries")
    
    if(nrow(label_points_df) > 0) {
      for(i in 1:nrow(label_points_df)) {
        leafletProxy("map", data = label_points_df) %>%
          addMarkers(
            lng = label_points_df[i, "X"], 
            lat = label_points_df[i, "Y"],
            label = as.character(label_points_df[i, "name"]),
            labelOptions = labelOptions(noHide = TRUE, direction = 'auto')
          )
      }
    }
  })
  
  filteredTrendData <- reactive({
    data <- rat_sightings %>%
      mutate(Year = year(as.Date(`Created.Date`, format = "%m/%d/%Y %I:%M:%S %p"))) %>%
      filter(Year == input$year_slider) %>%
      group_by(Month) %>%
      summarise(Sightings = n())
    
    data
  })
  
  output$rat_trend <- renderHighchart({
    trend_data <- reactive({
      rat_sightings %>%
        filter(Year == input$year_slider) %>%
        count(Month) %>%
        complete(Month = 1:12, fill = list(n = 0)) %>%
        mutate(Month = factor(month.abb[Month], levels = month.abb))  #proper month order
    })

    output$rat_trend <- renderHighchart({
      data <- trend_data()  
      req(nrow(data) > 0) 
      
      highchart() %>%
        hc_chart(type = 'spline') %>%  # smooth line
        hc_title(text = "Monthly Rat Sightings Trends") %>%
        hc_xAxis(categories = levels(data$Month)) %>%
        hc_yAxis(title = list(text = "Number of Sightings")) %>%
        hc_add_series(
          name = "Sightings",
          data = data$n,
          color = '#00AEEF'
        ) %>%
        hc_legend(enabled = TRUE)
    })
  })
  output$totalSightings <- renderText({
    if (input$borough_insights == "All") {
      total_sightings <- nrow(rat_sightings %>%
                                filter(Year == input$year_insights))
    } else {
      total_sightings <- nrow(rat_sightings %>%
                                filter(Year == input$year_insights, 
                                       Borough == input$borough_insights))
    }
    total_sightings
  })
  
  output$averageSightings <- renderText({
    if (input$borough_insights == "All") {
      avg_sightings <- rat_sightings %>%
        filter(Year == input$year_insights) %>%
        group_by(Month) %>%
        summarise(Sightings = n(), .groups = 'drop')
    } else {
      avg_sightings <- rat_sightings %>%
        filter(Year == input$year_insights, Borough == input$borough_insights) %>%
        group_by(Month) %>%
        summarise(Sightings = n(), .groups = 'drop')
    }
    mean(avg_sightings$Sightings)
  })
  output$mostActiveMonth <- renderText({
    if (input$borough_insights == "All") {
      active_month <- rat_sightings %>%
        filter(Year == input$year_insights) %>%
        group_by(Month) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))
    } else {
      active_month <- rat_sightings %>%
        filter(Year == input$year_insights, Borough == input$borough_insights) %>%
        group_by(Month) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))
    }
    
    if (nrow(active_month) > 0) {
      most_active_month_num <- active_month %>%
        slice(1) %>%
        pull(Month)
      
      # convert numeric month to month name
      month.name[most_active_month_num]
    }
  })
  output$boroughBreakdown <- renderTable({
    if (input$borough_insights == "All") {
      borough_data <- rat_sightings %>%
        filter(Year == input$year_insights) %>%
        group_by(Borough) %>%
        summarise(Sightings = n(), .groups = 'drop') %>%
        arrange(desc(Sightings))
    } else {
      borough_data <- rat_sightings %>%
        filter(Year == input$year_insights, Borough == input$borough_insights) %>%
        group_by(Borough) %>%
        summarise(Sightings = n(), .groups = 'drop') %>%
        arrange(desc(Sightings))
    }
    borough_data
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)