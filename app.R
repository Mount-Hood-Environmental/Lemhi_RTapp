# Author: Kevin See & Mike Ackerman
# Purpose: create radio telemetry Shiny example
# Created: 5/25/2018
# Last Modified: 5/25/2018
# Notes: This was originally created by Kevin and Mike with Biomark and is now hosted and maintained
# by Bryce Oldemeyer with MHE

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)

# setwd('R/Visualization/Shiny')

load('ExampData.rda')

sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}

#-----------------------------------------------------------------
# Shiny app
#-----------------------------------------------------------------

ui = fluidPage(
  # theme = shinytheme('sandstone'),
  theme = shinytheme('simplex'),
  # themeSelector(),
  titlePanel(fluidRow(column(width = 8, strong("Lemhi River Chinook Presmolt Winter Movement and Distribution"), style = "font-size:40px;"), 
                      column(width = 2, tags$a(tags$img(src = "MHE_logo.jpg", height = 70, width = 220, align = "right"),  href="https://mthoodenvironmental.com/")),
                      column(width = 2, tags$a(tags$img(src = "Biomark_Logo.png", height = 70, width = 220), href = "https://www.biomark.com/")))
  ),
  sidebarLayout(
    sidebarPanel2(fluid = FALSE,
      sliderInput(inputId = 'time', 
                  label = 'Date',
                  min = min(tagLoc$dateTime),
                  max = max(tagLoc$dateTime),
                  value = min(tagLoc$dateTime),
                  # time step in seconds (default could be 60*60*4 = 4 hours)
                  step = 60*60*4,
                  width = '100%',
                  timezone = '+0000',
                  animate = animationOptions(interval = 100,
                                             loop = T),
                  timeFormat = '%b %d, %Y'),

      radioButtons(
        inputId = "radio",
        label = "Tag Selection:",
        choices = list(
          "All",
          "Select Manually"
        ),
        selected = "All"),

      conditionalPanel(
        condition = "input.radio != 'All'",
        pickerInput('tag',
                    'Tag',
                    choices = unique(tagLoc$tag_id),
                    selected = unique(tagLoc$tag_id),
                    multiple = T,
                    options = list(`actions-box` = TRUE,
                                   `none-selected-text` = 'Please select at least one tag',
                                   `live-search` = T))
      ),
      out = h4("This web application visualizes preliminary data for Chinook salmon presmolt
               winter movement and distribution in the Lemhi River, ID.",tags$br(), tags$br(), 
               
               "Primary developers & contacts: ",tags$br(), 
               "Kevin See, Kevin.See@dfw.wa.gov",tags$br(), 
               "Mike Ackerman, Mikea@nezperce.org", tags$br(), tags$br(), 
               
               "Site maintainer:",tags$br(), 
               "Bryce Oldemeyer, Bryce.Oldemeyer@mthoodenvironmental.com")
      
    ),
    mainPanel(
      leafletOutput('mymap',
                    height = 600),
      
      tags$hr(),
      tags$h3('Tag Data'),
      
      DTOutput('tagData')
    )
  )
)

server <- function(input, output, session) {
  points <- reactive({
    if(input$radio == "All"){
      tagLoc %>% 
        filter(dateTime == input$time)
    } else {
      if(length(input$tag) == 0) {
        tagLoc %>% 
          filter(dateTime == input$time,
                 is.na(lat)) %>%
          slice(1)
        
        # tagLoc %>% 
        #   filter(dateTime == input$time) %>%
        #   slice(1) %>%
        #   mutate(lat = NA,
        #          long = NA)
      } else {
        tagLoc %>% 
          filter(dateTime == input$time) %>%
          filter(tag_id %in% input$tag)
      }
    }
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -115.315,
              lat = 45.598,
              zoom = 8) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(data = rtSites,
                 lng = ~ LONGITUDE,
                 lat = ~ LATITUDE,
                 label = ~ SiteCode,
                 popup = ~ SiteCode,
                 icon = list(iconUrl = 'www/antenna3.png',
                             iconSize = c(16, 16)),
                 labelOptions = labelOptions(style = list(color = 'orange')),
                 group = 'RT Sites') %>%
      addScaleBar(position = 'bottomleft',
                  options = scaleBarOptions(metric = T,
                                            imperial = F,
                                            maxWidth = 200)) %>%
      addMiniMap(position = 'topright',
                 # find options here: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
                 tiles = 'Stamen.TonerBackground') %>%
      addEasyButton(easyButton(icon = 'fa-globe',
                               title = 'Reset View',
                               onClick = JS("function(btn, map){ map.setView([45.598, -115.315], 8);}")))
    
  })
  
  
  observeEvent(input$time,{
    
    leafletProxy('mymap') %>%
      clearGroup(group = 'Tags') %>%
      addMarkers(data = points(),
                 lng = ~ long,
                 lat = ~ lat,
                 icon = list(iconUrl = 'www/fish2.png',
                             iconSize = c(24, 24)),
                 label = ~ tag_id,
                 group = 'Tags')
    
  })
  
  output$tagData <- renderDT({
    DT::datatable(tagDf %>%
                    select(Tag = tag_id,
                           PIT = PITTagNum,
                           DutyCycle,
                           Release:Weight) %>%
                    mutate(Release = as.Date(floor_date(Release, unit = 'days'))),
                  # options = list(lengthMenu = c(5, 15, 30, 50, n_distinct(tagLoc$tag_id)), pageLength = 5),
                  filter = 'top',
                  rownames = F)
    })
  
}

shinyApp(ui, server)

