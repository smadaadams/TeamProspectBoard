library(shiny)
library(tidyverse)
library(reactable)
library(htmltools)
library(shinyWidgets)
library(shinycssloaders)

ui <- fluidPage(theme = "theme.css",
                
                tags$head(
                  tags$style(HTML("hr {border-top: 2px solid #000000;}"))
                ),  
                fluidRow(
                  column(12,
                         radioGroupButtons(
                           inputId = "mobileview",
                           label = "", 
                           choices = c("Desktop View", "Mobile View"),
                           selected = "Mobile View",
                           status = "primary",
                           checkIcon = list(
                             yes = tags$i(class = "fa fa-check-square", 
                                          style = "color: white"),
                             no = tags$i(class = "fa fa-square-o", 
                                         style = "color: white"))
                         )
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                         div(style="display:inline-block; padding-right: 25px",
                             downloadButton("downloadList","Download List", class="number"))

                  )
                ),
                br(),
                fixedRow(
                  column(12,
                         radioGroupButtons("position","Filter Position",c("All","Pitchers","Hitters","C","1B","2B","SS","3B","OF"))
                  )
                ),
                hr(),
                withSpinner(
                  reactableOutput("board")
                ),
                br()
                
)