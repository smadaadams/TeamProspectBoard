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
                  )#,
                  # column(3,
                  #    radioGroupButtons(
                  #      inputId = "listview",
                  #      label = "", 
                  #      choices = c("Table View", "Report View"),
                  #      selected = "Table View",
                  #      status = "success"
                  #    )
                  # )
                ),
                
                # materialSwitch(inputId = "switch", label = "List View", status = "danger"),
                #  div(style="display:inline-block; margin-right: 25px",
                #      tags$b("Mobile View"), 
                #      switchInput(inputId = "mobileview", value = FALSE, inline = TRUE)), 
                #      br(),
                # div(style="display:inline-block; margin-right: 25px",
                #      tags$b("Report View"), 
                #      switchInput(inputId = "listview", value = FALSE, inline = TRUE)
                #  ),
                br(),
                fluidRow(
                  column(12,
                         div(style="display:inline-block; padding-right: 25px",downloadButton("downloadList","Download List", class="number"))#,
                         # div(style="display:inline-block;",actionLink("def", "Glossary")),
                         #         uiOutput("defUI")
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
                # withSpinner(
                #   reactableOutput("mockDraft2")
                # ),
                # uiOutput("loadUI")
                
)