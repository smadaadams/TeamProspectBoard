library(lubridate)

boardName <- "Prospects Live Mariners Board.csv"

server <- function(input, output, session) {
  
  boardData <- read.csv("data/MarinersBoard_11-10-20.csv", stringsAsFactors = F) 
  
  boardData <- boardData %>% 
    mutate(RankLong = paste0(Rank,". ",Name," - ",Position), 
           Rank=as.numeric(Rank),
           Age = floor(interval(mdy(boardData$Birthday),as.Date("2021-07-01")) / duration(num = 1, units = "years"))
    ) %>% 
    mutate_if(is.logical, as.character) %>% 
    mutate(
      across(everything(), ~replace_na(.x, ""))
    )
  
  draftDataR <- reactive({
    if(input$position != "All"){
      if(input$position == "Pitchers"){
        boardData <- boardData %>% 
          filter(Position %in% c("RHP","LHP")) 
      } else if(input$position == "Hitters"){
        boardData <- boardData %>% 
          filter(! Position %in% c("RHP","LHP")) 
      } else {
        boardData <- boardData %>% 
          filter(grepl(input$position, as.character(Position))) 
      }
    } else {
      boardData
    }
  })
  
  tableDataColumns <- boardData %>% 
    select(Role.D, Video1, Rank, Name, Position, OFP, Risk, Age, Highest.Level, ETA, Hit,Power,Field,Arm,Run,
           FB,CT,CB,SL,CH,SLV,SPL,FK,KN,SCR,PLM,Control,Command)
  tableDataColumns <- tableDataColumns[!sapply(tableDataColumns, function(x) all(x == ""))]
  
  #######       #######
  ####### TABLE #######
  #######       #######
  
  
  output$board <- renderReactable({
    
    if(input$mobileview == "Mobile View"){
      
      boardDataTable <- draftDataR() %>% 
        select(Role.D, RankLong, OFP, Tools)#, Rank.Icon, Rank.Change)
      
      reactable(boardDataTable,
                compact = TRUE, 
                pagination = FALSE,
                bordered = FALSE,
                style = list(fontSize=12),
                defaultExpanded = FALSE,
                defaultColDef = colDef(headerClass = "my-header", align = "left"),
                
                columns = list(
                  Role.D = colDef(name = "",
                                  maxWidth = 63, align="center",
                                  cell = function(value,index) {
                                    reporticon <- icon("clipboard", class = "fa-lg iconReport", lib = "font-awesome")
                                    reporticon
                                  },
                                  details = function(index) {
                                    div(style = "padding: 10px",
                                        hr(),
                                        tags$table(
                                          tags$tr(
                                            tags$td(padding = "15px",
                                                    img(src = draftDataR()$HeadshotURL[index], height = "150px")
                                            ),
                                            tags$td(class="bio", width="100%", style="padding: 15px",
                                                    tags$b(draftDataR()$Name[index]),
                                                    hr(class="hr2"),
                                                    paste("Age:",draftDataR()$Age[index],"yr"), br(),
                                                    paste("Height:",draftDataR()$Height[index]), br(),
                                                    paste("Weight:",draftDataR()$Weight[index],"lbs"), br(),
                                                    paste("Hits/Throws:", draftDataR()$HitThrow[index]), br(),
                                                    paste0("Acquired: ",draftDataR()$Aquired[index]), br(),
                                                    paste("RuleV:", draftDataR()$RuleV[index]), br()
                                            )
                                          )
                                        ),
                                        br(),
                                        # hr(class="hr1"),
                                        
                                        tabsetPanel(
                                          tabPanel(div(class="iconReport"," Report"), icon = icon("clipboard", class = "fa-lg iconReport", lib = "font-awesome"),
                                                   div(class="blurb",
                                                       br(),
                                                       tags$p(
                                                         tags$b("Physical Description: "),
                                                         draftDataR()$Phy.D[index]),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$Hit.D[index]!=""){
                                                           tags$b("Hit: ")
                                                         },
                                                         draftDataR()$Hit.D[index], tags$b(
                                                           if(draftDataR()$Hit.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$Hit[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$Pow.D[index]!=""){
                                                           tags$b("Power: ")
                                                         },
                                                         draftDataR()$Pow.D[index], tags$b(
                                                           if(draftDataR()$Pow.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$Power[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$Field.D[index]!=""){
                                                           tags$b("Field: ")
                                                         },
                                                         draftDataR()$Field.D[index], tags$b(
                                                           if(draftDataR()$Field.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$Field[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$Arm.D[index]!=""){
                                                           tags$b("Arm: ")
                                                         },
                                                         draftDataR()$Arm.D[index], tags$b(
                                                           if(draftDataR()$Arm.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$Arm[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$Run.D[index]!=""){
                                                           tags$b("Run: ")
                                                         },
                                                         draftDataR()$Run.D[index], tags$b(
                                                           if(draftDataR()$Run.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$Run[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$Mech.D[index]!=""){
                                                           tags$b("Mechanics: ")
                                                         },
                                                         draftDataR()$Mech.D[index]),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$FB.D[index]!=""){
                                                           tags$b("Fastball: ")
                                                         },
                                                         draftDataR()$FB.D[index],tags$b(
                                                           if(draftDataR()$FB.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$FB[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$CT.D[index]!=""){
                                                           tags$b("Cutter: ")
                                                         },
                                                         draftDataR()$CT.D[index],tags$b(
                                                           if(draftDataR()$CT.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$CT[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$CB.D[index]!=""){
                                                           tags$b("Curveball: ")
                                                         },
                                                         draftDataR()$CB.D[index],tags$b(
                                                           if(draftDataR()$CB.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$CB[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$SL.D[index]!=""){
                                                           tags$b("Slider: ")
                                                         },
                                                         draftDataR()$SL.D[index],tags$b(
                                                           if(draftDataR()$SL.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$SL[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$CH.D[index]!=""){
                                                           tags$b("Changeup: ")
                                                         },
                                                         draftDataR()$CH.D[index],tags$b(
                                                           if(draftDataR()$CH.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$CH[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$SLV.D[index]!=""){
                                                           tags$b("Slurve: ")
                                                         },
                                                         draftDataR()$SLV.D[index],tags$b(
                                                           if(draftDataR()$SLV.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$SLV[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$SPL.D[index]!=""){
                                                           tags$b("Splitter: ")
                                                         },
                                                         draftDataR()$SPL.D[index],tags$b(
                                                           if(draftDataR()$SPL.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$SPL[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$FK.D[index]!=""){
                                                           tags$b("Forkball: ")
                                                         },
                                                         draftDataR()$FK.D[index],tags$b(
                                                           if(draftDataR()$FK.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$FK[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$KN.D[index]!=""){
                                                           tags$b("Knuckleball: ")
                                                         },
                                                         draftDataR()$KN.D[index],tags$b(
                                                           if(draftDataR()$KN.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$KN[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$SCR.D[index]!=""){
                                                           tags$b("Screwball: ")
                                                         },
                                                         draftDataR()$SCR.D[index],tags$b(
                                                           if(draftDataR()$SCR.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$SCR[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$PLM.D[index]!=""){
                                                           tags$b("Palmball: ")
                                                         },
                                                         draftDataR()$PLM.D[index],tags$b(
                                                           if(draftDataR()$PLM.D[index]!=""){
                                                             " Grade: "
                                                           },
                                                           draftDataR()$PLM[index])),
                                                       
                                                       tags$p(
                                                         if(draftDataR()$ContCom.D[index]!=""){
                                                           tags$b("Control and Command: ")
                                                         },
                                                         draftDataR()$ContCom.D[index],tags$b(
                                                           if(draftDataR()$ContCom.D[index]!=""){
                                                             " Control: "
                                                           },
                                                           draftDataR()$Control[index],
                                                           if(draftDataR()$ContCom.D[index]!=""){
                                                             " | Command: "
                                                           },
                                                           draftDataR()$Command[index])),
                                                       
                                                       tags$p(
                                                         tags$b("Overall: "),
                                                         draftDataR()$Overall.D[index]),
                                                       
                                                       tags$b(
                                                         "OFP: ",
                                                         draftDataR()$OFP[index],
                                                         br(),
                                                         "Role: ",
                                                         draftDataR()$Role[index],
                                                         " - ",
                                                         draftDataR()$Role.D[index],
                                                         br(),
                                                         tags$b("Risk: "),
                                                         draftDataR()$Risk[index],
                                                         br(),
                                                         br(),
                                                         tags$em(
                                                           "Evaluator: ",
                                                           draftDataR()$Evaluator[index]
                                                         )
                                                       )
                                                   ),
                                                   hr()
                                                   
                                          ),
                                          tabPanel(div(class="iconVid"," Video"), icon = icon("youtube", class = "fa-lg iconVid", lib = "font-awesome"),
                                                   div(class="video",
                                                       br(),
                                                       if(draftDataR()$Video1[index]==""){
                                                         ""
                                                       } else {
                                                         tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video1[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                                       },
                                                       if(draftDataR()$Video2[index]==""){
                                                         ""
                                                       } else {
                                                         tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video2[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                                       },
                                                       if(draftDataR()$Video3[index]==""){
                                                         ""
                                                       } else {
                                                         tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video3[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                                       },
                                                       if(draftDataR()$Video4[index]==""){
                                                         ""
                                                       } else {
                                                         tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video4[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                                       },
                                                       br(),
                                                       br()
                                                   )
                                          )
                                        )
                                    )
                                    
                                  }
                  ),
                  RankLong = colDef(name = "Player", minWidth = 80, maxWidth = 200,
                                    align = "left",
                                    cell = function(value){
                                      div(class="number", value)
                                    }
                  ),
                  OFP = colDef(name = "OFP", maxWidth = 35,
                               align = "center"#,
                               # cell = function(value){
                               #   div(paste(value, "OFP"))
                               # }
                  ),
                  Tools = colDef(minWidth = 90, maxWidth = 300)
                  
                ), 
                
                theme = reactableTheme(borderColor = "white")
      )
    } else {
      boardDataTable <- draftDataR() %>%
        select(colnames(tableDataColumns)) %>% 
        mutate(Rank = as.numeric(Rank))
      
      reactable(boardDataTable,
                #compact = TRUE,
                pagination = FALSE,
                striped = TRUE,
                defaultExpanded = FALSE, 
                searchable = TRUE,
                #bordered=TRUE,
                borderless = FALSE,
                #fullWidth = FALSE,
                columnGroups = list(
                  colGroup(name = "Hitter Grades (Future)", columns = c("Hit","Power","Field","Arm","Run")),
                  colGroup(name = "Pitcher Grades (Future)", columns = colnames(tableDataColumns)[16:length(colnames(tableDataColumns))])
                  
                ),
                defaultColDef = colDef(class = "cell", headerClass = "my-header", align = "center",maxWidth = 38),
                defaultColGroup = colGroup(headerClass = "group-header"),
                style = list(fontSize=11),
                columns = list(
                  Name = colDef(name = "Player", minWidth = 120, maxWidth = 400, align="left",
                                cell = function(value){
                                  div(class="player", value)
                                }
                  ),
                  Rank = colDef(name = "Rk", maxWidth = 40, align = "center", class="border-left"),
                  Position = colDef(name = "Pos", maxWidth = 70, align="left"),
                  Highest.Level = colDef(name = "Highest Level", maxWidth = 65, align="left"),
                  Age = colDef(name = "2021 Age", maxWidth = 45),
                  ETA = colDef(name = "ETA", maxWidth = 43),
                  Power = colDef(name = "Pow", maxWidth = 50),
                  Field = colDef(name = "Field", maxWidth = 50),
                  Arm = colDef(name = "Arm", maxWidth = 43),
                  Hit = colDef(name = "Hit", class="border-left"),
                  Run = colDef(name = "Run", maxWidth = 43),
                  FB = colDef(name = "FB", class="border-left"),
                  Control = colDef(name = "Cont.", maxWidth = 50),
                  Command = colDef(name = "Com.", maxWidth = 50, class="cell border-right"),
                  Risk = colDef(name = "Risk", maxWidth = 65, align="left"),
                  OFP = colDef(name = "OFP", maxWidth = 40),
                  Tools = colDef(name = "Tools", minWidth = 300),
                  Role.D = colDef(name="Report", maxWidth = 63, align="center", class="border-left",
                                  cell = function(value,index) {
                                    reporticon <- icon("clipboard", class = "fa-lg iconReport", lib = "font-awesome")
                                    reporticon
                                  },
                                  details = function(index) {
                                    div(style = "padding: 10px",
                                        #hr(),
                                        tags$table(
                                          tags$tr(
                                            tags$td(padding = "15px", 
                                                    img(src = draftDataR()$HeadshotURL[index], height = "150px")
                                            ),
                                            tags$td(class="bio", width="100%", style="padding: 15px", 
                                                    tags$b(draftDataR()$Name[index]),
                                                    hr(class="hr2"),
                                                    paste("Age:",draftDataR()$Age[index],"yr"), br(),
                                                    paste("Height:",draftDataR()$Height[index]), br(),
                                                    paste("Weight:",draftDataR()$Weight[index],"lbs"), br(),
                                                    paste("Hits/Throws:", draftDataR()$HitThrow[index]), br(),
                                                    paste0("Acquired: ",draftDataR()$Aquired[index]), br(),
                                                    paste("RuleV:", draftDataR()$RuleV[index]), br()
                                            )
                                          )
                                        ),
                                        hr(class="hr1"),
                                        div(class="blurbDesktop",
                                            tags$p(
                                              tags$b("Physical Description: "),
                                              draftDataR()$Phy.D[index]),
                                            
                                            tags$p(
                                              if(draftDataR()$Hit.D[index]!=""){
                                                tags$b("Hit: ")
                                              },
                                              draftDataR()$Hit.D[index], tags$b(
                                                if(draftDataR()$Hit.D[index]!=""){
                                                  " Grade: " 
                                                },
                                                draftDataR()$Hit[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$Pow.D[index]!=""){
                                                tags$b("Power: ")
                                              },
                                              draftDataR()$Pow.D[index], tags$b(
                                                if(draftDataR()$Pow.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$Power[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$Field.D[index]!=""){
                                                tags$b("Field: ")
                                              },
                                              draftDataR()$Field.D[index], tags$b(
                                                if(draftDataR()$Field.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$Field[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$Arm.D[index]!=""){
                                                tags$b("Arm: ")
                                              },
                                              draftDataR()$Arm.D[index], tags$b(
                                                if(draftDataR()$Arm.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$Arm[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$Run.D[index]!=""){
                                                tags$b("Run: ")
                                              },
                                              draftDataR()$Run.D[index], tags$b(
                                                if(draftDataR()$Run.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$Run[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$Mech.D[index]!=""){
                                                tags$b("Delivery & Mechanics: ")
                                              },
                                              draftDataR()$Mech.D[index]),
                                            
                                            tags$p(
                                              if(draftDataR()$FB.D[index]!=""){
                                                tags$b("Fastball: ")
                                              },
                                              draftDataR()$FB.D[index],tags$b(
                                                if(draftDataR()$FB.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$FB[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$CT.D[index]!=""){
                                                tags$b("Cutter: ")
                                              },
                                              draftDataR()$CT.D[index],tags$b(
                                                if(draftDataR()$CT.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$CT[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$CB.D[index]!=""){
                                                tags$b("Curveball: ")
                                              },
                                              draftDataR()$CB.D[index],tags$b(
                                                if(draftDataR()$CB.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$CB[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$SL.D[index]!=""){
                                                tags$b("Slider: ")
                                              },
                                              draftDataR()$SL.D[index],tags$b(
                                                if(draftDataR()$SL.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$SL[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$CH.D[index]!=""){
                                                tags$b("Changeup: ")
                                              },
                                              draftDataR()$CH.D[index],tags$b(
                                                if(draftDataR()$CH.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$CH[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$SLV.D[index]!=""){
                                                tags$b("Slurve: ")
                                              },
                                              draftDataR()$SLV.D[index],tags$b(
                                                if(draftDataR()$SLV.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$SLV[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$SPL.D[index]!=""){
                                                tags$b("Splitter: ")
                                              },
                                              draftDataR()$SPL.D[index],tags$b(
                                                if(draftDataR()$SPL.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$SPL[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$FK.D[index]!=""){
                                                tags$b("Forkball: ")
                                              },
                                              draftDataR()$FK.D[index],tags$b(
                                                if(draftDataR()$FK.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$FK[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$KN.D[index]!=""){
                                                tags$b("Knuckleball: ")
                                              },
                                              draftDataR()$KN.D[index],tags$b(
                                                if(draftDataR()$KN.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$KN[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$SCR.D[index]!=""){
                                                tags$b("Screwball: ")
                                              },
                                              draftDataR()$SCR.D[index],tags$b(
                                                if(draftDataR()$SCR.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$SCR[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$PLM.D[index]!=""){
                                                tags$b("Palmball: ")
                                              },
                                              draftDataR()$PLM.D[index],tags$b(
                                                if(draftDataR()$PLM.D[index]!=""){
                                                  " Grade: "
                                                },
                                                draftDataR()$PLM[index])),
                                            
                                            tags$p(
                                              if(draftDataR()$ContCom.D[index]!=""){
                                                tags$b("Control and Command: ")
                                              },
                                              draftDataR()$ContCom.D[index],tags$b(
                                                if(draftDataR()$ContCom.D[index]!=""){
                                                  " Control: "
                                                },
                                                draftDataR()$Control[index],
                                                if(draftDataR()$ContCom.D[index]!=""){
                                                  " | Command: "
                                                },
                                                draftDataR()$Command[index])),
                                            
                                            tags$p(
                                              tags$b("Overall: "),
                                              draftDataR()$Overall.D[index]),
                                            
                                            tags$b(
                                              "OFP: ",
                                              draftDataR()$OFP[index],
                                              br(),
                                              "Role: ",
                                              draftDataR()$Role[index],
                                              " - ",
                                              draftDataR()$Role.D[index],
                                              br(),
                                              tags$b("Risk: "),
                                              draftDataR()$Risk[index],
                                              br(),
                                              br(),
                                              tags$em(
                                                "Evaluator: ",
                                                draftDataR()$Evaluator[index]
                                              )
                                            )
                                            
                                        )
                                    )
                                    
                                  }
                  ),
                  Video1 = colDef(name="Video", maxWidth = 63, align="center", 
                                  cell = function(value,index) {
                                    videoicon <- icon("youtube", class = "fa-lg iconVid", lib = "font-awesome")
                                    videoicon
                                  },
                                  details = function(index) {
                                    
                                    div(class="video", #draftDataR()$Video1[index]
                                        br(),
                                        if(draftDataR()$Video1[index]==""){
                                          ""
                                        } else {
                                          tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video1[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                        },
                                        if(draftDataR()$Video2[index]==""){
                                          ""
                                        } else {
                                          tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video2[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                        },
                                        if(draftDataR()$Video3[index]==""){
                                          ""
                                        } else {
                                          tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video3[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                        },
                                        if(draftDataR()$Video4[index]==""){
                                          ""
                                        } else {
                                          tags$iframe(class="video", height="200", src=paste0("https://www.youtube.com/embed/",draftDataR()$Video4[index]), frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                                        },
                                        br(),
                                        br()
                                    )
                                  }
                  )
                )
                
      )
    }
  })
  
  
  #   output$mockDraft2 <- renderReactable({
  #     
  #     if(input$mobileview == FALSE & input$load>0 & length(draftDataR()$Blurb)>50){
  #       
  #       boardDataTable <- draftDataR() %>% 
  #         select(RankLong, Rank.Icon, Rank.Change) 
  #       
  #       boardDataTable <- boardDataTable[c(51:250),] %>% drop_na(RankLong) %>% remove_rownames()
  #       
  #       draftDataRD <- draftDataR()[c(51:250),]
  #       
  #       if(input$listview == FALSE){ 
  #         listview=FALSE
  #       } else {
  #         listview=TRUE
  #       }
  #       
  #       reactable(boardDataTable,
  #                 compact = TRUE,
  #                 pagination = FALSE,
  #                 bordered = FALSE,
  #                 defaultExpanded = TRUE,
  #                 #defaultPageSize = 20,
  #                 #fullWidth = FALSE,
  #                 defaultColDef = colDef(headerClass = "my-header", align = "center"),
  #                 
  #                 columns = list(
  #                   
  #                   RankLong = colDef(name = "",
  #                                     align = "left",
  #                                     cell = function(value){
  #                                       div(class="number", value)
  #                                     }
  #                   ),
  #                   Rank.Icon = colDef(name="", align="right", maxWidth = 25,
  #                                      cell = function(value,index) {
  #                                        iconArrow <- icon(value, class = "fa-2x", lib = "font-awesome")
  #                                        div(class=if_else(draftDataRD$Rank.Change[index]>0, "iconUp","iconDown"), iconArrow)
  #                                      }
  #                   ),
  #                   Rank.Change = colDef(name="", align="left", maxWidth = 35,
  #                                        cell = function(value){
  #                                          div(class="number", if_else(value==0,"",as.character(abs(value))))
  #                                        }
  #                   )
  #                   
  #                 ), 
  #                 details = function(index) {
  #                   htmltools::div(style = "padding: 10px",
  #                                  hr(),
  #                                  tags$table(
  #                                    tags$tr(
  #                                      tags$td(padding = "15px", 
  #                                              img(src = draftDataRD$URL[index], height = "150px")
  #                                      ),
  #                                      tags$td(class="bio", width="100%", style="padding: 15px", 
  #                                              tags$b("Bio:"),
  #                                              hr(class="hr2"),
  #                                              paste("Age:",draftDataRD$age.DOB[index],"yr"), br(),
  #                                              paste("Height:",draftDataRD$HT[index]), br(),
  #                                              paste("Weight:",draftDataRD$WT[index],"lbs"), br(),
  #                                              paste("Hits/Throws:", draftDataRD$B.T[index]), br(),
  #                                              paste0("Hometown: ",draftDataRD$Hometown[index],", ",draftDataRD$State[index]), br(),
  #                                              paste("School:", draftDataRD$School[index]), br()
  #                                      )
  #                                    )
  #                                  ),
  #                                  hr(class="hr1"),
  #                                  div(class="blurb", draftDataRD$Blurb[index]),
  #                                  hr()
  #                   )
  #                   
  #                 }, theme = reactableTheme(borderColor = "white")
  #       )
  #     }
  # })
  # 
  # output$loadUI <- renderUI({
  #   if(input$mobileview == FALSE){
  #     actionButton("load", "View Rest of List")
  #   }
  # })
  
  # output$defUI <- renderUI({
  #   div(
  #   br(),
  #   if(input$def %% 2 != 0){
  #     sidebarPanel(width=12,
  #       helpText("This Worked d;lkjdlkjd ;ldk;lkdj ; lkjd;lkj dkljd;lk d;lkjd; d;lkjd d;lkjd;d d;lkjd ;lkd  dlkd;d ld")
  #     )
  #   }
  #   )
  # })
  
  
  output$downloadList <- downloadHandler(
    filename = function() {
      boardName
    },
    content = function(file) {
      write.csv(boardData %>% 
                  select(Rank,Name,Position,HitThrow,OFP,Role,Risk,Birthday,ETA,Highest.Level,Hit,Power,Field,Arm,Run,
                         FB,CT,CB,SL,CH,SLV,SPL,FK,KN,SCR,PLM,Control,Command
                  ), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
}