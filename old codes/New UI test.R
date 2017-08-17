
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  titlePanel("Extrusion Application"),
  
  tabsetPanel(id = "application",
              tabPanel('Part Catalog',
                       sidebarPanel(
                         conditionalPanel(
                           'input.dataset === "Single Extrusion PPS Data"',
                           checkboxGroupInput('show_vars1', 'Columns to Show:',
                                              choices = names(single_pps_data),
                                              selected = c("Part Number", "Part Description", 
                                                           "Resin Number", "Resin Description",
                                                           "Die Size", "Tip Size",
                                                           "Inner Diameter (in)", "Outer Diameter (in)",
                                                           "Wall Thickness (in)", "Length (in)")
                           )
                         ),
                         conditionalPanel(
                           'input.dataset === "Multi-Layered Exutrusion PPS Data"',
                           checkboxGroupInput('show_vars2', 'Columns to Show:',
                                              c("Part Number", "Part Description", 
                                                "Resin Number", "Resin Description",
                                                "Die Size", "Tip Size",
                                                "Inner Diameter (in)", "Outer Diameter (in)",
                                                "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                                                "Outer Wall Thickness (in)",
                                                "Total Wall Thickness (in)", "Total Length (in)")
                                              
                           )
                         ),
                         conditionalPanel(
                           'input.dataset === "Tapered Extrusion PPS Data"',
                           checkboxGroupInput('show_vars3', 'Columns to Show:',
                                              choices = names(tapered_pps_data), 
                                              c("Part Number", "Part Description", 
                                                "Resin Number", "Resin Description",
                                                "Die Size", "Tip Size",
                                                "Proximal Inner Diameter (in)", "Proximal Inner Diameter (in)",
                                                "Proximal Wall Thickness (in)",
                                                "Distal Inner Diameter (in)", "Distal Outer Diameter (in)",
                                                "Distal Wall Thickness (in)",
                                                "Proximal Length (in)", "Transition Length (in)", 
                                                "Transition Length (in)", "Total Length (in)")
                           )
                         )
                       ),
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset',
                           tabPanel('Single Extrusion PPS Data', 
                                    fluidRow(
                                      column(2,
                                             selectInput("PN","Part Number",
                                                         c("All",unique(as.character(single_pps_data$`Part Number`))))
                                      ),
                                      column(3,
                                             selectInput("PD","Part Description",
                                                         c("All",unique(as.character(single_pps_data$`Part Description`))))
                                      ),
                                      column(2,
                                             selectInput("RN","Resin Number",
                                                         c("All",unique(as.character(single_pps_data$`Resin Number`))))
                                      ),
                                      column(2,
                                             selectInput("RD","Resin Description",
                                                         c("All",unique(as.character(single_pps_data$`Resin Description`))))
                                      ),
                                      column(2,
                                             selectInput("PPSN","PPS Number",
                                                         c("All",unique(as.character(single_pps_data$`PPS Number`))))
                                      )
                                    ),
                                    fluidRow(
                                      DT::dataTableOutput("mytable1")
                                    )
                           ),
                           tabPanel('Multi-Layered Exutrusion PPS Data', DT::dataTableOutput('mytable2')),
                           tabPanel('Tapered Extrusion PPS Data', DT::dataTableOutput('mytable3'))
                         )
                       ) #end mainPanel
              ),#end tabPanel
              
              tabPanel('Output',
                       sidebarPanel(
                         conditionalPanel(
                           'input.output_dataset === "MES Data"',
                           checkboxGroupInput('show_vars4', 'Columns to Show:',
                                              choices = names(single_tari_data),
                                              selected = names(single_tari_data)
                           )
                         )
                       ),#end sidebarPanel
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'output_dataset',
                           tabPanel('MES Data', 
                                    fluidRow(
                                      column(2,
                                             selectInput("MN",
                                                         "Material Number",
                                                         c("All",
                                                           unique(as.character(single_tari_data$`Material Number`))))
                                      ),
                                      column(2,
                                             selectInput("Batch",
                                                         "SAP Batch Number",
                                                         c("All",
                                                           unique(as.character(single_tari_data$`SAP Batch Number`))))
                                      ),
                                      column(2,
                                             selectInput("SWR",
                                                         "SWR Number",
                                                         c("All",
                                                           unique(as.character(single_tari_data$`SWR Number`))))
                                      ),
                                      column(2,
                                             selectInput("OperatorID",
                                                         "Operator ID",
                                                         c("All",
                                                           unique(as.character(single_tari_data$`Operator ID`))))
                                      ),
                                      column(3,
                                             dateRangeInput("Start",
                                                            "Start Date Range",
                                                            start=Sys.Date()-2,end=Sys.Date()
                                             )
                                      )
                                      
                                    ),
                                    # Create a new row for the table.
                                    fluidRow(
                                      DT::dataTableOutput("mytable4"),
                                      verbatimTextOutput("summary")
                                    )
                           )
                         )
                       ) #end mainPanel
              ), #end tabPanel for 'Output'
              
              tabPanel('Extra',
                       sidebarPanel(
                         
                         conditionalPanel(
                           'input.extra_dataset === "Resin Data"',
                           checkboxGroupInput('show_vars5', 'Columns to Show:',
                                              choices = names(resin_data),
                                              selected = names(resin_data)
                           )
                         ),
                         conditionalPanel(
                           'input.extra_dataset === "Screw Data"',
                           checkboxGroupInput('show_vars6', 'Columns to Show:',
                                              choices = names(screw_data),
                                              selected = names(screw_data)
                           )
                         )
                       ),#end sidebarPanel
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'extra_dataset',
                           tabPanel('Resin Data', DT::dataTableOutput('mytable5')),
                           tabPanel('Screw Data', DT::dataTableOutput('mytable6'))
                         )
                       ) #end mainPanel
              ) #end tabPanel for 'Extra'
              
  )#end tabsetPanel for part catalog
  
  
) #end fluidPage




server <- function(input, output, session) {
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable({
      data1<-single_pps_data[, input$show_vars1]
      if(input$PN!="All"){
        data1<-data1[data1$`Part Number`==input$PN,]
      }
      if(input$PD!="All"){
        data1<-data1[data1$`Part Description`==input$PD,]
      }
      if(input$RN!="All"){
        data1<-data1[data1$`Resin Number`==input$RN,]
      }
      if(input$RD!="All"){
        data1<-data1[data1$`Resin Descriptionr`==input$RD,]
      }
      if(input$PPSN!="All"){
        data1<-data1[data1$`PPS Number`==input$PPSN,]
      }
      data1
    }
      options = list(orderClasses = TRUE, 
                     columnDefs = list(list(className = 'dt-center', 
                                            targets = "_all"
                                            )
                                       ),
                     scrollX=TRUE,
                     scrollY=500,
                     autoWidth=TRUE)
      )
    }
    )

  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(multi_pps_data[, input$show_vars2], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"
                                 )
                                 ),
                                 scrollX=TRUE,
                                 scrollY=500,
                                 autoWidth=TRUE), 
                  filter = "top")
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(tapered_pps_data[, input$show_vars3], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"
                                 )
                                 ),
                                 scrollX=TRUE,
                                 scrollY=500,
                                 autoWidth=TRUE), 
                  filter = "top")
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable({
      data4<-single_tari_data[,input$show_vars4]
      if(input$MN!="All"){
        data4<-data4[data4$`Material Number`==input$MN,]
      }
      if(input$Batch!="All"){
        data4<-data4[data4$`SAP Batch Number`==input$Batch,]
      }
      if(input$SWR!="All"){
        data4<-data4[data4$`SWR Number`==input$SWR,]
      }
      if(input$OperatorID!="All"){
        data4<-data4[data4$`Operator ID`==input$OperatorID,]
      }
      data4
    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE) )
  })
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(resin_data[, input$show_vars5], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"
                                 )
                                 ),
                                 scrollX=TRUE,
                                 scrollY=500,
                                 autoWidth=TRUE), 
                  filter = "top")
  })
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(screw_data[, input$show_vars6], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"
                                 )
                                 ),
                                 scrollX=TRUE,
                                 scrollY=500,
                                 autoWidth=TRUE), 
                  filter = "top")
  })
  
  
  
} #end server

# Run the application 
shinyApp(ui = ui, server = server)

