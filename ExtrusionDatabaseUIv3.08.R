#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
#Change the search bar size to show the entire cell.


#Creating variables across all sessions
path <- "C:/Users/kongl5/Desktop/Shiny/ExtrusionApp/Extrusion Application/PPS_document_contents/ListOfSheets/GTE"
single_pps_file <- "Single PPS Data Filled GTE.csv"
single_tari_file <- "Single Tari Data.csv"
multi_pps_file <- "Multi-Layered PPS Data Filled GTE.csv"
tapered_pps_file <- "Tapered PPS Data Filled GTE.csv"
resin_file <- "Resin Information.csv"
screw_file <- "Screw Properties.csv"

single_pps_pathfile <- paste(path, single_pps_file, sep = "/")
single_tari_pathfile <- paste(path, single_tari_file, sep = "/")
multi_pps_pathfile <- paste(path, multi_pps_file, sep = "/")
tapered_pps_pathfile <- paste(path, tapered_pps_file, sep = "/")
resin_pathfile <- paste(path, resin_file, sep = "/")
screw_pathfile <- paste(path, screw_file, sep = "/")

single_pps_data <- read.csv(single_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                            check.names = FALSE)
single_tari_data <- read.csv(single_tari_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
multi_pps_data <- read.csv(multi_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                           check.names = FALSE)
tapered_pps_data <- read.csv(tapered_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
resin_data <- read.csv(resin_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
screw_data <- read.csv(screw_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
temp=as.data.frame(matrix(0,nrow=nrow(single_tari_data),ncol=2))
colnames(temp)=c("Start Date","Start Time")
temp[,1:2]=str_split_fixed(single_tari_data$`Start Time`,' ',2)
temp[,1]=as.Date(temp[,1],"%m/%d/%Y",origin="1899-12-30")
single_tari_data=cbind(single_tari_data[,1:which(colnames(single_tari_data)=="Start Time")-1],
                       temp,single_tari_data[,(which(colnames(single_tari_data)=="Start Time")+1):ncol(single_tari_data)])
#Find the earliest date and latest date of start time in MES

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
                                      ),
                                      column(2,
                                             selectInput("DS","Die Size(in)",
                                                         c("All",unique(as.character(single_pps_data$`Die Size (in)`))))
                                      ),
                                      column(2,
                                             selectInput("TS","Tip Size(in)",
                                                         c("All",unique(as.character(single_pps_data$`Tip Size (in)`))))
                                      ),
                                      column(2,
                                             selectInput("SP","Screw Print",
                                                         c("All",unique(as.character(single_pps_data$`Screw Print`))))
                                      ),
                                      column(2,
                                             selectInput("IDI","Inner Diameter(in)",
                                                         c("All",unique(as.character(single_pps_data$`Inner Diameter (in)`))))
                                      ),
                                      column(2,
                                             selectInput("ODI","Out Diameter(in)",
                                                         c("All",unique(as.character(single_pps_data$`Outer Diameter (in)`))))
                                      ),
                                      column(2,
                                             selectInput("WT","Wall Thickness(in)",
                                                         c("All",unique(as.character(single_pps_data$`Wall Thickness (in)`))))
                                      ),
                                      column(2,
                                             selectInput("OR","Out of Roundness(in)",
                                                         c("All",unique(as.character(single_pps_data$`Out of Roundness (in)`))))
                                      ),
                                      column(2,
                                             selectInput("CCT","Concentricity(in)",
                                                         c("All",unique(as.character(single_pps_data$`Concentricity (in)`))))
                                      ),
                                      column(2,
                                             selectInput("Length","Length(in)",
                                                         c("All",unique(as.character(single_pps_data$`Length (in)`))))
                                      ),
                                      column(2,
                                             selectInput("PPD","Perpendicularity(in)",
                                                         c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
                                      ),
                                      column(1,
                                             selectInput("NEXIV","NEXIV",
                                                         c("All",unique(as.character(single_pps_data$NEXIV))))
                                      ),
                                      column(1,
                                             selectInput("Annealed","Annealed",
                                                         c("All",unique(as.character(single_pps_data$Annealed))))
                                      ),
                                      column(1,
                                             selectInput("Caliper","Caliper",
                                                         c("All",unique(as.character(single_pps_data$Caliper))))
                                      ),
                                      column(1,
                                             selectInput("OS","OD Sort",
                                                         c("All",unique(as.character(single_pps_data$'OD Sort'))))
                                      ),
                                      column(1,
                                             selectInput("MP","Melt Pump",
                                                         c("All",unique(as.character(single_pps_data$`Melt Pump`))))
                                      ),
                                      column(1,
                                             selectInput("HT","Hypo Tip",
                                                         c("All",unique(as.character(single_pps_data$`Hypo Tip`))))
                                      ),
                                      column(1,
                                             selectInput("SPD","Spark Die",
                                                         c("All",unique(as.character(single_pps_data$`Sparker Die`))))
                                      ),
                                      column(1,
                                             selectInput("SLD","Slicking Die",
                                                         c("All",unique(as.character(single_pps_data$`Slicking Die`))))
                                      ),
                                      column(1,
                                             selectInput("DLN","Delamination",
                                                         c("All",unique(as.character(single_pps_data$Delamination))))
                                      ),
                                      column(1,
                                             selectInput("OR","Out of Roundness(in)",
                                                         c("All",unique(as.character(single_pps_data$`Out of Roundness (in)`))))
                                      ),
                                      column(1,
                                             selectInput("ULT","Ultrasonic",
                                                         c("All",unique(as.character(single_pps_data$Ultrasonic))))
                                      ),
                                      column(1,
                                             selectInput("VC","Vacuum Calibration",
                                                         c("All",unique(as.character(single_pps_data$`Vacuum Calibration`))))
                                      ),
                                      column(1,
                                             selectInput("IRD","Irradiated",
                                                         c("All",unique(as.character(single_pps_data$Irradiated))))
                                      )
                                    ),
                                    fluidRow(
                                      DT::dataTableOutput("mytable1"),
                                      verbatimTextOutput("Summary1")
                                    )
                           ),
                           tabPanel('Multi-Layered Exutrusion PPS Data', DT::dataTableOutput('mytable2')),
                           tabPanel('Tapered Extrusion PPS Data', DT::dataTableOutput('mytable3'))
                         )
                       )#end mainPanel
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
                                      DT::dataTableOutput("mytable4")
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
  
  e1 <- new.env(
    #variables to contain:
    # vectorofAttributes - contains the attributes that were previously selected by the user
  ) #creates a new environment to store instance variables
  
  
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
      if(input$DS!="All"){
        data1<-data1[data1$`Die Size (in)`==input$DS,]
      }
      if(input$TS!="All"){
        data1<-data1[data1$`PPS Number`==input$TS,]
      }
      if(input$SP!="All"){
        data1<-data1[data1$`PPS Number`==input$SP,]
      }
      if(input$IDI!="All"){
        data1<-data1[data1$`PPS Number`==input$IDI,]
      }
      if(input$ODI!="All"){
        data1<-data1[data1$`PPS Number`==input$ODI,]
      }
      if(input$WT!="All"){
        data1<-data1[data1$`PPS Number`==input$WT,]
      }
      if(input$OR!="All"){
        data1<-data1[data1$`PPS Number`==input$OR,]
      }
      if(input$CCT!="All"){
        data1<-data1[data1$`PPS Number`==input$CCT,]
      }
      if(input$Length!="All"){
        data1<-data1[data1$`PPS Number`==input$Length,]
      }
      if(input$PPD!="All"){
        data1<-data1[data1$`PPS Number`==input$PPD,]
      }
      if(input$NEXIV!="All"){
        data1<-data1[data1$`PPS Number`==input$NEXIV,]
      }
      if(input$Annealed!="All"){
        data1<-data1[data1$`PPS Number`==input$Annealed,]
      }
      if(input$Caliper!="All"){
        data1<-data1[data1$`PPS Number`==input$Caliper,]
      }
      if(input$OS!="All"){
        data1<-data1[data1$`PPS Number`==input$OS,]
      }
      if(input$MP!="All"){
        data1<-data1[data1$`PPS Number`==input$MP,]
      }
      if(input$HT!="All"){
        data1<-data1[data1$`PPS Number`==input$HT,]
      }
      if(input$SPD!="All"){
        data1<-data1[data1$`PPS Number`==input$SPD,]
      }
      if(input$SLD!="All"){
        data1<-data1[data1$`PPS Number`==input$SLD,]
      }
      if(input$DLN!="All"){
        data1<-data1[data1$`PPS Number`==input$DLN,]
      }
      if(input$OR!="All"){
        data1<-data1[data1$`PPS Number`==input$OR,]
      }
      if(input$ULT!="All"){
        data1<-data1[data1$`PPS Number`==input$ULT,]
      }
      if(input$VC!="All"){
        data1<-data1[data1$`PPS Number`==input$VC,]
      }
      if(input$IRD!="All"){
        data1<-data1[data1$`PPS Number`==input$IRD,]
      }

      data1
    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE))
  })
  output$Summary1<-renderPrint({
    input$PN
  })
        
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
      if (input$PN=="All"){
        data<-single_tari_data[,input$show_vars4]
      }
      else {data<-single_tari_data[single_tari_data$`Material Number`==input$PN,input$show_vars4]
      }
      
      if(input$MN!="All"){
        data<-data[data$`Material Number`==input$MN,]
      }
      if(input$Batch!="All"){
        data<-single_tari_data[single_tari_data$`SAP Batch Number`==input$Batch,]
      }
      if(input$SWR!="All"){
        data<-single_tari_data[single_tari_data$`SWR Number`==input$SWR,]
      }
      if(input$OperatorID!="All"){
        data<-single_tari_data[single_tari_data$`Operator ID`==input$OperatorID,]
      }
      data
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

