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
library(gsubfn)
library(proto)
library(sqldf)
#Change the search bar size to show the entire cell.


#Creating variables across all sessions
path <- "C:/Users/kongl5/Desktop/Shiny/ExtrusionApp/Extrusion Application/PPS_document_contents/ListOfSheets/GTE"
single_pps_file <- "Single PPS Data Filled GTE.csv"
#single_tari_file <- "Single Tari Data.csv"
single_tari_file <- "MES Data_Single Tari Data.csv"
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
temp[,1]=as.Date(temp[,1],"%m/%d/%Y",origin="1970-01-01")
single_tari_data=cbind(single_tari_data[,1:which(colnames(single_tari_data)=="Start Time")-1],
                       temp,single_tari_data[,(which(colnames(single_tari_data)=="Start Time")+1):ncol(single_tari_data)])
#get the initial Start Date Period for MES Table
Time_Start=sqldf("select Min([Start Date]) from single_tari_data")
Time_Start<-as.numeric(Time_Start)
Time_Start<-as.Date(Time_Start,origin="1970-01-01")
Time_End<-sqldf("select Max([Start Date]) from single_tari_data")
Time_End<-as.numeric(Time_End)
Time_End<-as.Date(Time_End,origin="1970-01-01")

#Find the earliest date and latest date of start time in MES

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  titlePanel("Extrusion Application"),
  
  tabsetPanel(id = "application",
              tabPanel('Part Catalog',
                       sidebarPanel(
                         conditionalPanel(
                           'input.dataset === "Single Extrusion PPS Data"',uiOutput("show_vars1_input")),
                         conditionalPanel(
                           'input.dataset === "Multi-Layered Exutrusion PPS Data"',uiOutput("show_vars2_input")),
                         conditionalPanel(
                           'input.dataset === "Tapered Extrusion PPS Data"',uiOutput("show_Vars3_input"))
                       ),
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset',
                           tabPanel('Single Extrusion PPS Data', 
                                    fluidRow(
                                      column(8,
                                             fluidRow(
                                               column(3,uiOutput("PN_input")),
                                               column(3,uiOutput("PD_input")),
                                               column(3,uiOutput("RN_input")),
                                               column(3,uiOutput("RD_input"))
                                               ),
                                             fluidRow(
                                               column(3,uiOutput("PPSN_input")),
                                               column(3,uiOutput("DS_input")),
                                               column(3,uiOutput("TS_input")),
                                               column(3,uiOutput("SP_input"))
                                               ),
                                             fluidRow(
                                               column(3,uiOutput("IDI_input")),
                                               column(3,uiOutput("ODI_input")),
                                               column(3,uiOutput("WT_input")),
                                               column(3,uiOutput("OR_input"))
                                               ),
                                             fluidRow(
                                               column(3,uiOutput("CCT_input")),
                                               column(3,uiOutput("Length_input")),
                                               column(3,uiOutput("PPD_input"))
                                               )
                                             ),
                                      column(4,
                                             fluidRow(
                                               column(4,uiOutput("NEXIV_input")),
                                               column(4,uiOutput("Annealed_input")),
                                               column(4,uiOutput("Caliper_input"))
                                             ),
                                             fluidRow(
                                               column(4,uiOutput("OS_input")),
                                               column(4,uiOutput("MP_input")),
                                               column(4,uiOutput("HT_input"))
                                               ),
                                             fluidRow(
                                               column(4,uiOutput("SPD_input")),
                                               column(4,uiOutput("SLD_input")),
                                               column(4,uiOutput("DLN_input"))
                                               ),
                                             fluidRow(
                                               column(4,uiOutput("ULT_input")),
                                               column(5,uiOutput("VC_input") ),
                                               column(3,uiOutput("IRD_input"))
                                               )
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
                           'input.output_dataset === "MES Data"',uiOutput("show_vars4_input"))
                       ),#end sidebarPanel
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'output_dataset',
                           tabPanel('MES Data', 
                                    fluidRow(
                                      column(2,
                                             selectInput("MN","Material Number",
                                                         c("All",unique(as.character(single_tari_data$`Material Number`))))
                                      ),
                                      column(2,
                                             selectInput("Batch","SAP Batch Number",
                                                         c("All",unique(as.character(single_tari_data$`SAP Batch Number`))))
                                      ),
                                      column(2,
                                             selectInput("SWR","SWR Number",
                                                         c("All",unique(as.character(single_tari_data$`SWR Number`))))
                                      ),
                                      column(2,
                                             selectInput("OperatorID","Operator ID",
                                                         c("All",unique(as.character(single_tari_data$`Operator ID`))))
                                      ),
                                      column(3,
                                             dateRangeInput("Start","Start Date Range",start=Time_Start,end=Time_End)
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
                           'input.extra_dataset === "Resin Data"',uiOutput("show_vars5_input")),
                         conditionalPanel(
                           'input.extra_dataset === "Screw Data"',uiOutput("show_vars6_input"))
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
  
  #create six columns in three tabs
  output$show_vars1_input<-renderUI({
    checkboxGroupInput('show_vars1', 'Columns to Show:',
                       choices = names(single_pps_data),
                       selected = c("Part Number", "Part Description", 
                                    "Resin Number", "Resin Description",
                                    "Die Size", "Tip Size",
                                    "Inner Diameter (in)", "Outer Diameter (in)",
                                    "Wall Thickness (in)", "Length (in)")
    )
  })
  output$show_vars2_input<-renderUI({
    checkboxGroupInput('show_vars2', 'Columns to Show:',
                       c("Part Number", "Part Description", 
                         "Resin Number", "Resin Description",
                         "Die Size", "Tip Size",
                         "Inner Diameter (in)", "Outer Diameter (in)",
                         "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                         "Outer Wall Thickness (in)",
                         "Total Wall Thickness (in)", "Total Length (in)")
                       
    )
  })
  output$show_Vars3_input<-renderUI({
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
  })
  output$show_vars4_input<-renderUI({
    checkboxGroupInput('show_vars4', 'Columns to Show:',
                       choices = names(single_tari_data),
                       selected = names(single_tari_data)
    )
  })
  output$show_vars5_input<-renderUI({
    checkboxGroupInput('show_vars5', 'Columns to Show:',
                       choices = names(resin_data),
                       selected = names(resin_data)
    )
  })
  output$show_vars6_input<-renderUI({
    checkboxGroupInput('show_vars6', 'Columns to Show:',
                       choices = names(screw_data),
                       selected = names(screw_data)
    )
  })
  
  #create all input box in all tabs
  output$PN_input<-renderUI({
    selectInput("PN","Part Number",multiple=TRUE,
                c("All",unique(as.character(single_pps_data$`Part Number`))),
                selected="All")
  })
  output$PD_input<-renderUI({
    selectInput("PD","Part Description",
                c("All",unique(as.character(single_pps_data$`Part Description`))))
  })
  output$RN_input<-renderUI({
    selectInput("RN","Resin Number",
                c("All",unique(as.character(single_pps_data$`Resin Number`))))
  })
  output$RD_input<-renderUI({
    selectInput("RD","Resin Description",
                c("All",unique(as.character(single_pps_data$`Resin Description`))))
  })
  output$PPSN_input<-renderUI({
    selectInput("PPSN","PPS Number",
                c("All",unique(as.character(single_pps_data$`PPS Number`))))
  })
  output$DS_input<-renderUI({
    DS_min=-1
    DS_max=0.54
    sliderInput("DS","Die Size(in)",min=DS_min,max=DS_max,value=c(DS_min,DS_max))
  })
  output$TS_input<-renderUI({
    TS_min=-1
    TS_max=0.49
    sliderInput("TS","Tip Size(in)",min=TS_min,max=TS_max,value=c(TS_min,TS_max))
  })
  output$SP_input<-renderUI({
    selectInput("SP","Screw Print",
                c("All",unique(as.character(single_pps_data$`Screw Print`))))
  })
  output$IDI_input<-renderUI({
    IDI_min=0.0009
    IDI_max=0.353
    sliderInput("IDI","Inner Diameter(in)",min=IDI_min,max=IDI_max,value=c(IDI_min,IDI_max),sep="",round=-4)
  })
  output$ODI_input<-renderUI({
    ODI_min=0.0133
    ODI_max=0.495
    sliderInput("ODI","Out Diameter(in)",min=ODI_min,max=ODI_max,value=c(ODI_min,ODI_max),sep="",round=-4)
  })
  output$WT_input<-renderUI({
    WT_min=0.0005
    WT_max=0.053
    sliderInput("WT","Wall Thickness(in)",min=WT_min,max=WT_max,value=c(WT_min,WT_max),sep="",round=-4)
  })
  output$OR_input<-renderUI({
    OR_min=0
    OR_max=0.01
    sliderInput("OR","Out of Roundness(in)",min=OR_min,max=OR_max,value=c(OR_min,OR_max),sep="",round=-4)
  })
  output$CCT_input<-renderUI({
    CCT_min=1
    CCT_max=2
    sliderInput("CCT","Concentricity(in)",min=CCT_min,max=CCT_max,value=c(CCT_min,CCT_max),sep="",round=-4)
  })
  output$Length_input<-renderUI({
    Length_min=0.015
    Length_max=1
    sliderInput("Length","Length(in)",min=Length_min,max=Length_max,value=c(Length_min,Length_max),sep="",round=-4)
  })
  
  output$PPD_input<-renderUI({
    selectInput("PPD","Perpendicularity(in)",
                c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
  })
  output$NEXIV_input<-renderUI({
    selectInput("NEXIV","NEXIV",
                c("All",unique(as.character(single_pps_data$NEXIV))))
  })
  output$Annealed_input<-renderUI({
    selectInput("Annealed","Annealed",
                c("All",unique(as.character(single_pps_data$Annealed))))
  })
  output$Caliper_input<-renderUI({
    selectInput("Caliper","Caliper",
                c("All",unique(as.character(single_pps_data$Caliper))))
  })
  output$OS_input<-renderUI({
    selectInput("OS","OD Sort",
                c("All",unique(as.character(single_pps_data$`OD Sort`))))
  })
  output$MP_input<-renderUI({
    selectInput("MP","Melt Pump",
                c("All",unique(as.character(single_pps_data$`Melt Pump`))))
  })
  output$HT_input<-renderUI({
    selectInput("HT","Hypo Tip",
                c("All",unique(as.character(single_pps_data$`Hypo Tip`))))
  })
  output$SPD_input<-renderUI({
    selectInput("SPD","Spark Die",
                c("All",unique(as.character(single_pps_data$`Sparker Die`))))
  })
  output$SLD_input<-renderUI({
    selectInput("SLD","Slicking Die",
                c("All",unique(as.character(single_pps_data$`Slicking Die`))))
  })
  output$DLN_input<-renderUI({
    selectInput("DLN","Delamination",
                c("All",unique(as.character(single_pps_data$Delamination))))
  })
  output$ULT_input<-renderUI({
    selectInput("ULT","Ultrasonic",
                c("All",unique(as.character(single_pps_data$Ultrasonic))))
  })
  output$VC_input<-renderUI({
    selectInput("VC","Vacuum Calibration",
                c("All",unique(as.character(single_pps_data$`Vacuum Calibration`))))
  })
  output$IRD_input<-renderUI({
    selectInput("IRD","Irradiated",
                c("All",unique(as.character(single_pps_data$Irradiated))))
  })
  
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
#Die Size
# Tip Size
      if(input$SP!="All"){
        data1<-data1[data1$`Screw Print`==input$SP,]
      }
      
#IDI
#ODI
#WT
#OR
#CCT
#Length
      if(input$PPD!="All"){
        data1<-data1[data1$`Perpendicularity (in)`==input$PPD,]
      }
      if(input$NEXIV!="All"){
        data1<-data1[data1$`NEXIV`==input$NEXIV,]
      }
      if(input$Annealed!="All"){
        data1<-data1[data1$`Annealed`==input$Annealed,]
      }
      if(input$Caliper!="All"){
        data1<-data1[data1$`Caliper`==input$Caliper,]
      }
      if(input$OS!="All"){
        data1<-data1[data1$`OD Sort`==input$OS,]
      }
      if(input$MP!="All"){
        data1<-data1[data1$`Melt Pumpr`==input$MP,]
      }
      if(input$HT!="All"){
        data1<-data1[data1$`Hypo Tip`==input$HT,]
      }
      if(input$SPD!="All"){
        data1<-data1[data1$`Sparker Die`==input$SPD,]
      }
      if(input$SLD!="All"){
        data1<-data1[data1$`Slicking Die`==input$SLD,]
      }
      if(input$DLN!="All"){
        data1<-data1[data1$`Delamination`==input$DLN,]
      }
      if(input$ULT!="All"){
        data1<-data1[data1$`Ultrasonic`==input$ULT,]
      }
      if(input$VC!="All"){
        data1<-data1[data1$`Vacuum Calibration`==input$VC,]
      }
      if(input$IRD!="All"){
        data1<-data1[data1$`Irradiated`==input$IRD,]
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
        data4<-single_tari_data[,input$show_vars4]
      }
      else {data4<-single_tari_data[single_tari_data$`Material Number`==input$PN,input$show_vars4]
      }
      
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
      if(input$Start[1]!=Time_Start || input$Start[2]!=Time_End){
        data4<-data4[(data4$`Start Date`<=input$Start[2] & data4$`Start Date`>=input$Start[1]),]
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

