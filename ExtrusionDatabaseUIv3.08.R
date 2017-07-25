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


#MES Table
#get the initial Start Date Period for MES Table
Time_Start=sqldf("select Min([Start Date]) from single_tari_data")
Time_Start<-as.numeric(Time_Start)
Time_Start<-as.Date(Time_Start,origin="1970-01-01")
Time_End<-sqldf("select Max([Start Date]) from single_tari_data")
Time_End<-as.numeric(Time_End)
Time_End<-as.Date(Time_End,origin="1970-01-01")



#Special Parameter---Change the blank to No
for (i in 26:37 ){
  for (j in 1:nrow(single_pps_data)){
  if(single_pps_data[j,i]==""){
    single_pps_data[j,i]="No"
  }
  }
}

#Catalog--Multi-PPS-Table
  #Fill the Partnumber and PPS number for each single row in the table
for (i in 1:nrow(multi_pps_data)){
  if(multi_pps_data[i,"Part Number"]==""){
    multi_pps_data[i,"Part Number"]=multi_pps_data[i-1,"Part Number"]
  }
  if(multi_pps_data[i,"PPS Number"]==""){
    multi_pps_data[i,"PPS Number"]=multi_pps_data[i-1,"PPS Number"]
  }
}
  #Special Parameter---use NA to replace blank



#display all rows which share the same partnumbers where the parameter meets the requirements
temp=sqldf("select * from multi_pps_data where multi_pps_data.[Part Number] in 
              (select [Part Number] from multi_pps_data 
                where [Barrel Zone 1 Temperature  F]=345)")


#*********************************************

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
                           'input.dataset === "Multi-Layered Extrusion PPS Data"',uiOutput("show_vars2_input")),
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
                                               column(3,uiOutput("PCSPN_input")),
                                               column(3,uiOutput("PCSPD_input")),
                                               column(3,uiOutput("PCSRN_input")),
                                               column(3,uiOutput("PCSRD_input"))
                                               ),
                                             fluidRow(
                                               column(3,uiOutput("PCSPPSN_input")),
                                               column(3,uiOutput("PCSDS_input")),
                                               column(3,uiOutput("PCSTS_input")),
                                               column(3,uiOutput("PCSSP_input"))
                                               ),
                                             fluidRow(
                                               column(3,uiOutput("PCSIDI_input")),
                                               column(3,uiOutput("PCSODI_input")),
                                               column(3,uiOutput("PCSWT_input")),
                                               column(3,uiOutput("PCSOR_input"))
                                               ),
                                             fluidRow(
                                               column(3,uiOutput("PCSCCT_input")),
                                               column(3,uiOutput("PCSLength_input")),
                                               column(3,uiOutput("PCSPPD_input"))
                                               )
                                             ),
                                      column(4,
                                             fluidRow(
                                               column(4,uiOutput("PCSNEXIV_input")),
                                               column(4,uiOutput("PCSAnnealed_input")),
                                               column(4,uiOutput("PCSCaliper_input"))
                                             ),
                                             fluidRow(
                                               column(4,uiOutput("PCSOS_input")),
                                               column(4,uiOutput("PCSMP_input")),
                                               column(4,uiOutput("PCSHT_input"))
                                               ),
                                             fluidRow(
                                               column(4,uiOutput("PCSSPD_input")),
                                               column(4,uiOutput("PCSSLD_input")),
                                               column(4,uiOutput("PCSDLN_input"))
                                               ),
                                             fluidRow(
                                               column(4,uiOutput("PCSULT_input")),
                                               column(5,uiOutput("PCSVC_input") ),
                                               column(3,uiOutput("PCSIRD_input"))
                                               )
                                             )
                                      ),
                                    fluidRow(
                                      DT::dataTableOutput("mytable1"),
                                      verbatimTextOutput("Summary1")
                                    )
                           ),
                           tabPanel('Multi-Layered Extrusion PPS Data', 
                                    fluidRow(
                                      column(3,uiOutput("PCMPN_input")),
                                      column(3,uiOutput("PCMPD_input")),
                                      column(3,uiOutput("PCMRN_input")),
                                      column(3,uiOutput("PCMRD_input")),
                                      column(3,uiOutput("PCMDS_input")),
                                      column(3,uiOutput("PCMTS_input")),
                                      column(3,uiOutput("PCMID_input")),
                                      column(3,uiOutput("PCMOD_input")),
                                      column(3,uiOutput("PCMIWT_input")),
                                      column(3,uiOutput("PCMMWT_input")),
                                      column(3,uiOutput("PCMOWT_input")),
                                      column(3,uiOutput("PCMTWT_input")),
                                      column(3,uiOutput("PCMTL_input"))
                                      ),
                                    fluidRow(
                                      DT::dataTableOutput('mytable2')
                                      )
                                    ), #end Multi-Layered Extrusion PPS Data
                           tabPanel('Tapered Extrusion PPS Data',
                                    fluidRow(
                                      column(3,uiOutput("PCTPN_input")),
                                      column(3,uiOutput("PCTPD_input")),
                                      column(3,uiOutput("PCTRN_input")),
                                      column(3,uiOutput("PCTRD_input"))
                                      ),
                                    fluidRow(
                                      DT::dataTableOutput('mytable3')
                                      )
                                    )  #end Tapered Extrusion PPS Data
                           )#end tabsetPanel
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
                       choices= c("Part Number", "Part Description", 
                         "Resin Number", "Resin Description",
                         "Die Size (in)", "Tip Size (in)",
                         "Inner Diameter (in)", "Outer Diameter (in)",
                         "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                         "Outer Wall Thickness (in)",
                         "Total Wall Thickness (in)", "Total Length"),
                       selected=c("Part Number", "Part Description", 
                                  "Resin Number", "Resin Description",
                                  "Die Size (in)", "Tip Size (in)",
                                  "Inner Diameter (in)", "Outer Diameter (in)",
                                  "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                                  "Outer Wall Thickness (in)", "Total Wall Thickness (in)", "Total Length"
                                  )
                       
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
  
  #create Server side of input box for Part Catalog Single Extrusion PPS Data
  output$PCSPN_input<-renderUI({
    selectizeInput("PCSPN","Part Number",multiple=TRUE,
                c("All",unique(as.character(single_pps_data$`Part Number`))),
                selected="All")
  })
  output$PCSPD_input<-renderUI({
    selectInput("PCSPD","Part Description",
                c("All",unique(as.character(single_pps_data$`Part Description`))))
  })
  output$PCSRN_input<-renderUI({
    selectInput("PCSRN","Resin Number",
                c("All",unique(as.character(single_pps_data$`Resin Number`))))
  })
  output$PCSRD_input<-renderUI({
    selectInput("PCSRD","Resin Description",
                c("All",unique(as.character(single_pps_data$`Resin Description`))))
  })
  output$PCSPPSN_input<-renderUI({
    selectInput("PCSPPSN","PPS Number",
                c("All",unique(as.character(single_pps_data$`PPS Number`))))
  })
  output$PCSDS_input<-renderUI({
    DS_min=-1
    DS_max=0.54
    sliderInput("PCSDS","Die Size(in)",min=DS_min,max=DS_max,value=c(DS_min,DS_max))
  })
  output$PCSTS_input<-renderUI({
    TS_min=-1
    TS_max=0.49
    sliderInput("PCSTS","Tip Size(in)",min=TS_min,max=TS_max,value=c(TS_min,TS_max))
  })
  output$PCSSP_input<-renderUI({
    selectInput("PCSSP","Screw Print",
                c("All",unique(as.character(single_pps_data$`Screw Print`))))
  })
  output$PCSIDI_input<-renderUI({
    IDI_min=0.0009
    IDI_max=0.353
    sliderInput("PCSIDI","Inner Diameter(in)",min=IDI_min,max=IDI_max,value=c(IDI_min,IDI_max),sep="",round=-4)
  })
  output$PCSODI_input<-renderUI({
    ODI_min=0.0133
    ODI_max=0.495
    sliderInput("PCSODI","Out Diameter(in)",min=ODI_min,max=ODI_max,value=c(ODI_min,ODI_max),sep="",round=-4)
  })
  output$PCSWT_input<-renderUI({
    WT_min=0.0005
    WT_max=0.053
    sliderInput("PCSWT","Wall Thickness(in)",min=WT_min,max=WT_max,value=c(WT_min,WT_max),sep="",round=-4)
  })
  output$PCSOR_input<-renderUI({
    OR_min=0
    OR_max=0.01
    sliderInput("PCSOR","Out of Roundness(in)",min=OR_min,max=OR_max,value=c(OR_min,OR_max),sep="",round=-4)
  })
  output$PCSCCT_input<-renderUI({
    CCT_min=1
    CCT_max=2
    sliderInput("PCSCCT","Concentricity(in)",min=CCT_min,max=CCT_max,value=c(CCT_min,CCT_max),sep="",round=-4)
  })
  output$PCSLength_input<-renderUI({
    Length_min=0.015
    Length_max=1
    sliderInput("PCSLength","Length(in)",min=Length_min,max=Length_max,value=c(Length_min,Length_max),sep="",round=-4)
  })
  
  output$PCSPPD_input<-renderUI({
    selectInput("PCSPPD","Perpendicularity(in)",
                c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
  })
  output$PCSNEXIV_input<-renderUI({
    selectInput("PCSNEXIV","NEXIV",choices=c("All","yes","No"))
  })
  output$PCSAnnealed_input<-renderUI({
    selectInput("PCSAnnealed","Annealed",choices=c("All","yes","No"))
  })
  output$PCSCaliper_input<-renderUI({
    selectInput("PCSCaliper","Caliper",choices=c("All","yes","No"))
  })
  output$PCSOS_input<-renderUI({
    selectInput("PCSOS","OD Sort",choices=c("All","yes","No"))
  })
  output$PCSMP_input<-renderUI({
    selectInput("PCSMP","Melt Pump",choices=c("All","yes","No"))
  })
  output$PCSHT_input<-renderUI({
    selectInput("PCSHT","Hypo Tip",choices=c("All","yes","No"))
  })
  output$PCSSPD_input<-renderUI({
    selectInput("PCSSPD","Spark Die",choices=c("All","yes","No"))
  })
  output$PCSSLD_input<-renderUI({
    selectInput("PCSSLD","Slicking Die",choices=c("All","yes","No"))
  })
  output$PCSDLN_input<-renderUI({
    selectInput("PCSDLN","Delamination",choices=c("All","yes","No"))
  })
  output$PCSULT_input<-renderUI({
    selectInput("PCSULT","Ultrasonic",choices=c("All","yes","No"))
  })
  output$PCSVC_input<-renderUI({
    selectInput("PCSVC","Vacuum Calibration",choices=c("All","yes","No"))
  })
  output$PCSIRD_input<-renderUI({
    selectInput("PCSIRD","Irradiated",choices=c("All","yes","No"))
  })
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable({
      data_PCS<-single_pps_data[, input$show_vars1]
      if(input$PCSPN!="All"){
        data_PCS<-data_PCS[data_PCS$`Part Number`==input$PCSPN,]
      }
      if(input$PCSPD!="All"){
        data_PCS<-data_PCS[data_PCS$`Part Description`==input$PCSPD,]
      }
      if(input$PCSRN!="All"){
        data_PCS<-data_PCS[data_PCS$`Resin Number`==input$PCSRN,]
      }
      if(input$PCSRD!="All"){
        data_PCS<-data_PCS[data_PCS$`Resin Descriptionr`==input$PCSRD,]
      }
      if(input$PCSPPSN!="All"){
        data_PCS<-data_PCS[data_PCS$`PPS Number`==input$PCSPPSN,]
      }
#Die Size#Tip Size
      if(input$PCSSP!="All"){
        data_PCS<-data_PCS[data_PCS$`Screw Print`==input$PCSSP,]
      }
      
#IDI#ODI#WT#OR#CCT#Length
      if(input$PCSPPD!="All"){
        data_PCS<-data_PCS[data_PCS$`Perpendicularity (in)`==input$PCSPPD,]
      }
      if(input$PCSNEXIV!="All"){
        data_PCS<-data_PCS[data_PCS$`NEXIV`==input$PCSNEXIV,]
      }
      if(input$PCSAnnealed!="All"){
        data_PCS<-data_PCS[data_PCS$`Annealed`==input$PCSAnnealed,]
      }
      if(input$PCSCaliper!="All"){
        data_PCS<-data_PCS[data_PCS$`Caliper`==input$PCSCaliper,]
      }
      if(input$PCSOS!="All"){
        data_PCS<-data_PCS[data_PCS$`OD Sort`==input$PCSOS,]
      }
      if(input$PCSMP!="All"){
        data_PCS<-data_PCS[data_PCS$`Melt Pump`==input$PCSMP,]
      }
      if(input$PCSHT!="All"){
        data_PCS<-data_PCS[data_PCS$`Hypo Tip`==input$PCSHT,]
      }
      if(input$PCSSPD!="All"){
        data_PCS<-data_PCS[data_PCS$`Sparker Die`==input$PCSSPD,]
      }
      if(input$PCSSLD!="All"){
        data_PCS<-data_PCS[data_PCS$`Slicking Die`==input$PCSSLD,]
      }
      if(input$PCSDLN!="All"){
        data_PCS<-data_PCS[data_PCS$`Delamination`==input$PCSDLN,]
      }
      if(input$PCSULT!="All"){
        data_PCS<-data_PCS[data_PCS$`Ultrasonic`==input$PCSULT,]
      }
      if(input$PCSVC!="All"){
        data_PCS<-data_PCS[data_PCS$`Vacuum Calibration`==input$PCSVC,]
      }
      if(input$PCSIRD!="All"){
        data_PCS<-data_PCS[data_PCS$`Irradiated`==input$PCSIRD,]
      }

      data_PCS
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

  
  #create Server side of input box for Part Catalog Multi-Layered Extrusion PPS Data
  output$PCMPN_input<-renderUI({
    selectizeInput("PCMPN","Part Number",
                   c("All",unique(as.character(multi_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCMPD_input<-renderUI({
    selectizeInput("PCMPD","Part Description",
                   c("All",unique(as.character(multi_pps_data$`Part Description`))),
                   selected="All")
  })
  output$PCMRN_input<-renderUI({
    selectizeInput("PCMRN","Resin Number",
                   c("All",unique(as.character(multi_pps_data$`Resin Number`))),
                   selected="All")
  })
  output$PCMRD_input<-renderUI({
    selectizeInput("PCMRD","Resin Description",
                   c("All",unique(as.character(multi_pps_data$`Resin Description`))),
                   selected="All")
  })
  output$PCMDS_input<-renderUI({
    selectizeInput("PCMDS","Die Size",
                   c("All",unique(as.character(multi_pps_data$`Die Size (in)`))),
                   selected="All")
  })
  output$PCMTS_input<-renderUI({
    selectizeInput("PCMTS","Tip Size",
                   c("All",unique(as.character(multi_pps_data$`Tip Size (in)`))),
                   selected="All")
  })
  output$PCMID_input<-renderUI({
    selectizeInput("PCMID","Inner Diameter (in)",
                   c("All",unique(as.character(multi_pps_data$`Inner Diameter (in)`))),
                   selected="All")
  })
  output$PCMOD_input<-renderUI({
    selectizeInput("PCMOD","Outer Diameter (in)",
                   c("All",unique(as.character(multi_pps_data$`Outer Diameter (in)`))),
                   selected="All")
  })
  output$PCMIWT_input<-renderUI({
    selectizeInput("PCMIWT","Inner Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Inner Wall Thickness (in)`))),
                   selected="All")
  })
  
  output$PCMMWT_input<-renderUI({
    selectizeInput("PCMMWT","Middle Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Middle Wall Thickness (in)`))),
                   selected="All")
  })
  output$PCMOWT_input<-renderUI({
    selectizeInput("PCMOWT","Outer Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Outer Wall Thickness (in)`))),
                   selected="All")
  })
  output$PCMTWT_input<-renderUI({
    selectizeInput("PCMTWT","Total Wall Thickness (in)",
                   c("All",unique(as.character(multi_pps_data$`Total Wall Thickness (in)`))),
                   selected="All")
  })
  output$PCMTL_input<-renderUI({
    selectizeInput("PCMTL","Total Length",
                   c("All",unique(as.character(multi_pps_data$`Total Length`))),
                   selected="All")
  })
  
  #create Server side of Part Catalog Multi-Layered Extrusion PPS Data's table-----mytable2  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable({
      data_PCM<-multi_pps_data[, input$show_vars2]            
      if(input$PCMPN!="All"){
        data_PCM<-data_PCM[data_PCM$`Part Number`==input$PCMPN,]
      }
      if(input$PCMPD!="All"){
        data_PCM<-data_PCM[data_PCM$`Part Description`==input$PCMPD,]
      }
      if(input$PCMRN!="All"){
        data_PCM<-data_PCM[data_PCM$`Resin Number`==input$PCMRN,]
      }
      if(input$PCMRD!="All"){
        data_PCM<-data_PCM[data_PCM$`Resin Description`==input$PCMRD,]
      }
      if(input$PCMDS!="All"){
        data_PCM<-data_PCM[data_PCM$`Die Size (in)`==input$PCMDS,]
      }
      if(input$PCMTS!="All"){
        data_PCM<-data_PCM[data_PCM$`Tip Size (in)`==input$PCMTS,]
      }
      if(input$PCMID!="All"){
        data_PCM<-data_PCM[data_PCM$`Inner Diameter (in)`==input$PCMID,]
      }
      if(input$PCMOD!="All"){
        data_PCM<-data_PCM[data_PCM$`Outer Diameter (in)`==input$PCMOD,]
      }
      if(input$PCMIWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Inner Wall Thickness (in)`==input$PCMIWT,]
      }
      if(input$PCMMWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Middle Wall Thickness (in)`==input$PCMMWT,]
      }
      if(input$PCMOWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Outer Wall Thickness (in)`==input$PCMOWT,]
      }
      if(input$PCMTWT!="All"){
        data_PCM<-data_PCM[data_PCM$`Total Wall Thickness (in)`==input$PCMTWT,]
      }
      if(input$PCMTL!="All"){
        data_PCM<-data_PCM[data_PCM$`Total Length`==input$PCMTL,]
      }
      data_PCM
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
        data_OutputM<-single_tari_data[,input$show_vars4]
      }
      else {data_OutputM<-single_tari_data[single_tari_data$`Material Number`==input$PN,input$show_vars4]
      }
      
      if(input$MN!="All"){
        data_OutputM<-data_OutputM[data_OutputM$`Material Number`==input$MN,]
      }
      if(input$Batch!="All"){
        data_OutputM<-data_OutputM[data_OutputM$`SAP Batch Number`==input$Batch,]
      }
      if(input$SWR!="All"){
        data_OutputM<-data_OutputM[data_OutputM$`SWR Number`==input$SWR,]
      }
      if(input$OperatorID!="All"){
        data_OutputM<-data_OutputM[data_OutputM$`Operator ID`==input$OperatorID,]
      }
      if(input$Start[1]!=Time_Start || input$Start[2]!=Time_End){
        data_OutputM<-data_OutputM[(data_OutputM$`Start Date`<=input$Start[2] & data_OutputM$`Start Date`>=input$Start[1]),]
      }
      data_OutputM
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

