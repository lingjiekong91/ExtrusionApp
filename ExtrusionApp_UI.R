library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
library(gsubfn)
library(proto)
library(sqldf)


ui<-fluidPage(
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
                           'input.dataset === "Tapered Extrusion PPS Data"',uiOutput("show_vars3_input"))
                         ),
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset',
                           tabPanel('Single Extrusion PPS Data',
                                    fluidRow(
                                      div(style="height:30px;",tags$h1(strong("Part Resin"),style="font-size:25px;",align="left")),
                                      column(2, offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Part Number", style="font-size:15px;")))),
                                      column(3,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Part Description", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Resin Number", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Resin Description", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("PPS Number", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCSPN_input")),
                                      column(3,offset = 0,style='padding:0px;',
                                             uiOutput("PCSPD_input")),
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCSRN_input")),
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCSRD_input")),
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCSPPSN_input"))
                                      ),
                                    fluidRow(
                                      tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die Size (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die Land Length (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Tip Size (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Tip Land Length (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Screw Print", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSDS_min_input")),
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSDS_max_input")),
                                      column(2,uiOutput("PCSDLL_input")),
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSTS_min_input")),
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSTS_max_input")),
                                      column(2,uiOutput("PCSTLL_input")),
                                      column(2,uiOutput("PCSSP_input"))
                                    ),
                                    fluidRow(
                                      tags$h1(strong("Attributes"),style="font-size:25px;",align="left"),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Feedthroat Temperature F", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Barrel Zone 1 Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Barrel Zone 2 Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Barrel Zone 3 Temperature", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSFT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSFT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSBZT1_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSBZT1_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSBZT2_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSBZT2_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSBZT3_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSBZT3_max_input"))
                                    ),
                                    fluidRow(
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Clamp Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Adapter Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die 1 Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die 2 Temperature", style="font-size:15px;"))))
                                    ),
                                    fluidRow(#Attributes_2
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSCT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSCT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSAT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSAT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSDT1_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSDT1_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSDT2_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSDT2_max_input"))
                                    ),
                                    fluidRow(
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Inner Diameter (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Out Diameter (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Wall Thickness (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Out of Roundness (in)", style="font-size:15px;"))))
                                    ), 
                                    fluidRow(#
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSIDI_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSIDI_max_input")),
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSODI_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSODI_max_input")),
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSWT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSWT_max_input")),
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSOR_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSOR_max_input"))
                                    ),
                                    fluidRow(
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Concentricity (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Length (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Perpendicularity (in)", style="font-size:15px;"))))
                                    ), 
                                    fluidRow(
                                      column(1,style='padding:0px;',
                                             uiOutput("PCSCCT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSCCT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCSLength_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCSLength_max_input")),
                                      
                                      column(2,style='padding:0px;',
                                             uiOutput("PCSPPD_input"))
                                    ),
                                    fluidRow(
                                      tags$h1(strong("Special"),style="font-size:25px;",align="left"),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("NEXIV", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Annealed", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Caliper", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("OD Sort", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Melt Pump", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Hypo Tip", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSNEXIV_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSAnnealed_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSCaliper_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSOS_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSMP_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSHT_input"))
                                    ),
                                    fluidRow(
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Spark Die", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Slicking Die", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Delamination", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Ultrasonic", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Vacuum Calibration", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Irradiated", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSSPD_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSSLD_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSDLN_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSULT_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSVC_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCSIRD_input"))
                                    ),
                                    
                                    
                                    fluidRow(
                                      DT::dataTableOutput("mytable1")
                                      )
                                    ),
                           
                           
                           tabPanel('Multi-Layered Extrusion PPS Data', 
                                    fluidRow(
                                      div(style="height:30px;",tags$h1(strong("Part Resin"),style="font-size:25px;",align="left")),
                                      column(2, offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Part Number", style="font-size:15px;")))),
                                      column(3,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Part Description", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Resin Number", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Resin Description", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("PPS Number", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCMPN_input")),
                                      column(3,offset = 0,style='padding:0px;',
                                             uiOutput("PCMPD_input")),
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCMRN_input")),
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCMRD_input")),
                                      column(2,offset = 0,style='padding:0px;',
                                             uiOutput("PCMPPSN_input"))
                                    ),
                                    fluidRow(
                                      tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die Size (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die Land Length (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Tip Size (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Tip Land Length (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Screw Print", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMDS_min_input")),
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMDS_max_input")),
                                      column(2,uiOutput("PCMDLL_input")),
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMTS_min_input")),
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMTS_max_input")),
                                      column(2,uiOutput("PCMTLL_input")),
                                      column(2,uiOutput("PCMSP_input"))
                                    ),
                                    fluidRow(
                                      tags$h1(strong("Attributes"),style="font-size:25px;",align="left"),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Feedthroat Temperature F", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Barrel Zone 1 Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Barrel Zone 2 Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Barrel Zone 3 Temperature", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMFT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMFT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMBZT1_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMBZT1_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMBZT2_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMBZT2_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMBZT3_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMBZT3_max_input"))
                                    ),
                                    fluidRow(
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Clamp Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Adapter Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die 1 Temperature", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Die 2 Temperature", style="font-size:15px;"))))
                                    ),
                                    fluidRow(#Attributes_2
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMCT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMCT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMAT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMAT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMDT1_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMDT1_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMDT2_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMDT2_max_input"))
                                    ),
                                    fluidRow(
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Inner Diameter (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Out Diameter (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Inner Wall Thickness (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Middle Wall Thickness (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Outer Wall Thickness (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Total Wall Thickness (in)", style="font-size:15px;"))))
                                    ), 
                                    fluidRow(#
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMIDI_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMIDI_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMODI_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMODI_max_input")),
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMIWT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMIWT_max_input")),
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMMWT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMMWT_max_input")),
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMOWT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMOWT_max_input")),
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMTWT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMTWT_max_input"))
                                      

                                    ),
                                    fluidRow(
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Out of Roundness (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Concentricity (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Length (in)", style="font-size:15px;")))),
                                      column(2,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Perpendicularity (in)", style="font-size:15px;"))))
                                    ), 
                                    fluidRow(
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMOR_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMOR_max_input")),
                                      column(1,style='padding:0px;',
                                             uiOutput("PCMCCT_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMCCT_max_input")),
                                      
                                      column(1,style='padding:0px 0px 0px 5px ;',
                                             uiOutput("PCMLength_min_input")),
                                      column(1,style='padding:0px 5px 0px 0px;',
                                             uiOutput("PCMLength_max_input")),
                                      
                                      column(2,style='padding:0px;',
                                             uiOutput("PCMPPD_input"))
                                    ),
                                    fluidRow(
                                      tags$h1(strong("Special"),style="font-size:25px;",align="left"),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("NEXIV", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Annealed", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Caliper", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("OD Sort", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Melt Pump", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Hypo Tip", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMNEXIV_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMAnnealed_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMCaliper_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMOS_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMMP_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMHT_input"))
                                    ),
                                    fluidRow(
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Spark Die", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Slicking Die", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Delamination", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Ultrasonic", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Vacuum Calibration", style="font-size:15px;")))),
                                      column(1,offset = 0,
                                             div(style = "height:35px;",
                                                 tags$h2(strong("Irradiated", style="font-size:15px;"))))
                                    ),
                                    fluidRow(
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMSPD_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMSLD_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMDLN_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMULT_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMVC_input")),
                                      column(1,offset = 0,style='padding:0px 10px;',
                                             uiOutput("PCMIRD_input"))
                                    ),
                                    fluidRow(
                                      DT::dataTableOutput('mytable2')
                                      )
                                    ), #end Multi-Layered Extrusion PPS Data
                           tabPanel('Tapered Extrusion PPS Data',
                                    fluidRow(
                                      column(2,uiOutput("PCTPN_input")),
                                      column(2,uiOutput("PCTPD_input")),
                                      column(2,uiOutput("PCTRN_input")),
                                      column(2,uiOutput("PCTRD_input")),
                                      column(2,uiOutput("PCTPPSN_input"))
                                             ),
                                    fluidRow(
                                      column(width = 2, offset = 0,
                                             div(style = "height:25px;",
                                                 tags$h3(strong("Die Size (in)", style="font-size:15px;"))
                                                 )
                                             ),
                                      column(width = 2, offset = 0,
                                             div(style = "height:25px;",
                                                 tags$h3(strong("Die Land Length (in)", style="font-size:15px;"))
                                             )
                                      ),
                                      column(width = 2, offset = 0,
                                             div(style = "height:25px;",
                                                 tags$h3(strong("Tip Size (in)", style="font-size:15px;"))
                                             )
                                      ),
                                      column(width = 2, offset = 0,
                                             div(style = "height:25px;",
                                                 tags$h3(strong("Tip Land Length (in)", style="font-size:15px;"))
                                             )
                                      ),
                                      column(width = 2, offset = 0,
                                             div(style = "height:25px;",
                                                 tags$h3(strong("Screw Print", style="font-size:15px;"))
                                             )
                                      )
                                      
                                    ),
                                    fluidRow(
                                      column(1,offset = 0,style='padding:0px;',
                                             uiOutput("PCTDS_min_input")),
                                      column(1,offset = 0,style='padding:0px;',
                                             uiOutput("PCTDS_max_input")),
                                      column(2,uiOutput("PCTDLL_input")),
                                      column(2,uiOutput("PCTTS_input")),
                                      column(2,uiOutput("PCTTLL_input")),
                                      column(2,uiOutput("PCTSP_input"))
                                      ),
                                    fluidRow(
                                      column(2,uiOutput("PCTFT_input")),
                                      column(2,uiOutput("PCTBZT1_input")),
                                      column(2,uiOutput("PCTBZT2_input")),
                                      column(2,uiOutput("PCTBZT3_input")),
                                      column(2,uiOutput("PCTDT1_input")),
                                      column(2,uiOutput("PCTDT2_input"))
                                      ),
                                    fluidRow(
                                      column(2,uiOutput("PCTPID_input")),
                                      column(2,uiOutput("PCTPOD_input")),
                                      column(2,uiOutput("PCTPWT_input")),
                                      column(2,uiOutput("PCTPOR_input")),
                                      column(2,uiOutput("PCTPC_input"))
                                      ),
                                    fluidRow(
                                       column(2,uiOutput("PCTDID_input")),
                                       column(2,uiOutput("PCTDOD_input")),
                                       column(2,uiOutput("PCTDWT_input")),
                                       column(2,uiOutput("PCTDOR_input")),
                                       column(2,uiOutput("PCTDC_input"))
                                       ),
                                    fluidRow(
                                      DT::dataTableOutput('mytable3')
                                      )
                                    )
                           #end Tapered Extrusion PPS Data
                           )#end tabsetPanel
                         )#end mainPanel
                       ),#end tabPanel
              
              tabPanel('Output',
                       sidebarPanel(
                         conditionalPanel(
                           'input.output_dataset === "MES Data"',uiOutput("show_vars4_input")
                           )
                         ),#end sidebarPanel
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'output_dataset',
                           tabPanel('MES Data', 
                                    fluidRow(
                                      column(2,uiOutput("OPMMN_input")),
                                      column(2,uiOutput("OPMBN_input")),
                                      column(2,uiOutput("OPMSN_input")),
                                      column(2,uiOutput("OPMOI_input")),
                                      column(3,uiOutput("OPMDR_input"))
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
                           'input.extra_dataset === "Resin Data"',uiOutput("show_vars5_input")
                           ),
                         conditionalPanel(
                           'input.extra_dataset === "Screw Data"',uiOutput("show_vars6_input")
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
              ) #end for every tab
  ) #end fluidPage
