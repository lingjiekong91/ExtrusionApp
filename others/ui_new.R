library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
library(gsubfn)
library(proto)
library(sqldf)
#_s:name of the checkbox
#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

ui<-navbarPage("Extrusion Application",
               
               navbarMenu("Part Catalog",
                          #Single Extrusion PPS Data
                          tabPanel("Single Extrusion PPS Data",
                                   #Part Resin
                                   fluidRow(
                                     tags$h1(strong("Part Resin"),style="font-size:25px;",align="left"),
                                     #Part Number
                                     column(2,
                                             fluidRow(uiOutput("PCSPN_s")),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition="input.PCSPN_d",
                                                 uiOutput("PCSPN_input")
                                                 ))),
                                     # Part Description
                                     column(2,
                                             fluidRow(uiOutput("PCSPD_s")),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSPD_d",
                                                 uiOutput("PCSPD_input")
                                                 ))), 
                                      # Resin Number
                                      column(2,
                                             fluidRow(uiOutput("PCSRN_s")),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSRN_d",
                                                 uiOutput("PCSRN_input")
                                               ))),
                                      #Resin Description
                                      column(2,
                                             fluidRow(uiOutput("PCSRD_s")),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSRD_d",
                                                 uiOutput("PCSRD_input")
                                               ))),
                                      #PPS Number
                                      column(2,
                                             fluidRow(uiOutput("PCSPPSN_s")),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSPPSN_d",
                                                 uiOutput("PCSPPSN_input")
                                               )))
                                     ),
                                   #Tooling
                                   fluidRow(
                                     tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                     #Die Size
                                     column(2,
                                            fluidRow(uiOutput("PCSDS_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSDS_d",
                                                       uiOutput("PCSDS_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSDS_d",
                                                       uiOutput("PCSDS_max_input")
                                                     )))),
                                     #Die Land Length
                                     column(2,
                                            fluidRow(uiOutput("PCSDLL_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSDLL_d",
                                                uiOutput("PCSDLL_input")
                                              ))),
                                     #Tip Size
                                     column(2,
                                            fluidRow(uiOutput("PCSTS_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSTS_d",
                                                       uiOutput("PCSTS_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSTS_d",
                                                       uiOutput("PCSTS_max_input")
                                                     )
                                              ))),
                                     #Tip Land Length
                                     column(2,
                                            fluidRow(uiOutput("PCSTLL_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSTLL_d",
                                                uiOutput("PCSTLL_input")
                                              ))),
                                     #Screw Print
                                     column(2,
                                            fluidRow(uiOutput("PCSSP_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSSP_d",
                                                uiOutput("PCSSP_input")
                                              )))
                                   ),#end Tooling
                                   #Processing Attributes
                                   fluidRow(
                                     tags$h1(strong("Processing Attribute"),style="font-size:25px;",align="left"),
                                     
                                     #Feedthroat
                                     column(3,
                                            fluidRow(uiOutput("PCSFT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSFT_d",
                                                       uiOutput("PCSFT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSFT_d",
                                                       uiOutput("PCSFT_max_input")
                                                     )
                                              ))),
                                     #Barrel Zone 1
                                     column(3,
                                            fluidRow(uiOutput("PCSBZT1_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSBZT1_d",
                                                       uiOutput("PCSBZT1_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSBZT1_d",
                                                       uiOutput("PCSBZT1_max_input")
                                                     )
                                              ))),
                                     #Barrel ZOne2
                                     column(3,
                                            fluidRow(uiOutput("PCSBZT2_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSBZT2_d",
                                                       uiOutput("PCSBZT2_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSBZT2_d",
                                                       uiOutput("PCSBZT2_max_input")
                                                     )
                                              ))),
                                     #Barrel Zone3
                                     column(3,
                                            fluidRow(uiOutput("PCSBZT3_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSBZT3_d",
                                                       uiOutput("PCSBZT3_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSBZT3_d",
                                                       uiOutput("PCSBZT3_max_input")
                                                     )
                                              )))
                                   ),#end Processing Attribute 1
                                   fluidRow(
                                     column(3,
                                            fluidRow(uiOutput("PCSCT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSCT_d",
                                                       uiOutput("PCSCT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSCT_d",
                                                       uiOutput("PCSCT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSAT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSAT_d",
                                                       uiOutput("PCSAT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSAT_d",
                                                       uiOutput("PCSAT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSDT1_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSDT1_d",
                                                       uiOutput("PCSDT1_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSDT1_d",
                                                       uiOutput("PCSDT1_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSDT2_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSDT2_d",
                                                       uiOutput("PCSDT2_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSDT2_d",
                                                       uiOutput("PCSDT2_max_input")
                                                     )
                                              )))), #end Processing Attribute 2
                                   #Dimentional Attribute
                                   fluidRow(
                                     tags$h1(strong("Dimentional Attribute"),style="font-size:25px;",align="left"),
                                     column(3,
                                            fluidRow(uiOutput("PCSIDI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSIDI_d",
                                                       uiOutput("PCSIDI_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSIDI_d",
                                                       uiOutput("PCSIDI_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSODI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSODI_d",
                                                       uiOutput("PCSODI_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSODI_d",
                                                       uiOutput("PCSODI_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSWT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSWT_d",
                                                       uiOutput("PCSWT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSWT_d",
                                                       uiOutput("PCSWT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSOR_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSOR_d",
                                                       uiOutput("PCSOR_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSOR_d",
                                                       uiOutput("PCSOR_max_input")
                                                     )
                                              )))
                                   ), #end Dimentional Attribute 1
                                   fluidRow(
                                     column(3,
                                            fluidRow(uiOutput("PCSCCT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSCCT_d",
                                                       uiOutput("PCSCCT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSCCT_d",
                                                       uiOutput("PCSCCT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSLength_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSLength_d",
                                                       uiOutput("PCSLength_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCSLength_d",
                                                       uiOutput("PCSLength_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCSPPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSPPD_d",
                                                uiOutput("PCSPPD_input")
                                              )))
                                     
                                   ),
                                   #Special Operation
                                   fluidRow(
                                     tags$h1(strong("Special Operation"),style="font-size:25px;",align="left"),
                                     column(1,
                                            fluidRow(uiOutput("PCSNEXIV_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSNEXIV_d",
                                                uiOutput("PCSNEXIV_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSAnnealed_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSAnnealed_d",
                                                uiOutput("PCSAnnealed_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSCaliper_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSCaliper_d",
                                                uiOutput("PCSCaliper_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSOS_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSOS_d",
                                                uiOutput("PCSOS_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSMP_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSMP_d",
                                                uiOutput("PCSMP_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSHT_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSHT_d",
                                                uiOutput("PCSHT_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSSPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSSPD_d",
                                                uiOutput("PCSSPD_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSSLD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSSLD_d",
                                                uiOutput("PCSSLD_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSDLN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSDLN_d",
                                                uiOutput("PCSDLN_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSULT_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSULT_d",
                                                uiOutput("PCSULT_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSVC_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSVC_d",
                                                uiOutput("PCSVC_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCSIRD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSIRD_d",
                                                uiOutput("PCSIRD_input")
                                              )))
                                   ), #end Special Operation
                                   
                                   
                                   
                                   fluidRow(
                                     DT::dataTableOutput("mytable1")
                                     )
                                   )#end Single Extrusion PPS Data
                          
                          
                          )#end Part Catalog
               )#end ui


