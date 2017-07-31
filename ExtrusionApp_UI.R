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
                                    fluidRow(tags$h1(strong("Part Resin"),align="left"),
                                      column(2,uiOutput("PCSPN_input")),
                                      column(3,uiOutput("PCSPD_input")),
                                      column(2,uiOutput("PCSRN_input")),
                                      column(2,uiOutput("PCSRD_input")),
                                      column(2,uiOutput("PCSPPSN_input"))
                                      ),
                                    fluidRow(tags$h1(strong("Tooling"),align="left"),
                                      column(2,uiOutput("PCSDS_input")),
                                      column(2,uiOutput("PCSDLL_input")),
                                      column(2,uiOutput("PCSTS_input")),
                                      column(2,uiOutput("PCSTLL_input")),
                                      column(2,uiOutput("PCSSP_input"))
                                      ),
                                    fluidRow(tags$h1(strong("Attributes"),align="left"),
                                      column(3,uiOutput("PCSFT_input")),
                                      column(3,uiOutput("PCSBZT1_input")),
                                      column(3,uiOutput("PCSBZT2_input")),
                                      column(3,uiOutput("PCSBZT3_input"))
                                      ),
                                    fluidRow(#Attributes_2
                                      column(3,uiOutput("PCSCT_input")),
                                      column(3,uiOutput("PCSAT_input")),
                                      column(3,uiOutput("PCSDT1_input")),
                                      column(3,uiOutput("PCSDT2_input"))
                                      ),
                                    fluidRow(#
                                      column(3,uiOutput("PCSIDI_input")),
                                      column(3,uiOutput("PCSODI_input")),
                                      column(3,uiOutput("PCSWT_input")),
                                      column(3,uiOutput("PCSOR_input"))
                                      ),
                                    fluidRow(
                                      column(3,uiOutput("PCSCCT_input")),
                                      column(3,uiOutput("PCSLength_input")),
                                      column(3,uiOutput("PCSPPD_input"))
                                      ),
                                    
                                    fluidRow(tags$h1(strong("Special"),align="left"),
                                      column(2,uiOutput("PCSNEXIV_input")),
                                      column(2,uiOutput("PCSAnnealed_input")),
                                      column(2,uiOutput("PCSCaliper_input")),
                                      column(2,uiOutput("PCSOS_input")),
                                      column(2,uiOutput("PCSMP_input")),
                                      column(2,uiOutput("PCSHT_input"))
                                      ),
                                    fluidRow(
                                      column(2,uiOutput("PCSSPD_input")),
                                      column(2,uiOutput("PCSSLD_input")),
                                      column(2,uiOutput("PCSDLN_input")),
                                      column(2,uiOutput("PCSULT_input")),
                                      column(2,uiOutput("PCSVC_input")),
                                      column(2,uiOutput("PCSIRD_input"))
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
                                      column(3,uiOutput("PCMFT_input")),
                                      column(3,uiOutput("PCMBZT1_input")),
                                      column(3,uiOutput("PCMBZT2_input")),
                                      column(3,uiOutput("PCMBZT3_input")),
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
                                      column(2,uiOutput("PCTPN_input")),
                                      column(2,uiOutput("PCTPD_input")),
                                      column(2,uiOutput("PCTRN_input")),
                                      column(2,uiOutput("PCTRD_input")),
                                      column(2,uiOutput("PCTPPSN_input"))
                                             ),
                                    fluidRow(
                                      column(1,uiOutput("PCTDS_min_input")),
                                      column(1,uiOutput("PCTDS_max_input")),
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
