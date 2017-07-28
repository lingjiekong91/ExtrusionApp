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
                                               column(5,uiOutput("PCSVC_input")),
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
                                      column(4,
                                             fluidRow(uiOutput("PCTPN_input")),
                                             fluidRow(uiOutput("PCTPD_input")),
                                             fluidRow(uiOutput("PCTRN_input")),
                                             fluidRow(uiOutput("PCTRD_input")),
                                             fluidRow(uiOutput("PCTPPSN_input"))
                                             ),
                                      column(4,
                                             fluidRow(
                                               column(6,uiOutput("PCTDS_min_input")),
                                               column(6,uiOutput("PCTDS_max_input"))
                                               ),
                                             fluidRow(uiOutput("PCTDLL_input")),
                                             fluidRow(uiOutput("PCTTS_input")),
                                             fluidRow(uiOutput("PCTTLL_input")),
                                             fluidRow(uiOutput("PCTSP_input"))
                                             ),
                                      column(4,
                                             fluidRow(uiOutput("PCTFT_input")),
                                             fluidRow(uiOutput("PCTBZT1_input")),
                                             fluidRow(uiOutput("PCTBZT2_input")),
                                             fluidRow(uiOutput("PCTBZT3_input")),
                                             fluidRow(uiOutput("PCTDT1_input")),
                                             fluidRow(uiOutput("PCTDT2_input"))
                                             )
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
