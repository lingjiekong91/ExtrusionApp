library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
library(gsubfn)
library(proto)
library(sqldf)


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
                                      column(3,uiOutput("PCTRD_input")),
                                      column(3,uiOutput("PCTPPSN_input")),
                                      #
                                      column(3,uiOutput("PCTDS_input")),
                                      column(3,uiOutput("PCTDLL_input")),
                                      column(3,uiOutput("PCTTS_input")),
                                      column(3,uiOutput("PCTTLL_input")),
                                      column(3,uiOutput("PCTSP_input")),
                                      #
                                      column(3,uiOutput("PCTFT_input")),
                                      column(3,uiOutput("PCTBZT1_input")),
                                      column(3,uiOutput("PCTBZT2_input")),
                                      column(3,uiOutput("PCTBZT3_input")),
                                      column(3,uiOutput("PCTDT1_input")),
                                      column(3,uiOutput("PCTDT2_input")),
                                      #
                                      column(3,uiOutput("PCTPID_input")),
                                      column(3,uiOutput("PCTPOD_input")),
                                      column(3,uiOutput("PCTPWT_input")),
                                      column(3,uiOutput("PCTPOR_input")),
                                      column(3,uiOutput("PCTPC_input")),
                                      #
                                      column(3,uiOutput("PCTDID_input")),
                                      column(3,uiOutput("PCTDOD_input")),
                                      column(3,uiOutput("PCTDWT_input")),
                                      column(3,uiOutput("PCTDOR_input")),
                                      column(3,uiOutput("PCTDC_input")),
                                      #
                                      column(3,uiOutput("PCTPL_input")),
                                      column(3,uiOutput("PCTTL_input")),
                                      column(3,uiOutput("PCTDL_input")),
                                      column(3,uiOutput("PCTTL_input")),
                                      column(3,uiOutput("PCTPPD_input")),
                                      #
                                      column(3,uiOutput("PCTNEXIV_input")),
                                      column(3,uiOutput("PCTAnnealed_input")),
                                      column(3,uiOutput("PCTCaliper_input")),
                                      column(3,uiOutput("PCTOS_input")),
                                      column(3,uiOutput("PCTMP_input")),
                                      #
                                      column(3,uiOutput("PCTHT_input")),
                                      column(3,uiOutput("PCTSPD_input")),
                                      column(3,uiOutput("PCTSLD_input")),
                                      column(3,uiOutput("PCTDLN_input")),
                                      column(3,uiOutput("PCTULT_input")),
                                      column(3,uiOutput("PCTVC_input")),
                                      column(3,uiOutput("PCTIRD_input"))
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
              )#end tabsetPanel for part catalog
  ) #end fluidPage
