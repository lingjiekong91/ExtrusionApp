server <- function(input, output, session) {

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
                       choices = names(multi_pps_data),
                       selected=c("Part Number", "Part Description", 
                                  "Resin Number", "Resin Description",
                                  "Die Size (in)", "Tip Size (in)",
                                  "Inner Diameter (in)", "Outer Diameter (in)",
                                  "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                                  "Outer Wall Thickness (in)", "Total Wall Thickness (in)", "Total Length"
                       )
                       
    )
  })
  output$show_vars3_input<-renderUI({
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
  
  
  #Table Display for the Part Catalog--Single Extrusion PPS DATA 
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
  
  # Keep it for the test using
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
  
  output$OPMMN_input<-renderUI({
    selectInput("OPMMN","Material Number",c("All",unique(as.character(single_tari_data$`Material Number`))))
  })
  output$OPMBN_input<-renderUI({
    selectInput("OPMBN","SAP Batch Number",c("All",unique(as.character(single_tari_data$`SAP Batch Number`))))
    
  })
  output$OPMSN_input<-renderUI({
    selectInput("OPMSN","SWR Number",c("All",unique(as.character(single_tari_data$`SWR Number`))))
    
  })
  output$OPMOI_input<-renderUI({
    selectInput("OPMOI","Operator ID",c("All",unique(as.character(single_tari_data$`Operator ID`))))
    
  })
  output$OPMDR_input<-renderUI({
    dateRangeInput("OPMDR","Start Date Range",start=Time_Start,end=Time_End)
    
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable({
      if (input$PCSPN=="All"){
        data_OPM<-single_tari_data[,input$show_vars4]
      }
      else {data_OPM<-single_tari_data[single_tari_data$`Material Number`==input$PN,input$show_vars4]
      }
      
      if(input$OPMMN!="All"){
        data_OPM<-data_OPM[data_OPM$`Material Number`==input$OPMMN,]
      }
      if(input$OPMBN!="All"){
        data_OPM<-data_OPM[data_OPM$`SAP Batch Number`==input$OPMBN,]
      }
      if(input$OPMSN!="All"){
        data_OPM<-data_OPM[data_OPM$`SWR Number`==input$OPMSN,]
      }
      if(input$OPMOI!="All"){
        data_OPM<-data_OPM[data_OPM$`Operator ID`==input$OPMOI,]
      }
      if(input$OPMDR[1]!=Time_Start || input$OPMDR[2]!=Time_End){
        data_OPM<-data_OPM[(data_OPM$`Start Date`<=input$OPMDR[2] & data_OPM$`Start Date`>=input$OPMDR[1]),]
      }
      data_OPM
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
